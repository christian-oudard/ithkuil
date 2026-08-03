package phonology

import (
	"strings"
	"unicode/utf8"
)

// Stress is the orthographic stress position of a word. Equivalent to
// parse.Stress; lives here because this is where the bytes of the
// stress mark are interpreted. Higher layers map it to grammar
// categories (UnframedVerbal, FramedVerbal, etc.).
//
// Strip and Apply form an inverse pair on well-formed input:
//
//	Apply(Strip(w)) == w
//	Strip(Apply(n, s)) == (n, s)
//
// "Well-formed" means input that follows the spec's convention that
// penultimate and monosyllabic stress are orthographically unmarked
// (§3.10). Input that explicitly marks penultimate stress (a stray
// acute on the penultimate vowel) round-trips as the unmarked form;
// that is a correction of invalid input rather than a faithful
// preservation.
type Stress int

const (
	Monosyllabic Stress = iota
	Penultimate
	Ultimate
	Antepenultimate
	// InvalidStress signals the romanization carried more than one
	// stress diacritic, so no single stress position can be derived.
	// Returned by Strip; callers that need an unambiguous reading
	// should reject the input.
	InvalidStress
)

func (s Stress) String() string {
	return enumName(s, "Stress",
		"Monosyllabic", "Penultimate", "Ultimate", "Antepenultimate", "InvalidStress")
}

// stripMap drops a stress diacritic but preserves the umlaut layer.
// Acute on a plain vowel returns the plain vowel; circumflex on an
// umlauted vowel returns the umlauted vowel.
var stripMap = map[rune]rune{
	'á': 'a', 'é': 'e', 'í': 'i', 'ó': 'o', 'ú': 'u',
	'â': 'ä', 'ê': 'ë', 'ô': 'ö', 'û': 'ü',
}

// applyMap is the inverse: plain → stressed.
var applyMap = map[rune]rune{
	'a': 'á', 'e': 'é', 'i': 'í', 'o': 'ó', 'u': 'ú',
	'ä': 'â', 'ë': 'ê', 'ö': 'ô', 'ü': 'û',
}

// isStressMark reports whether r is an acute or circumflex form.
func isStressMark(r rune) bool {
	_, ok := stripMap[r]
	return ok
}

// diphthongs is the closed set of ten falling diphthongs from §1.2.1.
// They are the only vowel pairs that share a syllable; every other
// two-vowel conjunct (the Series 3 and 4 forms — ia, uo, ao, oë …) is
// disyllabic, which is why §1.3.1 needs the grave accent to mark the
// unstressed -i- of -Cìa-.
//
// Taken as closed against §4.6.3, which calls üo a "word-initial
// diphthong" outright. The morphology's own vowel-form tables use ae,
// ea, üo and üö as form-0 values and never say how many syllables any
// of the four carries, so §1.2.1's list is the only statement on the
// question and this follows it. It is not a free choice: syllable count
// is what selects the formative's Relation in Slot X.
var diphthongs = map[string]bool{
	"ai": true, "ei": true, "ëi": true, "oi": true, "ui": true,
	"au": true, "eu": true, "ëu": true, "ou": true, "iu": true,
}

// splitSyllables divides a vowel conjunct into its syllable nuclei,
// taking a diphthong as one nucleus and any other vowel as its own.
// Stress marks are ignored for the lookup, so "íhi" and "ihi" split
// the same way.
func splitSyllables(conj string) []string {
	rs := []rune(conj)
	var out []string
	for i := 0; i < len(rs); {
		if i+1 < len(rs) {
			pair := string([]rune{normalize(rs[i]), normalize(rs[i+1])})
			if diphthongs[pair] {
				out = append(out, string(rs[i:i+2]))
				i += 2
				continue
			}
		}
		out = append(out, string(rs[i]))
		i++
	}
	return out
}

// normalize strips a stress diacritic from a single vowel rune.
func normalize(r rune) rune {
	if plain, ok := stripMap[r]; ok {
		return plain
	}
	return r
}

// syllable locates one syllable nucleus: which conjunct it sits in and
// its byte offset within that conjunct.
type syllable struct {
	conj   int
	offset int
	text   string
}

// syllables lists every syllable nucleus in word, in order.
func syllables(word string) []syllable {
	conjs := SplitConjuncts(word)
	var out []syllable
	for ci, c := range conjs {
		if c == "" {
			continue
		}
		r, _ := utf8.DecodeRuneInString(c)
		if !IsVowel(r) {
			continue
		}
		off := 0
		for _, s := range splitSyllables(c) {
			out = append(out, syllable{conj: ci, offset: off, text: s})
			off += len(s)
		}
	}
	return out
}

// SyllableCount returns the number of syllables in word.
func SyllableCount(word string) int { return len(syllables(word)) }

// StressPosition reports how many syllables word has, the 0-based
// index of the first one carrying a stress diacritic (-1 when none
// does), and how many diacritics the word carries in all.
func StressPosition(word string) (n, idx, marks int) {
	syls := syllables(word)
	idx = -1
	for i, s := range syls {
		for _, r := range s.text {
			if isStressMark(r) {
				if idx < 0 {
					idx = i
				}
				marks++
				break
			}
		}
	}
	return len(syls), idx, marks
}

// Strip removes any stress diacritic from word and returns the bare
// (normalized) text plus the detected Stress position.
//
// Detection rules:
//   - Acute/circumflex on the final syllable → Ultimate.
//   - Acute/circumflex on the third-from-last syllable or earlier →
//     Antepenultimate.
//   - Acute/circumflex on the penultimate syllable → Penultimate
//     (the acute is informational; structural position is what counts).
//   - No mark, ≤ 1 syllable → Monosyllabic.
//   - No mark, more syllables → Penultimate (the unmarked default).
//
// Syllables, not vowel-conjuncts: a conjunct like -ia- or -oa- is two
// of them, so wuttíhia is wu-ttí-hi-a and its mark is antepenultimate,
// which is what makes it the FRAMED verbal the §6.2.2 gloss calls it.
func Strip(word string) (string, Stress) {
	n, stressedSyllable, markCount := StressPosition(word)

	// Derive Stress from position. >1 stress mark is a malformed
	// romanization — the spec marks at most one syllable per word
	// (§1.3.1) — so report it instead of silently picking one.
	var stress Stress
	switch {
	case markCount > 1:
		stress = InvalidStress
	case stressedSyllable < 0 && n <= 1:
		stress = Monosyllabic
	case stressedSyllable < 0:
		stress = Penultimate
	case stressedSyllable == n-1:
		stress = Ultimate
	case stressedSyllable == n-2:
		stress = Penultimate
	default:
		stress = Antepenultimate
	}

	// Strip every stress mark from the romanization.
	var b strings.Builder
	b.Grow(len(word))
	for _, r := range word {
		if rep, ok := stripMap[r]; ok {
			b.WriteRune(rep)
		} else {
			b.WriteRune(r)
		}
	}
	return b.String(), stress
}

// Apply places a stress diacritic on the normalized word according to
// the requested Stress. Penultimate and Monosyllabic are unmarked per
// §3.10; the input is returned unchanged. Ultimate marks the last
// syllable; Antepenultimate marks the third-from-last. Words with too
// few syllables to satisfy the requested position are returned
// unchanged.
//
// A falling diphthong takes its mark on the prominent first member.
// A disyllabic conjunct is two syllables, so the mark lands on
// whichever of its vowels the count selects.
func Apply(word string, stress Stress) string {
	if stress == Penultimate || stress == Monosyllabic {
		return word
	}
	syls := syllables(word)
	conjs := SplitConjuncts(word)
	n := len(syls)
	var target int
	switch stress {
	case Ultimate:
		if n < 2 {
			return word // monosyllabic ultimate is unmarked
		}
		target = n - 1
	case Antepenultimate:
		if n < 3 {
			return word
		}
		target = n - 3
	default:
		return word
	}
	s := syls[target]
	c := conjs[s.conj]
	conjs[s.conj] = c[:s.offset] + markFirstVowel(c[s.offset:])
	return strings.Join(conjs, "")
}

// vowelConjunctIndices returns the positions, within
// SplitConjuncts(word), of conjuncts that begin with a vowel
// rune. Each entry corresponds to one syllable.
func vowelConjunctIndices(word string) []int {
	conjs := SplitConjuncts(word)
	var out []int
	for i, c := range conjs {
		if c == "" {
			continue
		}
		r, _ := utf8.DecodeRuneInString(c)
		if IsVowel(r) {
			out = append(out, i)
		}
	}
	return out
}

// markFirstVowel applies the stress diacritic to the first markable
// vowel in s. Non-vowel runes and already-stressed vowels pass
// through unchanged.
func markFirstVowel(s string) string {
	var b strings.Builder
	marked := false
	for _, r := range s {
		if !marked {
			if rep, ok := applyMap[r]; ok {
				b.WriteRune(rep)
				marked = true
				continue
			}
		}
		b.WriteRune(r)
	}
	return b.String()
}
