// Package surface is Layer A of the parse/render stack: pure
// transformations between raw surface text and a (normalized text +
// stress position) pair. It has no grammatical knowledge — it works
// at the level of letters, accents, and syllable count only.
//
// The two public functions form an inverse pair on well-formed input:
//
//	Apply(Strip(w)) == w
//	Strip(Apply(n, s)) == (n, s)
//
// "Well-formed" here means input that follows the spec's convention
// that penultimate and monosyllabic stress are orthographically
// unmarked (§3.10). Input that explicitly marks penultimate stress
// (a stray acute on the penultimate vowel) will round-trip as the
// unmarked form; that's a correction of invalid input rather than a
// faithful preservation.
package surface

import (
	"strings"
	"unicode/utf8"
)

// Stress is the orthographic stress position of a word. Equivalent to
// parse.Stress; lives here because layer A is where the bytes of the
// stress mark are interpreted. Higher layers map this to grammar
// categories (UnframedVerbal, FramedVerbal, etc.).
type Stress int

const (
	Monosyllabic Stress = iota
	Penultimate
	Ultimate
	Antepenultimate
	// InvalidStress signals the surface form carried more than one
	// stress diacritic, so no single stress position can be derived.
	// Returned by Strip; callers that need an unambiguous reading
	// should reject the input.
	InvalidStress
)

func (s Stress) String() string {
	return [...]string{"Monosyllabic", "Penultimate", "Ultimate", "Antepenultimate", "InvalidStress"}[s]
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

// Strip removes any stress diacritic from word and returns the bare
// (normalized) text plus the detected Stress position.
//
// Detection rules:
//   - Acute/circumflex on the final vowel-conjunct → Ultimate.
//   - Acute/circumflex on the third-from-last vowel-conjunct or
//     earlier → Antepenultimate.
//   - Acute/circumflex on the penultimate vowel-conjunct → Penultimate
//     (the acute is informational; structural position is what counts).
//   - No mark, ≤ 1 vowel-conjunct → Monosyllabic.
//   - No mark, more vowel-conjuncts → Penultimate (the unmarked default).
func Strip(word string) (string, Stress) {
	vowelIdx := vowelConjunctIndices(word)
	conjs := SplitConjuncts(word)

	// Find each stress-marked syllable.
	stressedSyllable := -1
	markCount := 0
	for i, vi := range vowelIdx {
		for _, r := range conjs[vi] {
			if isStressMark(r) {
				if stressedSyllable < 0 {
					stressedSyllable = i
				}
				markCount++
				break
			}
		}
	}

	// Derive Stress from position. >1 stress mark is a malformed
	// surface form — the spec marks at most one syllable per word
	// (§1.3.1) — so report it instead of silently picking one.
	n := len(vowelIdx)
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

	// Strip every stress mark from the surface text.
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
// vowel-conjunct; Antepenultimate marks the third-from-last. Words
// with too few vowel-conjuncts to satisfy the requested position are
// returned unchanged.
//
// Within a multi-vowel conjunct (diphthong or disyllabic pair) the
// mark goes on the first vowel — the prominent member of a falling
// diphthong, and the first syllable of a disyllabic pair.
func Apply(word string, stress Stress) string {
	if stress == Penultimate || stress == Monosyllabic {
		return word
	}
	vowelIdx := vowelConjunctIndices(word)
	conjs := SplitConjuncts(word)
	n := len(vowelIdx)
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
	conjs[vowelIdx[target]] = markFirstVowel(conjs[vowelIdx[target]])
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
