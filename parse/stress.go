package parse

import (
	"unicode/utf8"
)

// Stress is an orthographic observation about a surface word: where the
// acute/circumflex diacritic falls (or its absence). It exists in the
// parse layer because it's a property of the written form, not of the
// grammatical structure — the grammatical category lives in
// grammar.Final, which the parser builds from the observed Stress
// together with Slot IX content.
type Stress int

const (
	Monosyllabic Stress = iota
	Penultimate
	Ultimate
	Antepenultimate
)

func (s Stress) String() string {
	return [...]string{"Monosyllabic", "Penultimate", "Ultimate", "Antepenultimate"}[s]
}

// stressedVowels are the acute (á é í ó ú) and circumflex (â ê ô û)
// forms — these mark the stressed syllable. The set is asymmetric
// because circumflex doubles as the umlaut form and "î" isn't used:
// "i" pairs with the diaeresis hiatus marker "ï" instead.
var stressedVowels = map[rune]bool{
	'á': true, 'é': true, 'í': true, 'ó': true, 'ú': true,
	'â': true, 'ê': true, 'ô': true, 'û': true,
}

// IsStressedVowel reports whether a rune carries a stress mark.
func IsStressedVowel(r rune) bool { return stressedVowels[r] }

// containsStress reports whether s contains any stressed vowel.
func containsStress(s string) bool {
	for _, r := range s {
		if IsStressedVowel(r) {
			return true
		}
	}
	return false
}

// DetectStress determines the stress pattern of a word from its vowel
// markers. Rules:
//   - No stress marks, 1 or fewer syllables → Monosyllabic
//   - No stress marks, more syllables → Penultimate (default)
//   - Stress on the last syllable → Ultimate
//   - Stress on the penultimate syllable → Penultimate
//   - Stress earlier than that → Antepenultimate
//
// Syllables are the vowel conjuncts of the word as segmented by
// SplitConjuncts.
//
// Deprecated: New code should call surface.Strip directly, which
// returns the bare text alongside the stress position. This shim
// remains for callers that only want the stress.
func DetectStress(word string) Stress {
	conjs := SplitConjuncts(word)
	var syllables []string
	for _, c := range conjs {
		if c == "" {
			continue
		}
		r, _ := utf8.DecodeRuneInString(c)
		if IsVowelChar(r) {
			syllables = append(syllables, c)
		}
	}
	n := len(syllables)
	wordHasStress := containsStress(word)
	if n <= 1 && !wordHasStress {
		return Monosyllabic
	}
	if !wordHasStress {
		return Penultimate
	}
	// Find the 1-based position of the first stressed syllable.
	stressPos := 1
	for _, s := range syllables {
		if containsStress(s) {
			break
		}
		stressPos++
	}
	switch {
	case stressPos == n:
		return Ultimate
	case stressPos <= n-2:
		return Antepenultimate
	default:
		return Penultimate
	}
}
