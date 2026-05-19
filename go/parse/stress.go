package parse

import (
	"unicode/utf8"

	"github.com/coudard/ithkuil/go/grammar"
)

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
func DetectStress(word string) grammar.Stress {
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
		return grammar.Monosyllabic
	}
	if !wordHasStress {
		return grammar.Penultimate
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
		return grammar.Ultimate
	case stressPos <= n-2:
		return grammar.Antepenultimate
	default:
		return grammar.Penultimate
	}
}
