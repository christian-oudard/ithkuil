package surface

import (
	"strings"
	"unicode/utf8"
)

// Conjunct splitting (Layer B-ish). A word is split into alternating
// runs of vowel and non-vowel characters; the inverse is just
// strings.Join on the slice. Stress marks aren't stripped here —
// callers usually strip first (via Strip) or apply normalization
// downstream.

// vowelChars is the set of vowel runes recognized by the Ithkuil
// orthography: plain, umlauted, accented, and hiatus-marked forms.
var vowelChars = map[rune]bool{
	'a': true, 'ä': true,
	'e': true, 'ë': true,
	'i': true, 'ï': true,
	'o': true, 'ö': true,
	'u': true, 'ü': true,
	'á': true, 'é': true, 'í': true, 'ó': true, 'ú': true,
	'à': true, 'è': true, 'ì': true, 'ò': true, 'ù': true,
	'â': true, 'ê': true, 'î': true, 'ô': true, 'û': true,
	'ǎ': true, 'ě': true, 'ǐ': true, 'ǒ': true, 'ǔ': true,
}

// IsVowel reports whether r is a vowel character.
func IsVowel(r rune) bool { return vowelChars[r] }

// SplitConjuncts segments a word into alternating runs of consonant
// and vowel characters. Empty input yields an empty slice.
//
// Examples:
//
//	"malëuţřait" → ["m", "a", "l", "ëu", "ţř", "ai", "t"]
//	"ţřai"       → ["ţř", "ai"]
//	"emal"       → ["e", "m", "a", "l"]
//
// Inverse: JoinConjuncts.
func SplitConjuncts(s string) []string {
	if s == "" {
		return nil
	}
	var out []string
	var run []rune
	prev := -1 // -1 sentinel: no run yet
	for _, r := range s {
		c := 0
		if IsVowel(r) {
			c = 1
		}
		if c != prev && prev != -1 {
			out = append(out, string(run))
			run = run[:0]
		}
		run = append(run, r)
		prev = c
	}
	if len(run) > 0 {
		out = append(out, string(run))
	}
	return out
}

// JoinConjuncts is the inverse of SplitConjuncts. It's just
// strings.Join on the empty separator; provided here as a paired
// name to make round-trip code read symmetrically.
func JoinConjuncts(parts []string) string {
	return strings.Join(parts, "")
}

// IsVowelConjunct reports whether a conjunct begins with a vowel.
// Returns false for the empty string.
func IsVowelConjunct(s string) bool {
	if s == "" {
		return false
	}
	r, _ := utf8.DecodeRuneInString(s)
	return IsVowel(r)
}

// IsConsonantConjunct reports whether a conjunct begins with a
// non-vowel. Returns false for the empty string.
func IsConsonantConjunct(s string) bool {
	if s == "" {
		return false
	}
	r, _ := utf8.DecodeRuneInString(s)
	return !IsVowel(r)
}
