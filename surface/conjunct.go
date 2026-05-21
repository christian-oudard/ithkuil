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

// vowelChars is the set of vowel runes recognized by the Ithkuil V4
// orthography: 9 base vowels plus the 9 stressed forms (acute and
// circumflex). "i" has no umlaut so it has no circumflex form.
var vowelChars = map[rune]bool{
	'a': true, 'ä': true,
	'e': true, 'ë': true,
	'i': true,
	'o': true, 'ö': true,
	'u': true, 'ü': true,
	'á': true, 'é': true, 'í': true, 'ó': true, 'ú': true,
	'â': true, 'ê': true, 'ô': true, 'û': true,
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

// MergeGlottalVowels collapses V-'-V triples back into a single
// conjunct of the form "V'V". SplitConjuncts treats the glottal
// stop as a non-vowel (it isn't in the vowel set), which would
// split glottalized case vowels like "i'a" into three conjuncts.
// Callers that consume the conjunct list as slot-level chunks
// need them re-merged so case lookup (LOC = "i'a") works.
//
// Leading or trailing glottals without a vowel on both sides are
// left alone.
func MergeGlottalVowels(conjs []string) []string {
	out := make([]string, 0, len(conjs))
	i := 0
	for i < len(conjs) {
		if i+2 < len(conjs) &&
			IsVowelConjunct(conjs[i]) &&
			conjs[i+1] == "'" &&
			IsVowelConjunct(conjs[i+2]) {
			out = append(out, conjs[i]+"'"+conjs[i+2])
			i += 3
		} else {
			out = append(out, conjs[i])
			i++
		}
	}
	return out
}
