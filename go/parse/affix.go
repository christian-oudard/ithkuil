package parse

import (
	"unicode/utf8"

	"github.com/coudard/ithkuil/go/grammar"
)

// type1Degrees maps Series-1 vowels to degree (1-9, plus 0 for "ae").
var type1Degrees = map[string]int{
	"a": 1, "ä": 2, "e": 3, "i": 4, "ëi": 5,
	"ö": 6, "o": 7, "ü": 8, "u": 9, "ae": 0,
}

// type2Degrees maps Series-2 vowels to degree.
var type2Degrees = map[string]int{
	"ai": 1, "au": 2, "ei": 3, "eu": 4, "ëu": 5,
	"ou": 6, "oi": 7, "iu": 8, "ui": 9, "ea": 0,
}

// type3Degrees maps Series-3 vowels to degree, including series-3
// alternates (uä/uë/üä/üë/öë/öä/ië/iä) and the "üo" 0-degree special.
var type3Degrees = map[string]int{
	"ia": 1, "uä": 1, "ie": 2, "uë": 2,
	"io": 3, "üä": 3, "iö": 4, "üë": 4,
	"eë": 5,
	"uö": 6, "öë": 6, "uo": 7, "öä": 7,
	"ue": 8, "ië": 8, "ua": 9, "iä": 9,
	"üo": 0,
}

var type1Vowels = [...]string{"ae", "a", "ä", "e", "i", "ëi", "ö", "o", "ü", "u"}
var type2Vowels = [...]string{"ea", "ai", "au", "ei", "eu", "ëu", "ou", "oi", "iu", "ui"}
var type3Vowels = [...]string{"üo", "ia", "ie", "io", "iö", "eë", "uö", "uo", "ue", "ua"}

// AffixVowel returns the canonical surface vowel for an affix of the
// given Type and Degree (0-9). For Type-3, the canonical (non-alternate)
// form is returned. An out-of-range degree returns the empty string.
func AffixVowel(t grammar.AffixType, degree int) string {
	if degree < 0 || degree > 9 {
		return ""
	}
	switch t {
	case grammar.Type1Affix:
		return type1Vowels[degree]
	case grammar.Type2Affix:
		return type2Vowels[degree]
	case grammar.Type3Affix:
		return type3Vowels[degree]
	}
	return ""
}

// Type2DegreeToVowel returns the Series-2 vowel for a degree (0-9).
// Convenience wrapper around AffixVowel for Type-2 callers.
func Type2DegreeToVowel(degree int) string {
	return AffixVowel(grammar.Type2Affix, degree)
}

// ClassifyAffixVowel returns the AffixType and degree (0-9) of an affix
// vowel Vx. Unrecognized vowels default to (Type1Affix, 0) — matching
// the Haskell reference, which uses this only when a richer parser has
// already validated the input.
func ClassifyAffixVowel(v string) (grammar.AffixType, int) {
	if d, ok := type1Degrees[v]; ok {
		return grammar.Type1Affix, d
	}
	if d, ok := type2Degrees[v]; ok {
		return grammar.Type2Affix, d
	}
	if d, ok := type3Degrees[v]; ok {
		return grammar.Type3Affix, d
	}
	return grammar.Type1Affix, 0
}

// IsVowelConjunct reports whether a conjunct begins with a vowel.
// Returns false for the empty string.
func IsVowelConjunct(s string) bool {
	if s == "" {
		return false
	}
	r, _ := utf8.DecodeRuneInString(s)
	return IsVowelChar(r)
}

// IsConsonantConjunct reports whether a conjunct begins with a
// consonant. Returns false for the empty string.
func IsConsonantConjunct(s string) bool {
	if s == "" {
		return false
	}
	r, _ := utf8.DecodeRuneInString(s)
	return !IsVowelChar(r)
}

// ParseAffixes parses a sub-string of a formative into a list of affixes.
// Both orderings work:
//   - VxCs (vowel-then-consonant) for Slot VII.
//   - CsVx (consonant-then-vowel) for Slot V.
//
// Trailing odd conjuncts and mid-stream mismatches are skipped so that
// recovery is possible after the caller has trimmed too much.
func ParseAffixes(text string) []grammar.Affix {
	if text == "" {
		return nil
	}
	return pairConjunctAffixes(SplitConjuncts(text))
}

func pairConjunctAffixes(parts []string) []grammar.Affix {
	var out []grammar.Affix
	i := 0
	for i+1 < len(parts) {
		a, b := parts[i], parts[i+1]
		switch {
		case IsVowelConjunct(a) && IsConsonantConjunct(b):
			t, d := ClassifyAffixVowel(a)
			out = append(out, grammar.Affix{Type: t, Degree: d, Consonant: b})
			i += 2
		case IsConsonantConjunct(a) && IsVowelConjunct(b):
			t, d := ClassifyAffixVowel(b)
			out = append(out, grammar.Affix{Type: t, Degree: d, Consonant: a})
			i += 2
		default:
			// Two vowels or two consonants in a row — skip one and retry.
			i++
		}
	}
	return out
}
