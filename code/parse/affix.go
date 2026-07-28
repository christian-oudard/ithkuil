package parse

import (
	"fmt"

	"github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
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

// CaStackVowel is §3.5/§3.7's specialized Vx marking the following Cs
// as a Ca complex stacked on the Slot VI Ca. It carries no degree, so
// it sits outside the three degree tables above.
const CaStackVowel = "üö"

// column4Degrees maps Column-4 vowels to their §4.6.5 form number,
// which selects one of the nine Transrelative cases. These are not
// affix degrees: the Cs they sit on is a referential consonant form,
// not an affix, and the pair is a case shortcut rather than a graded
// affix. Ambiguity with a §3.9.2 Type-3 case-accessor is ruled out by
// §4.6.5's own restriction that a referential affix may not add the
// Abstract Perspective increments -w or -y, which is exactly what the
// accessor Cs forms all end in.
var column4Degrees = map[string]int{
	"ao": 1, "aö": 2, "eo": 3, "eö": 4, "oë": 5,
	"öe": 6, "oe": 7, "öa": 8, "oa": 9,
}

var column4Vowels = [...]string{"", "ao", "aö", "eo", "eö", "oë", "öe", "oe", "öa", "oa"}

// AffixVowel returns the canonical surface vowel for an affix of the
// given Type and Degree (0-9). For Type-3, the canonical (non-alternate)
// form is returned.
//
// Panics on a Type or Degree outside the §3.5 table. Nothing a user can
// write reaches that: parsing derives degrees from AffixVowelDegree,
// whose tables only hold 0-9, and the compose syntax matches a single
// digit. An out-of-range value therefore means a Formative was built in
// code that no surface form can express. Returning "" instead would
// splice an affix with no vowel into the output and yield a different,
// well-formed word with no complaint.
func AffixVowel(t grammar.AffixType, degree int) string {
	if t == grammar.CaStackAffix {
		return CaStackVowel
	}
	if t == grammar.Column4Affix {
		if degree < 1 || degree > 9 {
			panic(fmt.Sprintf("parse.AffixVowel: Column-4 form %d outside 1-9", degree))
		}
		return column4Vowels[degree]
	}
	if degree < 0 || degree > 9 {
		panic(fmt.Sprintf("parse.AffixVowel: degree %d outside 0-9", degree))
	}
	switch t {
	case grammar.Type1Affix:
		return type1Vowels[degree]
	case grammar.Type2Affix:
		return type2Vowels[degree]
	case grammar.Type3Affix:
		return type3Vowels[degree]
	}
	panic(fmt.Sprintf("parse.AffixVowel: unknown affix type %d", t))
}

// Type2DegreeToVowel returns the Series-2 vowel for a degree (0-9).
// Convenience wrapper around AffixVowel for Type-2 callers.
func Type2DegreeToVowel(degree int) string {
	return AffixVowel(grammar.Type2Affix, degree)
}

// AffixVowelDegree returns the AffixType and degree (0-9) of an affix
// vowel Vx, and whether v is a Vx form at all. The §3.5 table is the
// whole inventory: anything outside it is not an affix vowel, and a
// caller building a Formative has to say so rather than pick a degree.
func AffixVowelDegree(v string) (grammar.AffixType, int, bool) {
	if v == CaStackVowel {
		return grammar.CaStackAffix, 0, true
	}
	if d, ok := column4Degrees[v]; ok {
		return grammar.Column4Affix, d, true
	}
	if d, ok := type1Degrees[v]; ok {
		return grammar.Type1Affix, d, true
	}
	if d, ok := type2Degrees[v]; ok {
		return grammar.Type2Affix, d, true
	}
	if d, ok := type3Degrees[v]; ok {
		return grammar.Type3Affix, d, true
	}
	return grammar.Type1Affix, 0, false
}

// ClassifyAffixVowel is the lenient form of AffixVowelDegree, for the
// callers that are sniffing at a word's shape rather than decoding it
// — an unrecognized vowel reads as (Type1Affix, 0) instead of failing.
// Anything that produces a Formative wants AffixVowelDegree.
func ClassifyAffixVowel(v string) (grammar.AffixType, int) {
	t, d, _ := AffixVowelDegree(v)
	return t, d
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
	return pairConjunctAffixes(phonology.SplitConjuncts(text))
}

func pairConjunctAffixes(parts []string) []grammar.Affix {
	var out []grammar.Affix
	i := 0
	for i+1 < len(parts) {
		a, b := parts[i], parts[i+1]
		switch {
		case phonology.IsVowelConjunct(a) && phonology.IsConsonantConjunct(b):
			t, d := ClassifyAffixVowel(a)
			out = append(out, grammar.Affix{Type: t, Degree: d, Consonant: b})
			i += 2
		case phonology.IsConsonantConjunct(a) && phonology.IsVowelConjunct(b):
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
