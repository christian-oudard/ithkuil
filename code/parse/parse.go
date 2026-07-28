// Package parse contains the primitives for segmenting Ithkuil text into
// morphological pieces. surface.SplitConjuncts is the entry point that every higher
// layer of parsing builds on; per-slot parsers (ParseSlotII, …) decode
// individual vowel/consonant conjuncts into grammar values.
package parse

import (
	"strings"

	"github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
)

// accentMap strips stress marks for parsing. Acute accents map to the
// plain vowel (á→a); circumflex maps to the umlauted variant (â→ä).
var accentMap = map[rune]rune{
	'á': 'a', 'é': 'e', 'í': 'i', 'ó': 'o', 'ú': 'u',
	'â': 'ä', 'ê': 'ë', 'ô': 'ö', 'û': 'ü',
}

// NormalizeAccents removes stress marks from a vowel sequence so the
// downstream lookup tables see only the base 9-vowel inventory.
func NormalizeAccents(s string) string {
	var b strings.Builder
	b.Grow(len(s))
	for _, r := range s {
		if rep, ok := accentMap[r]; ok {
			b.WriteRune(rep)
		} else {
			b.WriteRune(r)
		}
	}
	return b.String()
}

// ParseSlotII decodes a Vv vowel into (Stem, Version). The series is
// ignored — Series 3 alternates resolve to the same form number — and the
// form number selects stem and version:
//
//	1,2 → S1   3,4 → S2   7,6 → S0   9,8 → S3
//	odd form → PRC, even form → CPT
//
// Form 5 ("ëi") is the Cs-root special Vv and is rejected here; its
// handling lives in the Cs-root branch of the full parser.
func ParseSlotII(v string) (grammar.SlotII, bool) {
	_, form, ok := phonology.VowelFormLookup(NormalizeAccents(v))
	if !ok {
		return grammar.SlotII{}, false
	}
	switch form {
	case 1:
		return grammar.SlotII{Stem: grammar.S1, Version: grammar.PRC}, true
	case 2:
		return grammar.SlotII{Stem: grammar.S1, Version: grammar.CPT}, true
	case 3:
		return grammar.SlotII{Stem: grammar.S2, Version: grammar.PRC}, true
	case 4:
		return grammar.SlotII{Stem: grammar.S2, Version: grammar.CPT}, true
	case 6:
		return grammar.SlotII{Stem: grammar.S0, Version: grammar.CPT}, true
	case 7:
		return grammar.SlotII{Stem: grammar.S0, Version: grammar.PRC}, true
	case 8:
		return grammar.SlotII{Stem: grammar.S3, Version: grammar.CPT}, true
	case 9:
		return grammar.SlotII{Stem: grammar.S3, Version: grammar.PRC}, true
	}
	return grammar.SlotII{}, false
}

// ParseSlotIV decodes a Vr vowel into (Function, Specification, Context).
// The series gives the context (1=EXS, 2=FNC, 3=RPS, 4=AMG); the form
// gives function and specification per the V4 grammar table. Form 5 is
// the Cs-root special and is rejected here.
func ParseSlotIV(v string) (grammar.SlotIV, bool) {
	series, form, ok := phonology.VowelFormLookup(NormalizeAccents(v))
	if !ok {
		return grammar.SlotIV{}, false
	}
	var ctx grammar.Context
	switch series {
	case 1:
		ctx = grammar.EXS
	case 2:
		ctx = grammar.FNC
	case 3:
		ctx = grammar.RPS
	case 4:
		ctx = grammar.AMG
	default:
		return grammar.SlotIV{}, false
	}
	var fn grammar.Function
	var spec grammar.Specification
	switch form {
	case 1:
		fn, spec = grammar.STA, grammar.BSC
	case 2:
		fn, spec = grammar.STA, grammar.CTE
	case 3:
		fn, spec = grammar.STA, grammar.CSV
	case 4:
		fn, spec = grammar.STA, grammar.OBJ
	case 6:
		fn, spec = grammar.DYN, grammar.OBJ
	case 7:
		fn, spec = grammar.DYN, grammar.CSV
	case 8:
		fn, spec = grammar.DYN, grammar.CTE
	case 9:
		fn, spec = grammar.DYN, grammar.BSC
	default:
		return grammar.SlotIV{}, false
	}
	return grammar.SlotIV{Function: fn, Specification: spec, Context: ctx}, true
}
