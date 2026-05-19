// Package parse contains the primitives for segmenting Ithkuil text into
// morphological pieces. SplitConjuncts is the entry point that every higher
// layer of parsing builds on; per-slot parsers (ParseSlotII, …) decode
// individual vowel/consonant conjuncts into grammar values.
package parse

import (
	"strings"

	"github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/phonology"
)

// vowelChars is the set of characters that count as vowels for the purposes
// of conjunct segmentation. Includes the 9 base vowels, accented forms
// (acute = stress, circumflex = umlauted + stress), and diaeresis variants
// used as hiatus markers.
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

// IsVowelChar reports whether r is a vowel character.
func IsVowelChar(r rune) bool {
	return vowelChars[r]
}

// SplitConjuncts segments a word into alternating runs of consonant and
// vowel characters. Empty input yields an empty slice.
//
//	"mala"  → ["m", "a", "l", "a"]
//	"ţřai"  → ["ţř", "ai"]
//	"emal"  → ["e", "m", "a", "l"]
func SplitConjuncts(s string) []string {
	if s == "" {
		return nil
	}
	var out []string
	var run []rune
	var runIsVowel bool
	for i, r := range s {
		v := IsVowelChar(r)
		if i == 0 {
			run = []rune{r}
			runIsVowel = v
			continue
		}
		if v == runIsVowel {
			run = append(run, r)
		} else {
			out = append(out, string(run))
			run = []rune{r}
			runIsVowel = v
		}
	}
	out = append(out, string(run))
	return out
}

// MergeGlottalVowels collapses V-'-V triples back into a single conjunct
// of the form "V'V". SplitConjuncts treats the glottal stop as a
// consonant (it isn't in the vowel-character set), which would split
// glottalized case vowels like "i'a" into three conjuncts. Higher-level
// parsers need them re-merged so case lookup (LOC = "i'a") works.
//
// Leading or trailing glottals without a vowel on both sides are left
// alone.
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

// accentMap strips stress and hiatus marks for parsing. Acute accents map
// to the plain vowel (á→a); circumflex maps to the umlauted variant
// (â→ä); diaeresis on i is the hiatus marker and maps to plain i.
var accentMap = map[rune]rune{
	'á': 'a', 'é': 'e', 'í': 'i', 'ó': 'o', 'ú': 'u',
	'â': 'ä', 'ê': 'ë', 'ô': 'ö', 'û': 'ü',
	'ï': 'i',
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
