// This file decodes the two §4.6 referential shapes, the way
// fullparse.Formative decodes a formative: romanization in, a grammar
// value out.
//
// Both run the phonotactic checks before returning. §4.6.1 says the
// epenthetic -ë- appears "if necessary due to phonotactic rules", so a
// referential is subject to the ordinary cluster constraints, and a
// cluster that breaks them is not a word we should be reading. Leaving
// the check out let "lxa" classify as a referential while the
// validator rejected it as unpronounceable.

package fullparse

import (
	"errors"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
)

// errNotReferential says the romanization does not have the shape, which is
// the ordinary outcome when the classifier is trying parsers in turn.
var errNotReferential = errors.New("not a referential")

// phonotactic reads word as phonology, reporting what it breaks. The
// referential decoders run it because a word the phonotactics reject
// is not a referential either, whatever shape it has.
func phonotactic(word string) error {
	return phonology.CheckText(word)
}

// Referential decodes a §4.6.1 single- or dual-referential:
//
//	[ë] C1 Vc1 [w/y Vc2 [C2 [ë]]]
//
// Ultimate stress maps to RPV essence.
func Referential(word string) (g.Referential, error) {
	if err := phonotactic(word); err != nil {
		return g.Referential{}, err
	}
	bare, stress := phonology.Strip(word)
	if stress == phonology.InvalidStress {
		return g.Referential{}, errNotReferential
	}
	conjs := absorbRule1Glottals(phonology.MergeGlottalVowels(phonology.SplitConjuncts(bare)))
	if len(conjs) < 2 {
		return g.Referential{}, errNotReferential
	}
	i := 0
	// §4.6.3 epenthesis: "üo-" lets a C_P suppletive cluster occupy
	// C1 instead of a personal-reference cluster. We track that and
	// route the C_P through ParseCarrierType below.
	cpEpenthesis := false
	if conjs[i] == "ë" {
		i++
		if i+1 >= len(conjs) {
			return g.Referential{}, errNotReferential
		}
	} else if conjs[i] == "üo" && i+1 < len(conjs) {
		if _, isCp := parse.ParseCarrierType(conjs[i+1]); isCp {
			cpEpenthesis = true
			i++
		}
	}
	c1 := conjs[i]
	if !phonology.IsConsonantConjunct(c1) {
		return g.Referential{}, errNotReferential
	}
	var head g.RefHead
	if cpEpenthesis {
		ct, _ := parse.ParseCarrierType(c1)
		head = g.SuppletiveHead{Type: ct}
	} else {
		cat, refs, ok := parse.DecomposeRefWithCategory(c1)
		if !ok || len(refs) == 0 {
			return g.Referential{}, errNotReferential
		}
		head = g.PersonalHead{Refs: refs, Category: cat}
	}
	i++
	if i >= len(conjs) || !phonology.IsVowelConjunct(conjs[i]) {
		return g.Referential{}, errNotReferential
	}
	caseA, caseAok := parse.ParseCase(conjs[i])
	if !caseAok {
		return g.Referential{}, errNotReferential
	}
	i++

	var second *g.SecondReferent
	if i < len(conjs) && (conjs[i] == "w" || conjs[i] == "y") {
		i++
		if i >= len(conjs) || !phonology.IsVowelConjunct(conjs[i]) {
			return g.Referential{}, errNotReferential
		}
		c2v, c2ok := parse.ParseCase(conjs[i])
		if !c2ok {
			return g.Referential{}, errNotReferential
		}
		second = &g.SecondReferent{Case: c2v}
		i++
		if i < len(conjs) && phonology.IsConsonantConjunct(conjs[i]) {
			rs, dok := parse.DecomposeRefCluster(conjs[i])
			if !dok || len(rs) == 0 {
				return g.Referential{}, errNotReferential
			}
			second.Refs = rs
			i++
			if i < len(conjs) && conjs[i] == "ë" {
				i++
			}
		}
	}
	if i != len(conjs) {
		return g.Referential{}, errNotReferential
	}
	return g.Referential{
		Head:       head,
		Case:       caseA,
		Second:     second,
		RpvEssence: stress == phonology.Ultimate,
	}, nil
}

// CombinationReferential decodes the §4.6.2 shape
// [ë] C1 Vc Spec [VxCs...] [Vc2]. Returns an error if any constraint fails. The Vc2 special form "üa" maps to THM and "a" alone means
// "no second case".
func CombinationReferential(text string) (g.CombinationReferential, error) {
	if err := phonotactic(text); err != nil {
		return g.CombinationReferential{}, err
	}
	// §4.6.2 slot 6: ultimate stress gives the adjunct RPV Essence.
	// The diacritic has to come off before any vowel is looked up, or
	// a stressed affix vowel decodes to the wrong degree.
	bare, stress := phonology.Strip(text)
	if stress == phonology.InvalidStress {
		return g.CombinationReferential{}, errNotReferential
	}
	// Vc is looked up as a whole conjunct, so a glottalized case vowel
	// ("a'o" in mma'oxinļ) has to be one conjunct rather than the three
	// SplitConjuncts leaves it as.
	conjs := absorbRule1Glottals(phonology.MergeGlottalVowels(phonology.SplitConjuncts(bare)))
	// §4.6.3 epenthesis: "a-" lets a C_P suppletive cluster occupy C1
	// instead of a personal-reference cluster. Otherwise "ë" is the
	// only acceptable prefix.
	cpEpenthesis := false
	if len(conjs) > 1 && conjs[0] == "a" {
		if _, isCp := parse.ParseCarrierType(conjs[1]); isCp {
			cpEpenthesis = true
			conjs = conjs[1:]
		}
	} else if len(conjs) > 0 && conjs[0] == "ë" {
		conjs = conjs[1:]
	}
	if len(conjs) < 3 {
		return g.CombinationReferential{}, errNotReferential
	}
	c1, vc, specConj := conjs[0], conjs[1], conjs[2]
	if !phonology.IsConsonantConjunct(c1) || !phonology.IsVowelConjunct(vc) {
		return g.CombinationReferential{}, errNotReferential
	}
	spec, specOK := parseCombinationSpec(specConj)
	if !specOK {
		return g.CombinationReferential{}, errNotReferential
	}
	var head g.RefHead
	if cpEpenthesis {
		ct, _ := parse.ParseCarrierType(c1)
		head = g.SuppletiveHead{Type: ct}
	} else {
		cat, refs, refsOk := parse.DecomposeRefWithCategory(c1)
		if !refsOk || len(refs) == 0 {
			return g.CombinationReferential{}, errNotReferential
		}
		head = g.PersonalHead{Refs: refs, Category: cat}
	}
	caseVal, caseOk := parse.ParseCase(vc)
	if !caseOk {
		return g.CombinationReferential{}, errNotReferential
	}
	// Pair up the rest as VxCs with optional trailing Vc2.
	rest := conjs[3:]
	// A geminated consonant anywhere in the tail (e.g. "kk" in ţnaxekka)
	// signals this is actually a formative with a Slot V boundary,
	// not a combination referential.
	for _, c := range rest {
		if phonology.IsConsonantConjunct(c) && hasDoubledLetter(c) {
			return g.CombinationReferential{}, errNotReferential
		}
	}
	var affixes []g.Affix
	var case2 *g.Case
	for i := 0; i < len(rest); {
		if i+1 < len(rest) &&
			phonology.IsVowelConjunct(rest[i]) &&
			phonology.IsConsonantConjunct(rest[i+1]) {
			t, d := parse.ClassifyAffixVowel(rest[i])
			affixes = append(affixes, g.Affix{Type: t, Degree: d, Consonant: rest[i+1]})
			i += 2
			continue
		}
		if i == len(rest)-1 && phonology.IsVowelConjunct(rest[i]) {
			// Final Vc2: special-case "a" (no case) and "üa" → THM.
			switch rest[i] {
			case "a":
				// no second case
			case "üa":
				thm := g.THM
				case2 = &thm
			default:
				if c, ok := parse.ParseCase(rest[i]); ok {
					case2 = &c
				}
			}
			i++
			continue
		}
		return g.CombinationReferential{}, errNotReferential
	}
	return g.CombinationReferential{
		Head:       head,
		Case:       caseVal,
		Spec:       spec,
		Affixes:    affixes,
		Case2:      case2,
		RpvEssence: stress == phonology.Ultimate,
	}, nil
}

// absorbRule1Glottals moves a §1.7 Rule 1 glottal-stop back onto the
// vowel-form it was inserted into.
//
// §1.7 offers two placements. Rule 1 leaves the glottal after the
// vowel-form (a → a'), and SplitConjuncts then hands it to us on the
// front of the consonant that follows. Rule 3's epenthetic spelling
// (a → a'a, ai → a'i) is forced only where Rule 1 will not do, notably
// word-finally, and reaches us already merged into the vowel conjunct.
// The case tables are keyed on the Rule 3 spelling because a formative's
// word-final V_C always lands there, so a Rule 1 glottal has to be put
// back before the case can be looked up.
//
// Only referentials need this. Their vowel slots are all cases (§4.6.1
// V_C1 and V_C2, §4.6.2 V_C), and they have no Slot V, so a glottal in
// this position cannot be a §3.6.2 end-of-slot marker the way it can in
// a formative.
func absorbRule1Glottals(conjs []string) []string {
	out := make([]string, len(conjs))
	copy(out, conjs)
	for i := 1; i < len(out); i++ {
		// A bare "'" is a word-final glottal, which Rule 3 forbids;
		// leave it to be rejected rather than absorbing it.
		if out[i] == "'" || !strings.HasPrefix(out[i], "'") {
			continue
		}
		if !phonology.IsVowelConjunct(out[i-1]) {
			continue
		}
		out[i] = strings.TrimPrefix(out[i], "'")
		out[i-1] = phonology.GlottalizeVowel(out[i-1])
	}
	return out
}

// parseCombinationSpec decodes a Specification consonant marker
// (x/xt/xp/xx — §4.6.2) into the typed Specification enum.
func parseCombinationSpec(c string) (g.Specification, bool) {
	switch c {
	case "x":
		return g.BSC, true
	case "xt":
		return g.CTE, true
	case "xp":
		return g.CSV, true
	case "xx":
		return g.OBJ, true
	}
	return 0, false
}

// hasDoubledLetter reports whether s contains two consecutive identical
// runes. Used to detect geminated Ca clusters — a signal that an
// ambiguous word is a formative (with §3.6.1 gemination marking Slot V
// boundary) rather than a combination referential (whose post-spec
// affix Cs values never contain doubled letters).
func hasDoubledLetter(s string) bool {
	var prev rune
	for i, r := range s {
		if i > 0 && r == prev {
			return true
		}
		prev = r
	}
	return false
}
