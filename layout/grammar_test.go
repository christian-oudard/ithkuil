package layout

import (
	"reflect"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// TestFromGrammar_ToGrammar_RoundTrip drives Layer D inverse-then-
// forward and checks the original Formative is recovered. The corpus
// is a hand-picked set spanning shapes (Cr/Cs/Ref) and Final variants
// (nominal/verbal/framed).
func TestFromGrammar_ToGrammar_RoundTrip(t *testing.T) {
	stem3 := g.MinimalFormative("ml")
	stem3.Root = g.CrRoot{Cluster: "ml", Stem: g.S3, Version: g.CPT, SlotIV: g.DefaultSlotIV}

	verbal := g.MinimalFormative("ml")
	verbal.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}

	framed := g.MinimalFormative("ml")
	framed.Final = g.FramedVerbal{Case: g.THM}

	withAffixes := g.MinimalFormative("ml")
	withAffixes.SlotVII = []g.Affix{{Type: g.Type1Affix, Degree: 1, Consonant: "r"}}

	withSlotV := g.MinimalFormative("ml")
	withSlotV.SlotV = []g.Affix{
		{Type: g.Type1Affix, Degree: 1, Consonant: "r"},
		{Type: g.Type1Affix, Degree: 1, Consonant: "r"},
	}

	withConcat := g.MinimalFormative("ml")
	t1 := g.Type1
	withConcat.Concat = &t1

	cases := []struct {
		name string
		f    g.Formative
	}{
		{"minimal", g.MinimalFormative("ml")},
		{"non-default-stem", stem3},
		{"verbal", verbal},
		{"framed-thm", framed},
		{"slot7-affix", withAffixes},
		{"slot5-affixes", withSlotV},
		{"concat-type1", withConcat},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			l := FromGrammar(tc.f, Options{})
			got, err := ToGrammar(l)
			if err != nil {
				t.Fatalf("ToGrammar: %v", err)
			}
			if !reflect.DeepEqual(got, tc.f) {
				t.Errorf("round-trip drift:\n  got  %+v\n  want %+v", got, tc.f)
			}
		})
	}
}
