package slots

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

	csRoot := g.MinimalFormative("ml")
	csRoot.Root = g.CsRoot{Cs: "r", Degree: 5, Version: g.PRC, Function: g.STA, Context: g.EXS}

	csRootCpt := g.MinimalFormative("ml")
	csRootCpt.Root = g.CsRoot{Cs: "r", Degree: 5, Version: g.CPT, Function: g.DYN, Context: g.EXS}

	refRoot := g.MinimalFormative("ml")
	refRoot.Root = g.RefRoot{C1: "l", Version: g.PRC, SlotIV: g.DefaultSlotIV}

	refRootCpt := g.MinimalFormative("ml")
	refRootCpt.Root = g.RefRoot{C1: "l", Version: g.CPT, SlotIV: g.DefaultSlotIV}

	withSlot8 := g.MinimalFormative("ml")
	withSlot8.SlotVIII = g.VnCnValence{Valence: g.PRL, MoodScope: g.SUB}

	withAspect := g.MinimalFormative("ml")
	withAspect.SlotVIII = g.VnCnAspect{Aspect: g.RTR, MoodScope: g.FAC}

	withPhase := g.MinimalFormative("ml")
	withPhase.SlotVIII = g.VnCnPhase{Phase: g.PCT, MoodScope: g.FAC}

	t2 := g.Type2
	withConcatT2 := g.MinimalFormative("ml")
	withConcatT2.Concat = &t2

	nonDefaultCa := g.MinimalFormative("ml")
	nonDefaultCa.SlotVI = g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}

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
		{"concat-type2", withConcatT2},
		{"cs-root", csRoot},
		{"cs-root-cpt-dyn", csRootCpt},
		{"ref-root", refRoot},
		{"ref-root-cpt", refRootCpt},
		{"slot8-valence-sub", withSlot8},
		{"slot8-aspect", withAspect},
		{"slot8-phase", withPhase},
		{"non-default-ca", nonDefaultCa},
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

// TestFromGrammar_Shortcut exercises the Cc shortcut path in
// FromGrammar — only fires when Options{Shortcut: true} and the
// formative has the right shape (CrRoot, default SlotIV, no Slot V,
// Slot VI in the shortcut table).
func TestFromGrammar_Shortcut(t *testing.T) {
	// Default formative is shortcut-eligible.
	f := g.MinimalFormative("ml")
	l := FromGrammar(f, Options{Shortcut: true})
	if l.Cc == "" {
		t.Error("Shortcut: expected non-empty Cc on minimal formative")
	}
	got, err := ToGrammar(l)
	if err != nil {
		t.Fatalf("ToGrammar after shortcut: %v", err)
	}
	if !reflect.DeepEqual(got, f) {
		t.Errorf("shortcut round-trip drift:\n  got  %+v\n  want %+v", got, f)
	}
}

func TestFromGrammar_Slot8_EffectAndLevel(t *testing.T) {
	withEffect := g.MinimalFormative("ml")
	withEffect.SlotVIII = g.VnCnEffect{Effect: g.BEN1, MoodScope: g.FAC}

	withLevel := g.MinimalFormative("ml")
	withLevel.SlotVIII = g.VnCnLevel{Level: g.MIN, MoodScope: g.FAC}

	for _, tc := range []struct {
		name string
		f    g.Formative
	}{
		{"slot8-effect-ben1", withEffect},
		{"slot8-level-min", withLevel},
	} {
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

// TestParse_MovedGlottalRoundTrip exercises Layout.MovedGlottal and
// restoreMovedGlottal: a §3.9.1 moved-glottal Vc form survives a
// parse-then-render round-trip (case 37+ gets the glottal back).
func TestParse_MovedGlottalRoundTrip(t *testing.T) {
	l, err := Parse("la'la")
	if err != nil {
		t.Fatalf("Parse(la'la): %v", err)
	}
	if !l.MovedGlottal {
		t.Error("Parse(la'la) should set MovedGlottal")
	}
	f, err := ToGrammar(l)
	if err != nil {
		t.Fatalf("ToGrammar: %v", err)
	}
	un, ok := f.Final.(g.UnframedNominal)
	if !ok || un.Case != g.PRN {
		t.Errorf("Final = %+v, want UnframedNominal{PRN}", f.Final)
	}
}
