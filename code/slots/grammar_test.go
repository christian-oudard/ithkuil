package slots

import (
	"reflect"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// stripSurface returns a copy of f with the Surface hint cleared. Used
// in round-trip tests that build a Formative programmatically (Surface
// = nil) and want to compare against the result of FromGrammar +
// ToGrammar, which records the surface choices made by FromGrammar's
// canonical defaults.
func stripSurface(f g.Formative) g.Formative {
	return f
}

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
	withConcat.Concat = t1

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
	withConcatT2.Concat = t2

	nonDefaultCa := g.MinimalFormative("ml")
	nonDefaultCa.SlotVI = g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}

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
			l := FromGrammar(tc.f)
			got, err := ToGrammar(l)
			if err != nil {
				t.Fatalf("ToGrammar: %v", err)
			}
			if !reflect.DeepEqual(stripSurface(got), tc.f) {
				t.Errorf("round-trip drift:\n  got  %+v\n  want %+v", got, tc.f)
			}
		})
	}
}

// TestFromGrammar_Shortcut exercises the Cc shortcut path in
// FromGrammar. Eligibility needs a CrRoot with default SlotIV and a
// Slot VI the shortcut table can encode, but eligibility alone isn't
// enough: the shortcut is only taken when it produces a shorter
// surface, which needs a Vv that wouldn't have elided anyway.
func TestFromGrammar_Shortcut(t *testing.T) {
	f := g.MinimalFormative("ml")
	cr := f.Root.(g.CrRoot)
	cr.Stem = g.S2
	f.Root = cr
	f.Final = g.UnframedNominal{Case: g.ERG}
	l := FromGrammar(f)
	if l.Cc == "" {
		t.Error("Shortcut: expected non-empty Cc on shortcut-winning formative")
	}
	got, err := ToGrammar(l)
	if err != nil {
		t.Fatalf("ToGrammar after shortcut: %v", err)
	}
	if !reflect.DeepEqual(stripSurface(got), f) {
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
			l := FromGrammar(tc.f)
			got, err := ToGrammar(l)
			if err != nil {
				t.Fatalf("ToGrammar: %v", err)
			}
			if !reflect.DeepEqual(stripSurface(got), tc.f) {
				t.Errorf("round-trip drift:\n  got  %+v\n  want %+v", got, tc.f)
			}
		})
	}
}

// TestParse_ShortcutSlotV exercises the §3.6.2 path: a shortcut Cc
// formative with a Slot V affix whose final Vx carries the end-of-
// slot glottal-stop. The parser must recognise the (Vx, Cs) order
// (not the reversed (Cs, Vx) used when Ca is present) and split the
// "'C" surface conjunct so the leading glottal is stripped from the
// Cs, leaving the affix's true consonant.
func TestParse_ShortcutSlotV(t *testing.T) {
	cases := []struct {
		in     string
		slotV  []AffixChunk
		vc     string
		movedG bool
	}{
		{"wamla'r", []AffixChunk{{Vx: "a", Cs: "r"}}, "", false},
		{"wamla're", []AffixChunk{{Vx: "a", Cs: "r"}}, "e", false},
	}
	for _, c := range cases {
		t.Run(c.in, func(t *testing.T) {
			l, err := Parse(c.in)
			if err != nil {
				t.Fatalf("Parse(%q): %v", c.in, err)
			}
			if !reflect.DeepEqual(l.SlotV, c.slotV) {
				t.Errorf("SlotV = %+v, want %+v", l.SlotV, c.slotV)
			}
			if l.Vc != c.vc {
				t.Errorf("Vc = %q, want %q", l.Vc, c.vc)
			}
			if l.MovedGlottal != c.movedG {
				t.Errorf("MovedGlottal = %v, want %v", l.MovedGlottal, c.movedG)
			}
			if l.Ca != "" {
				t.Errorf("Ca = %q, want \"\" (shortcut form)", l.Ca)
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

// The §3.9.1 glottal may sit inside a vowel conjunct rather than at the
// head of a consonant one — "přa'ölua" carries it intervocalically in
// Vr. MergeGlottalVowels folds that into a single "a'ö" conjunct, so
// stripMovedGlottal has to look inside vowel conjuncts too. The three
// forms below are §3.9.2-3.9.4 examples, all NAV case.
func TestParse_MovedGlottalInsideVr(t *testing.T) {
	cases := []struct{ word, vr string }{
		{"pře'ilua", "ei"},
		{"při'olua", "io"},
		{"přa'ölua", "aö"},
	}
	for _, c := range cases {
		l, err := Parse(c.word)
		if err != nil {
			t.Errorf("Parse(%s): %v", c.word, err)
			continue
		}
		if !l.MovedGlottal {
			t.Errorf("Parse(%s) should set MovedGlottal", c.word)
		}
		if l.Vr != c.vr {
			t.Errorf("Parse(%s) Vr = %q, want %q", c.word, l.Vr, c.vr)
		}
		f, err := ToGrammar(l)
		if err != nil {
			t.Errorf("ToGrammar(%s): %v", c.word, err)
			continue
		}
		un, ok := f.Final.(g.UnframedNominal)
		if !ok || un.Case != g.NAV {
			t.Errorf("%s Final = %+v, want UnframedNominal{NAV}", c.word, f.Final)
		}
	}
}

// A glottal in the last vowel conjunct is an ordinary Vc glottal, not a
// moved one. "iträlo'a" is PLM (case 68), whose Vc is "o'a".
func TestParse_VcGlottalNotMoved(t *testing.T) {
	l, err := Parse("iträlo'a")
	if err != nil {
		t.Fatalf("Parse(iträlo'a): %v", err)
	}
	if l.MovedGlottal {
		t.Error("Parse(iträlo'a) should not set MovedGlottal")
	}
	if l.Vc != "o'a" {
		t.Errorf("Vc = %q, want \"o'a\"", l.Vc)
	}
	f, err := ToGrammar(l)
	if err != nil {
		t.Fatalf("ToGrammar: %v", err)
	}
	un, ok := f.Final.(g.UnframedNominal)
	if !ok || un.Case != g.PLM {
		t.Errorf("Final = %+v, want UnframedNominal{PLM}", f.Final)
	}
}
