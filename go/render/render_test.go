package render

import (
	"testing"

	g "github.com/coudard/ithkuil/go/grammar"
)

func TestFormative_Minimal(t *testing.T) {
	// MinimalFormative("ml"): I=∅, II=S1/PRC ("a"), III="ml", IV=STA/BSC/EXS ("a"),
	// V=[], VI=default ("l"), VII=[], VIII=nil, IX=CaseSlot{THM} ("a").
	// Default Vv elides per §3.2 → short form "mlala".
	f := g.MinimalFormative("ml")
	got := Formative(f)
	want := "mlala"
	if got != want {
		t.Errorf("Formative(minimal \"ml\") = %q, want %q", got, want)
	}
}

func TestFormative_NonDefaultSlots(t *testing.T) {
	// Build a formative with several non-default slots and confirm the
	// rendering composes correctly.
	f := g.MinimalFormative("ml")
	f.Root = g.CrRoot{
		Cluster: "ml",
		Stem:    g.S2, Version: g.PRC,
		SlotIV: g.SlotIV{Function: g.DYN, Specification: g.OBJ, Context: g.EXS},
	}
	f.Final = g.UnframedNominal{Case: g.ERG} // Vc = "o"

	got := Formative(f)
	// Vv=e, Cr=ml, Vr=DYN+OBJ → form 6 series 1 (EXS) = "ö", Ca="l", Vc="o"
	want := "emlölo"
	if got != want {
		t.Errorf("Formative(non-default) = %q, want %q", got, want)
	}
}

func TestFormative_Ultimate(t *testing.T) {
	// Verbal formative: Final is UnframedVerbal{Assertive{INF}}.
	// Ultimate marks the last vowel; default Vv elides → "mlalú".
	f := g.MinimalFormative("ml")
	f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.INF}}
	got := Formative(f)
	want := "mlalú"
	if got != want {
		t.Errorf("Formative(verbal) = %q, want %q", got, want)
	}
}

func TestFormative_WithSlotVIII(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SlotVIII = g.VnCnValence{
		Valence: g.MNO,
		MoodScope: g.SUB,
	}
	got := Formative(f)
	// Long: a-ml-a-l-ahl-a. 4 vowels, PEN needs 2, slack 2 → both the
	// default Vv and the THM Vc elide → "mlalahl".
	want := "mlalahl"
	if got != want {
		t.Errorf("Formative(with VIII) = %q, want %q", got, want)
	}
}

func TestFormative_WithAffixes(t *testing.T) {
	f := g.MinimalFormative("ml")
	// Slot VII affix: vowel=a, cons=r → "ar"
	f.SlotVII = []g.Affix{{Type: g.Type1Affix, Degree: 1, Consonant: "r"}}
	got := Formative(f)
	// Long: a-ml-a-l-ar-a. Both default Vv and THM Vc elide → "mlalar".
	want := "mlalar"
	if got != want {
		t.Errorf("Formative(with affix) = %q, want %q", got, want)
	}
}

func TestSlotIRenderings(t *testing.T) {
	t1 := g.Type1
	t2 := g.Type2
	cases := []struct {
		in   *g.ConcatenationStatus
		want string
	}{
		{nil, ""},
		{&t1, "h"},
		{&t2, "hw"},
	}
	for _, c := range cases {
		if got := plainCc(c.in); got != c.want {
			t.Errorf("plainCc(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestVk(t *testing.T) {
	// Assertive renders the Validation Series-1 vowel.
	if got := Vk(g.Assertive{Validation: g.OBS}); got != "a" {
		t.Errorf("Vk(Assertive OBS) = %q, want \"a\"", got)
	}
	if got := Vk(g.Assertive{Validation: g.INF}); got != "u" {
		t.Errorf("Vk(Assertive INF) = %q, want \"u\"", got)
	}
	// Each non-ASR illocution renders its dedicated diphthong.
	cases := []struct {
		variant g.Vk
		want    string
	}{
		{g.Directive{}, "ai"}, {g.Declarative{}, "au"},
		{g.Interrogative{}, "ei"}, {g.Verificative{}, "eu"},
		{g.Admonitive{}, "ou"}, {g.Potentiative{}, "oi"},
		{g.Hortative{}, "iu"}, {g.Conjectural{}, "ui"},
	}
	for _, c := range cases {
		if got := Vk(c.variant); got != c.want {
			t.Errorf("Vk(%T) = %q, want %q", c.variant, got, c.want)
		}
	}
}

// Exhaustive table-driven tests for each Slot VIII variant's encoder.

func TestPhase(t *testing.T) {
	cases := []struct {
		p    g.Phase
		want string
	}{
		{g.PCT, "ai"}, {g.ITR, "au"}, {g.REP, "ei"}, {g.ITM, "eu"},
		{g.RCT, "ëu"}, {g.FRE, "ou"}, {g.FRG, "oi"}, {g.VAC, "iu"},
		{g.FLC, "ui"},
	}
	for _, c := range cases {
		if got := Phase(c.p); got != c.want {
			t.Errorf("Phase(%v) = %q, want %q", c.p, got, c.want)
		}
	}
}

func TestEffect(t *testing.T) {
	cases := []struct {
		e    g.Effect
		want string
	}{
		{g.BEN1, "ia"}, {g.BEN2, "ie"}, {g.BEN3, "io"}, {g.BSLF, "iö"},
		{g.UNK, "eë"}, {g.DSLF, "uö"}, {g.DET3, "uo"}, {g.DET2, "ue"},
		{g.DET1, "ua"},
	}
	for _, c := range cases {
		if got := Effect(c.e); got != c.want {
			t.Errorf("Effect(%v) = %q, want %q", c.e, got, c.want)
		}
	}
}

func TestLevel(t *testing.T) {
	cases := []struct {
		l    g.Level
		want string
	}{
		{g.MIN, "ao"}, {g.SBE, "aö"}, {g.IFR, "eo"}, {g.DFT, "eö"},
		{g.EQU, "oë"}, {g.SUR, "öe"}, {g.SPL, "oe"}, {g.SPQ, "öa"},
		{g.MAX, "oa"},
	}
	for _, c := range cases {
		if got := Level(c.l); got != c.want {
			t.Errorf("Level(%v) = %q, want %q", c.l, got, c.want)
		}
	}
}

func TestAspect(t *testing.T) {
	// Verify every one of the 36 aspects renders to a non-empty form
	// and that the spot-check vowels match.
	for _, a := range g.AllAspects {
		if Aspect(a) == "" {
			t.Errorf("Aspect(%v) is empty", a)
		}
	}
	cases := []struct {
		a    g.Aspect
		want string
	}{
		{g.RTR, "a"}, {g.ATP, "u"}, {g.RSM, "ai"},
		{g.IRP, "ui"}, {g.PMP, "ia"}, {g.PPR, "ua"},
		{g.DCL, "ao"}, {g.SQN, "oa"},
	}
	for _, c := range cases {
		if got := Aspect(c.a); got != c.want {
			t.Errorf("Aspect(%v) = %q, want %q", c.a, got, c.want)
		}
	}
}

func TestSlotVIII_AllVariants(t *testing.T) {
	// Exercise each SlotVIII variant through the dispatch.
	cases := []struct {
		name string
		s    g.SlotVIII
	}{
		{"Valence",
			g.VnCnValence{Valence: g.MNO, MoodScope: g.FAC}},
		{"Phase",
			g.VnCnPhase{Phase: g.PCT, MoodScope: g.FAC}},
		{"Effect",
			g.VnCnEffect{Effect: g.BEN1, MoodScope: g.FAC}},
		{"Level",
			g.VnCnLevel{Level: g.MAX, MoodScope: g.FAC}},
		{"Aspect",
			g.VnCnAspect{Aspect: g.RTR, MoodScope: g.FAC}},
	}
	for _, c := range cases {
		if got := SlotVIII(c.s); got == "" {
			t.Errorf("SlotVIII(%s) returned empty", c.name)
		}
	}
}

func TestSlotV_AffixOrdering(t *testing.T) {
	// Slot V uses Cs+Vx (reversed) ordering.
	got := SlotV([]g.Affix{
		{Type: g.Type1Affix, Degree: 1, Consonant: "r"},
		{Type: g.Type1Affix, Degree: 9, Consonant: "t"},
	})
	if got != "ratu" {
		t.Errorf("SlotV = %q, want \"ratu\"", got)
	}
}

func TestFormative_THMVcElision_KeepsForMinimal(t *testing.T) {
	// MinimalFormative("ml") has 3 syllables in long form (a-ml-a-l-a).
	// PEN needs 2; slack is 1; only one elision fits. Prefer Vv.
	f := g.MinimalFormative("ml")
	got := Formative(f)
	want := "mlala"
	if got != want {
		t.Errorf("Formative(minimal) = %q, want %q", got, want)
	}
}

func TestFormative_THMVcElision_NotForUltimate(t *testing.T) {
	// Ultimate stress carries Vk, not Vc — the trailing-a rule doesn't
	// apply, only Vv elides. The diacritic still lands on the last vowel.
	f := g.MinimalFormative("ml")
	f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
	got := Formative(f)
	want := "mlalá"
	if got != want {
		t.Errorf("Formative(ultimate-ASR) = %q, want %q", got, want)
	}
}

func TestFormative_ShortForm_AntepenultKeepsLong(t *testing.T) {
	// Antepenult stress needs ≥3 syllables. Eliding the leading "a" of a
	// 3-syllable long form would leave 2 syllables, breaking the stress.
	// The renderer must keep the long form so the diacritic can land.
	f := g.MinimalFormative("ml")
	f.Final = g.FramedVerbal{Case: g.THM}
	got := Formative(f)
	want := "ámlala"
	if got != want {
		t.Errorf("Formative(antepenult) = %q, want %q", got, want)
	}
}

func TestFormative_ShortForm_NonDefaultVvNoElision(t *testing.T) {
	// Non-default Stem (S2/PRC = "e") never elides; the THM Vc still does.
	f := g.MinimalFormative("ml")
	cr := f.Root.(g.CrRoot)
	cr.Stem = g.S2
	f.Root = cr
	got := Formative(f)
	want := "emlal"
	if got != want {
		t.Errorf("Formative(S2/PRC) = %q, want %q", got, want)
	}
}

func TestFormative_ShortForm_WithSlotI_NoElision(t *testing.T) {
	// Slot I prefix locks the formative into long form for Vv; THM Vc
	// still elides.
	f := g.MinimalFormative("ml")
	t1 := g.Type1
	f.Concat = &t1
	got := Formative(f)
	want := "hamlal"
	if got != want {
		t.Errorf("Formative(T1) = %q, want %q", got, want)
	}
}

func TestFormative_ANTPaddingNilSlotIX(t *testing.T) {
	// A Formative with empty Slot IX content (FramedVerbal{THM}) renders
	// Vv+Cr+Vr+Ca = 2 syllables (a-m-a-l). ANT stress needs 3; §5.8.8
	// says pad with default Slot IX → append "a", yielding "amala",
	// then mark ANT → "ámala".
	f := g.Formative{
		Root:   g.DefaultCrRoot("m"),
		SlotVI: g.DefaultSlotVI,
		Final:  g.FramedVerbal{Case: g.THM},
	}
	got := Formative(f)
	want := "ámala"
	if got != want {
		t.Errorf("Formative(ANT, nil SlotIX) = %q, want %q", got, want)
	}
}

func TestFormative_ShortcutW_Series1(t *testing.T) {
	f := g.MinimalFormative("ml")
	got := FormativeWithOpts(f, Options{Shortcut: true})
	want := "wamla"
	if got != want {
		t.Errorf("Formative(W shortcut series 1) = %q, want %q", got, want)
	}
}

func TestFormative_ShortcutW_Series2(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SlotVI = g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL,
		Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}
	got := FormativeWithOpts(f, Options{Shortcut: true})
	want := "waimla"
	if got != want {
		t.Errorf("Formative(W shortcut series 2) = %q, want %q", got, want)
	}
}

func TestFormative_ShortcutY_Series1(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SlotVI = g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL,
		Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}
	got := FormativeWithOpts(f, Options{Shortcut: true})
	want := "yamla"
	if got != want {
		t.Errorf("Formative(Y shortcut series 1) = %q, want %q", got, want)
	}
}

func TestFormative_ShortcutWithConcat(t *testing.T) {
	f := g.MinimalFormative("ml")
	t1 := g.Type1
	f.Concat = &t1
	got := FormativeWithOpts(f, Options{Shortcut: true})
	want := "hlamla"
	if got != want {
		t.Errorf("Formative(T1+W shortcut) = %q, want %q", got, want)
	}
}

func TestApplyFinalStress(t *testing.T) {
	verbal := g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
	nominal := g.UnframedNominal{Case: g.THM}
	framed := g.FramedVerbal{Case: g.THM}
	cases := []struct {
		name  string
		word  string
		final g.Final
		want  string
	}{
		{"nominal-unmarked", "amlala", nominal, "amlala"},
		{"monosyllabic-verbal-unmarked", "mal", verbal, "mal"},
		{"ultimate-3syl", "amlalu", verbal, "amlalú"},
		{"ultimate-marks-first-of-diphthong", "amlaleu", verbal, "amlaléu"},
		{"antepenult-3syl", "amlala", framed, "ámlala"},
		{"antepenult-4syl", "agulahla", framed, "agúlahla"},
		{"antepenult-too-short-noop", "amla", framed, "amla"},
		{"ultimate-umlaut", "amläl", verbal, "amlâl"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			if got := applyFinalStress(c.word, c.final); got != c.want {
				t.Errorf("applyFinalStress(%q, %v) = %q, want %q", c.word, c.final, got, c.want)
			}
		})
	}
}

