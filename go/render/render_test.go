package render

import (
	"testing"

	g "github.com/coudard/ithkuil/go/grammar"
)

func TestFormative_Minimal(t *testing.T) {
	// MinimalFormative("ml"): I=∅, II=S1/PRC ("a"), III="ml", IV=STA/BSC/EXS ("a"),
	// V=[], VI=default ("l"), VII=[], VIII=nil, IX=CaseSlot{THM} ("a")
	f := g.MinimalFormative("ml")
	got := Formative(f)
	want := "amlala"
	if got != want {
		t.Errorf("Formative(minimal \"ml\") = %q, want %q", got, want)
	}
}

func TestFormative_NonDefaultSlots(t *testing.T) {
	// Build a formative with several non-default slots and confirm the
	// rendering composes correctly.
	f := g.MinimalFormative("ml")
	f.SlotII = g.SlotII{Stem: g.S2, Version: g.PRC}     // Vv = "e"
	f.SlotIV = g.SlotIV{                                // Vr = ?
		Function:      g.DYN,
		Specification: g.OBJ,
		Context:       g.EXS,
	}
	f.SlotIX = g.CaseSlot{Case: g.ERG} // Vc = "o"

	got := Formative(f)
	// Vv=e, Cr=ml, Vr=DYN+OBJ → form 6 series 1 (EXS) = "ö", Ca="l", Vc="o"
	want := "emlölo"
	if got != want {
		t.Errorf("Formative(non-default) = %q, want %q", got, want)
	}
}

func TestFormative_Ultimate(t *testing.T) {
	// Verbal formative: SlotIX is IllocValSlot, stress is Ultimate.
	f := g.MinimalFormative("ml")
	f.Stress = g.Ultimate
	f.SlotIX = g.IllocValSlot{Illocution: g.ASR, Validation: g.INF}
	got := Formative(f)
	// Vk for ASR+INF = "u"; everything else default.
	want := "amlalu"
	if got != want {
		t.Errorf("Formative(verbal) = %q, want %q", got, want)
	}
}

func TestFormative_WithSlotVIII(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SlotVIII = g.VnCnValence{
		Valence: g.MNO,
		MS:      g.MoodVal{Mood: g.SUB},
	}
	got := Formative(f)
	// Slot VIII: Vn=a (MNO), Cn=hl (SUB). Slotted between VII and IX.
	// Full: I="" II=a III=ml IV=a V="" VI=l VII="" VIII=ahl IX=a → "amlalahla"
	want := "amlalahla"
	if got != want {
		t.Errorf("Formative(with VIII) = %q, want %q", got, want)
	}
}

func TestFormative_WithAffixes(t *testing.T) {
	f := g.MinimalFormative("ml")
	// Slot VII affix: vowel=a, cons=r → "ar"
	f.SlotVII = []g.Affix{{Vowel: "a", Consonant: "r", Type: g.Type1Affix}}
	got := Formative(f)
	// I="" II=a III=ml IV=a V="" VI=l VII=ar IX=a → "amlalara"
	want := "amlalara"
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
		if got := SlotI(c.in); got != c.want {
			t.Errorf("SlotI(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestVk(t *testing.T) {
	// ASR uses validation vowel.
	if got := Vk(g.ASR, g.OBS); got != "a" {
		t.Errorf("Vk(ASR,OBS) = %q, want \"a\"", got)
	}
	if got := Vk(g.ASR, g.INF); got != "u" {
		t.Errorf("Vk(ASR,INF) = %q, want \"u\"", got)
	}
	// Non-ASR uses illocution vowel.
	cases := []struct {
		ill  g.Illocution
		want string
	}{
		{g.DIR, "ai"}, {g.DEC, "au"}, {g.IRG, "ei"}, {g.VER, "eu"},
		{g.ADM, "ou"}, {g.POT, "oi"}, {g.HOR, "iu"}, {g.CNJ, "ui"},
	}
	for _, c := range cases {
		// Validation ignored for non-ASR.
		if got := Vk(c.ill, g.OBS); got != c.want {
			t.Errorf("Vk(%v, OBS) = %q, want %q", c.ill, got, c.want)
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
			g.VnCnValence{Valence: g.MNO, MS: g.MoodVal{Mood: g.FAC}}},
		{"Phase",
			g.VnCnPhase{Phase: g.PCT, MS: g.MoodVal{Mood: g.FAC}}},
		{"Effect",
			g.VnCnEffect{Effect: g.BEN1, MS: g.MoodVal{Mood: g.FAC}}},
		{"Level",
			g.VnCnLevel{Level: g.MAX, MS: g.MoodVal{Mood: g.FAC}}},
		{"Aspect",
			g.VnCnAspect{Aspect: g.RTR, MS: g.MoodVal{Mood: g.FAC}}},
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
		{Vowel: "a", Consonant: "r", Type: g.Type1Affix},
		{Vowel: "u", Consonant: "t", Type: g.Type1Affix},
	})
	if got != "ratu" {
		t.Errorf("SlotV = %q, want \"ratu\"", got)
	}
}

func TestMoodOrScopeP1(t *testing.T) {
	cases := []struct {
		ms   g.MoodOrScope
		want string
	}{
		{g.MoodVal{Mood: g.FAC}, "h"},
		{g.MoodVal{Mood: g.SUB}, "hl"},
		{g.MoodVal{Mood: g.HYP}, "hň"},
		// CaseScope renders to the same Pattern-1 consonant as the
		// parallel Mood.
		{g.CaseScopeVal{CaseScope: g.CCN}, "h"},
		{g.CaseScopeVal{CaseScope: g.CCV}, "hň"},
	}
	for _, c := range cases {
		if got := MoodOrScopeP1(c.ms); got != c.want {
			t.Errorf("MoodOrScopeP1(%v) = %q, want %q", c.ms, got, c.want)
		}
	}
}

func TestMoodOrScopeP2(t *testing.T) {
	cases := []struct {
		ms   g.MoodOrScope
		want string
	}{
		{g.MoodVal{Mood: g.FAC}, "w"},
		{g.MoodVal{Mood: g.SUB}, "hw"},
		{g.MoodVal{Mood: g.HYP}, "hňw"},
		{g.CaseScopeVal{CaseScope: g.CCN}, "w"},
		{g.CaseScopeVal{CaseScope: g.CCV}, "hňw"},
	}
	for _, c := range cases {
		if got := MoodOrScopeP2(c.ms); got != c.want {
			t.Errorf("MoodOrScopeP2(%v) = %q, want %q", c.ms, got, c.want)
		}
	}
}
