package semantics

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
)

func TestMoodOrCaseScope(t *testing.T) {
	tests := []struct {
		mood     g.Mood
		isVerbal bool
		want     string
	}{
		{g.FAC, true, "FAC"},
		{g.FAC, false, "CCN"},
		{g.HYP, true, "HYP"},
		{g.HYP, false, "CCV"},
		{g.SUB, true, "SUB"},
		{g.SUB, false, "CCA"},
	}
	for _, tt := range tests {
		got := MoodOrCaseScope(tt.mood, tt.isVerbal)
		if got != tt.want {
			t.Errorf("MoodOrCaseScope(%v,%v) = %q, want %q",
				tt.mood, tt.isVerbal, got, tt.want)
		}
	}
}

func TestSlotVIIICnLabel(t *testing.T) {
	// nil → ""
	if got := SlotVIIICnLabel(nil, g.UnframedNominal{Case: g.THM}); got != "" {
		t.Errorf("SlotVIIICnLabel(nil) = %q, want \"\"", got)
	}
	// Verbal final → Mood label
	s := g.VnCnValence{Valence: g.PRL, MoodScope: g.SUB}
	got := SlotVIIICnLabel(s, g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}})
	if got != "SUB" {
		t.Errorf("verbal final SlotVIIICnLabel = %q, want \"SUB\"", got)
	}
	// Nominal final → CaseScope
	got = SlotVIIICnLabel(s, g.UnframedNominal{Case: g.THM})
	if got != "CCA" {
		t.Errorf("nominal final SlotVIIICnLabel = %q, want \"CCA\"", got)
	}
	// Framed-verbal final → CaseScope (framed counts as nominal for Cn)
	got = SlotVIIICnLabel(s, g.FramedVerbal{Case: g.THM})
	if got != "CCA" {
		t.Errorf("framed-verbal SlotVIIICnLabel = %q, want \"CCA\"", got)
	}
}

func TestIsVH(t *testing.T) {
	cases := []struct {
		stress    phonology.Stress
		pairCount int
		want      bool
	}{
		{phonology.Ultimate, 1, true},
		{phonology.Ultimate, 3, true},
		{phonology.Ultimate, 0, false}, // no pairs → V_N
		{phonology.Penultimate, 1, false},
		{phonology.Monosyllabic, 0, false},
	}
	for _, c := range cases {
		if got := IsVH(c.stress, c.pairCount); got != c.want {
			t.Errorf("IsVH(%v,%d) = %v, want %v", c.stress, c.pairCount, got, c.want)
		}
	}
}

func TestModularIsVerbal(t *testing.T) {
	yes, no := true, false
	// marksMood wins when set
	if !ModularIsVerbal(g.VnCnAspect{Aspect: g.PRG}, &yes) {
		t.Error("marksMood=true should win even over Aspect Vn")
	}
	if ModularIsVerbal(g.VnCnValence{Valence: g.MNO}, &no) {
		t.Error("marksMood=false should win even over Pattern-1 Vn")
	}
	// Fallback: Pattern-1 Vn → verbal
	if !ModularIsVerbal(g.VnCnValence{Valence: g.PRL}, nil) {
		t.Error("Pattern-1 Vn fallback should be verbal")
	}
	// Fallback: Aspect Vn → nominal (CaseScope)
	if ModularIsVerbal(g.VnCnAspect{Aspect: g.HAB}, nil) {
		t.Error("Aspect Vn fallback should be nominal")
	}
	// nil SlotVIII: with no Vn pattern info, treat as verbal (Mood is the
	// pattern-1 default).
	if !ModularIsVerbal(nil, nil) {
		t.Error("nil SlotVIII fallback should default to verbal")
	}
}

func TestVnCategory(t *testing.T) {
	cases := []struct {
		vn, cn, want string
	}{
		{"a", "", "RTR"},    // no Cn → Aspect
		{"e", "hňw", "HAB"}, // Pattern-2 Cn → Aspect
		{"ä", "hl", "PRL"},  // Pattern-1 Cn → Valence
		{"i", "n", "PRG"},   // Cm "n" → Aspect
		{"u", "ň", "PTI"},   // Cm "ň" → Pattern-1 (Valence first)
	}
	for _, c := range cases {
		got := VnCategory(c.vn, c.cn)
		if got != c.want {
			t.Errorf("VnCategory(%q,%q) = %q, want %q", c.vn, c.cn, got, c.want)
		}
	}
}

func TestCnLabel(t *testing.T) {
	if got := CnLabel("hl", true); got != "SUB" {
		t.Errorf("CnLabel(hl,Mood) = %q, want \"SUB\"", got)
	}
	if got := CnLabel("hl", false); got != "CCA" {
		t.Errorf("CnLabel(hl,CaseScope) = %q, want \"CCA\"", got)
	}
	if got := CnLabel("hňw", true); got != "HYP" {
		t.Errorf("CnLabel(hňw,Mood) = %q, want \"HYP\"", got)
	}
	// A §4.3 Slot 3 C_M is not a C_N and has no Mood reading, so it
	// belongs to CmLabel; CnLabel used to answer for it too, which is
	// the conflation that left Slot 3's n without a segment.
	if got := CnLabel("n", true); got != "Cn?" {
		t.Errorf("CnLabel(n) = %q, want %q", got, "Cn?")
	}
	if got := CmLabel("n"); got != "CmAspect" {
		t.Errorf("CmLabel(n) = %q, want \"CmAspect\"", got)
	}
	if got := CmLabel("ň"); got != "CmOther" {
		t.Errorf("CmLabel(ň) = %q, want \"CmOther\"", got)
	}
	if got := CmLabel("zzz"); got != "Cm?" {
		t.Errorf("CmLabel(zzz) = %q, want \"Cm?\"", got)
	}
}

func TestVhCode(t *testing.T) {
	cases := map[string]string{
		"a": "→Case/Mood/Val/Illoc",
		"e": "→Case/Mood",
		"i": "→formative",
		"u": "→formative",
		"o": "→formative+adjuncts",
		"á": "→Case/Mood/Val/Illoc", // stripped of stress mark
	}
	for in, want := range cases {
		if got := VhCode(in); got != want {
			t.Errorf("VhCode(%q) = %q, want %q", in, got, want)
		}
	}
}

func TestPrefixCode(t *testing.T) {
	if got := PrefixCode("w"); got != "→parent" {
		t.Errorf("PrefixCode(w) = %q", got)
	}
	if got := PrefixCode("y"); got != "→concat" {
		t.Errorf("PrefixCode(y) = %q", got)
	}
	if PrefixCode("z") != "z" {
		t.Error("PrefixCode echoes unknown input")
	}
}

func TestVhMeaning(t *testing.T) {
	for _, v := range []string{"a", "e", "i", "u", "o"} {
		if VhMeaning(v) == "" {
			t.Errorf("VhMeaning(%q) empty", v)
		}
	}
	// Unknown vowel: prose still non-empty, includes the input.
	got := VhMeaning("x")
	if got == "" {
		t.Error("VhMeaning(x) empty for unrecognized input")
	}
}

func TestPrefixMeaning(t *testing.T) {
	if PrefixMeaning("w") == "" || PrefixMeaning("y") == "" {
		t.Error("PrefixMeaning empty for w/y")
	}
	if PrefixMeaning("z") != "" {
		t.Error("PrefixMeaning should be empty for unknown prefix")
	}
}

func TestCmName(t *testing.T) {
	if got := CmName("CmAspect"); got != "Cm (n)" {
		t.Errorf("CmName(CmAspect) = %q", got)
	}
	if got := CmName("CmOther"); got != "Cm (ň)" {
		t.Errorf("CmName(CmOther) = %q", got)
	}
	if CmName("unknown") != "" {
		t.Error("CmName(unknown) should be empty")
	}
}

func TestCmMeaning(t *testing.T) {
	if CmMeaning("CmAspect") == "" || CmMeaning("CmOther") == "" {
		t.Error("CmMeaning empty for known marker")
	}
	if CmMeaning("unknown") != "" {
		t.Error("CmMeaning(unknown) should be empty")
	}
}

func TestCnLabel_FailureCases(t *testing.T) {
	// Invalid Cn returns the sentinel.
	if got := CnLabel("zzz", true); got != "Cn?" {
		t.Errorf("CnLabel(zzz) = %q, want Cn?", got)
	}
}

func TestVnCategory_AllVnCategories(t *testing.T) {
	// Exercise each fallback branch in VnCategory.
	cases := []struct {
		vn, cn, want string
	}{
		// Aspect when no Cn or Pattern-2 Cn.
		{"a", "w", "RTR"}, // Aspect series 1
		// Pattern-1 fallback chain for Vn series.
		{"a", "hl", "MNO"},  // Valence (series 1)
		{"ai", "h", "PCT"},  // Phase (series 2)
		{"ia", "h", "BEN1"}, // Effect (series 3)
		{"ao", "h", "MIN"},  // Level (series 4)
		// Unknown Vn but valid Pattern-1 Cn → final fallback.
		{"zz", "h", "Vn?"},
	}
	for _, c := range cases {
		got := VnCategory(c.vn, c.cn)
		if got != c.want {
			t.Errorf("VnCategory(%q, %q) = %q, want %q", c.vn, c.cn, got, c.want)
		}
	}
}
