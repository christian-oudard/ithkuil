package grammar

import "testing"

// Direct unit tests for the small helpers the rest of the suite exercises only
// indirectly. Boring per-line coverage but a fast regression catcher if any of
// these tables drift.

func TestVsScope_AllVowels(t *testing.T) {
	cases := []struct {
		vs   string
		want AffixScope
	}{
		{"", ScopeVDom}, {"a", ScopeVDom},
		{"u", ScopeVSub},
		{"e", ScopeVIIDom},
		{"i", ScopeVIISub},
		{"o", ScopeFormative},
		{"ö", ScopeAdjacent},
	}
	for _, c := range cases {
		got, ok := VsScope(c.vs)
		if !ok || got != c.want {
			t.Errorf("VsScope(%q) = (%v,%v), want (%v,true)", c.vs, got, ok, c.want)
		}
	}
	if _, ok := VsScope("xx"); ok {
		t.Error("VsScope(xx) returned ok=true")
	}
}

func TestCzScope_AllForms(t *testing.T) {
	cases := []struct {
		cz   string
		want AffixScope
	}{
		{"h", ScopeVDom},
		{"'h", ScopeVSub},
		{"'hl", ScopeVIIDom},
		{"'hr", ScopeVIISub},
		{"hw", ScopeFormative},
		{"'hw", ScopeAdjacent},
	}
	for _, c := range cases {
		got, ok := CzScope(c.cz)
		if !ok || got != c.want {
			t.Errorf("CzScope(%q) = (%v,%v), want (%v,true)", c.cz, got, ok, c.want)
		}
	}
	if _, ok := CzScope("x"); ok {
		t.Error("CzScope(x) returned ok=true")
	}
}

func TestVzScope_AllVowels(t *testing.T) {
	cases := []struct {
		vz   string
		want AffixScope
	}{
		{"a", ScopeVDom},
		{"u", ScopeVSub},
		{"e", ScopeVIIDom},
		{"i", ScopeVIISub},
		{"o", ScopeFormative},
		{"ö", ScopeAdjacent},
	}
	for _, c := range cases {
		got, ok := VzScope(c.vz)
		if !ok || got != c.want {
			t.Errorf("VzScope(%q) = (%v,%v), want (%v,true)", c.vz, got, ok, c.want)
		}
	}
	if _, ok := VzScope("ai"); ok {
		t.Error("VzScope(ai) returned ok=true; ai means 'same as Cz' and shouldn't decode here")
	}
}

func TestIsVerbal_Variants(t *testing.T) {
	cases := []struct {
		f    Final
		want bool
	}{
		{UnframedVerbal{Vk: Assertive{Validation: OBS}}, true},
		{UnframedNominal{Case: THM}, false},
		{FramedVerbal{Case: THM}, false},
	}
	for _, c := range cases {
		if got := IsVerbal(c.f); got != c.want {
			t.Errorf("IsVerbal(%T) = %v, want %v", c.f, got, c.want)
		}
	}
}

func TestNames_RoundTripCanonicalAbbrevs(t *testing.T) {
	for _, abbrev := range []string{"THM", "ERG", "DAT", "S1", "PRC", "CPT",
		"STA", "DYN", "BSC", "EXS", "UNI", "CSL", "M",
		"DEL", "NRM", "FAC", "CCN", "ASR", "OBS",
	} {
		if n := Name(abbrev); n == "" {
			t.Errorf("Name(%q) empty", abbrev)
		}
	}
}

func TestMeaning_NonEmptyForCommonAbbrevs(t *testing.T) {
	// Most categories have a short meaning entry; a representative
	// handful of those should be non-empty.
	common := []string{"S1", "PRC", "CPT", "STA", "DYN", "ASR", "OBS"}
	any := false
	for _, a := range common {
		if Meaning(a) != "" {
			any = true
			break
		}
	}
	if !any {
		t.Error("Meaning returned empty for every common abbrev — table maybe lost its entries")
	}
}

func TestAllNamed_NonEmpty(t *testing.T) {
	m := AllNamed()
	if len(m) < 50 {
		t.Errorf("AllNamed returned %d entries, want at least 50", len(m))
	}
	if m["THM"] == "" {
		t.Error("AllNamed missing THM")
	}
}

func TestCase_StringerExhaustive(t *testing.T) {
	for _, c := range AllCases {
		if c.String() == "" {
			t.Errorf("Case(%d).String() empty", c)
		}
	}
}

func TestRoot_FinalMarkers_Compile(t *testing.T) {
	// Each variant must implement the sealed interface. These calls go
	// through Tag() which is uncovered otherwise.
	var f Final
	f = UnframedNominal{Case: THM}
	_ = f.Tag()
	f = FramedVerbal{Case: THM}
	_ = f.Tag()
	f = UnframedVerbal{Vk: Assertive{Validation: OBS}}
	_ = f.Tag()
}

func TestSlotVIIIMoodScope_AllVariants(t *testing.T) {
	cases := []SlotVIII{
		VnCnValence{Valence: MNO, MoodScope: SUB},
		VnCnPhase{Phase: PCT, MoodScope: ASM},
		VnCnEffect{Effect: BEN1, MoodScope: SPC},
		VnCnLevel{Level: MIN, MoodScope: COU},
		VnCnAspect{Aspect: RTR, MoodScope: HYP},
	}
	wants := []Mood{SUB, ASM, SPC, COU, HYP}
	for i, c := range cases {
		if got := SlotVIIIMoodScope(c); got != wants[i] {
			t.Errorf("SlotVIIIMoodScope(%T) = %v, want %v", c, got, wants[i])
		}
	}
	if got := SlotVIIIMoodScope(nil); got != FAC {
		t.Errorf("SlotVIIIMoodScope(nil) = %v, want FAC", got)
	}
}

func TestSlotVIIIVnLabel_AllVariants(t *testing.T) {
	cases := []SlotVIII{
		VnCnValence{Valence: MNO},
		VnCnPhase{Phase: PCT},
		VnCnEffect{Effect: BEN1},
		VnCnLevel{Level: MIN},
		VnCnAspect{Aspect: RTR},
	}
	for _, c := range cases {
		if got := SlotVIIIVnLabel(c); got == "" {
			t.Errorf("SlotVIIIVnLabel(%T) empty", c)
		}
	}
	if got := SlotVIIIVnLabel(nil); got != "" {
		t.Errorf("SlotVIIIVnLabel(nil) = %q, want \"\"", got)
	}
}

func TestVk_AllTags(t *testing.T) {
	// Every Vk implementation has a Tag() method that should return its
	// 3-letter abbreviation. Walking AllVk covers them all.
	for _, v := range AllVk {
		if v.Tag() == "" {
			t.Errorf("Vk(%T).Tag() empty", v)
		}
	}
}

func TestCaseGroup_String(t *testing.T) {
	cases := []CaseGroup{
		Transrelative, Appositive, Associative, Adverbial,
		Relational, Affinitive, SpatioTemporal1, SpatioTemporal2,
	}
	for _, g := range cases {
		if g.String() == "" {
			t.Errorf("CaseGroup(%d).String() empty", g)
		}
	}
}
