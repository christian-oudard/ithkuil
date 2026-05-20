package parse

import (
	"testing"

	"github.com/christian-oudard/ithkuil/grammar"
)

func TestParseVnValence(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.Valence
	}{
		{"a", grammar.MNO}, {"ä", grammar.PRL}, {"e", grammar.CRO},
		{"i", grammar.RCP}, {"ëi", grammar.CPL}, {"ö", grammar.DUP},
		{"o", grammar.DEM}, {"ü", grammar.CNG}, {"u", grammar.PTI},
	}
	for _, c := range cases {
		got, ok := ParseVnValence(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseVnValence(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestParseVnPhase(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.Phase
	}{
		{"ai", grammar.PCT}, {"au", grammar.ITR}, {"ei", grammar.REP},
		{"ui", grammar.FLC},
	}
	for _, c := range cases {
		got, ok := ParseVnPhase(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseVnPhase(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestParseVnEffect_Alternates(t *testing.T) {
	// Both canonical and alternate forms resolve to the same Effect.
	pairs := [][2]string{
		{"ia", "uä"}, {"ie", "uë"}, {"io", "üä"}, {"iö", "üë"},
		{"uö", "öë"}, {"uo", "öä"}, {"ue", "ië"}, {"ua", "iä"},
	}
	for _, p := range pairs {
		eCanon, ok1 := ParseVnEffect(p[0])
		eAlt, ok2 := ParseVnEffect(p[1])
		if !ok1 || !ok2 {
			t.Errorf("ParseVnEffect %q or %q failed", p[0], p[1])
			continue
		}
		if eCanon != eAlt {
			t.Errorf("canonical %q → %v, alternate %q → %v (should match)",
				p[0], eCanon, p[1], eAlt)
		}
	}
}

func TestParseVnLevel(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.Level
	}{
		{"ao", grammar.MIN}, {"aö", grammar.SBE}, {"eo", grammar.IFR},
		{"oa", grammar.MAX},
	}
	for _, c := range cases {
		got, ok := ParseVnLevel(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseVnLevel(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestParseVnAspect_Coverage(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.Aspect
	}{
		// Column 1
		{"a", grammar.RTR}, {"e", grammar.HAB}, {"u", grammar.ATP},
		// Column 2
		{"ai", grammar.RSM}, {"ou", grammar.CNT},
		// Column 3 canonical + alternate
		{"ia", grammar.PMP}, {"uä", grammar.PMP},
		{"ua", grammar.PPR}, {"iä", grammar.PPR},
		// Column 4
		{"ao", grammar.DCL}, {"oa", grammar.SQN},
	}
	for _, c := range cases {
		got, ok := ParseVnAspect(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseVnAspect(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestParseCnMood(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.Mood
	}{
		{"h", grammar.FAC}, {"hl", grammar.SUB}, {"hr", grammar.ASM},
		{"hm", grammar.SPC}, {"hn", grammar.COU}, {"hň", grammar.HYP},
	}
	for _, c := range cases {
		got, ok := ParseCnMood(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseCnMood(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestParseCnMoodP2(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.Mood
	}{
		{"w", grammar.FAC}, {"y", grammar.FAC},
		{"hw", grammar.SUB}, {"hrw", grammar.ASM},
		{"hmw", grammar.SPC}, {"hnw", grammar.COU}, {"hňw", grammar.HYP},
	}
	for _, c := range cases {
		got, ok := ParseCnMoodP2(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseCnMoodP2(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestParseCnCaseScope(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.CaseScope
	}{
		{"h", grammar.CCN}, {"w", grammar.CCN}, {"y", grammar.CCN},
		{"hl", grammar.CCA}, {"hr", grammar.CCS},
		{"hm", grammar.CCQ}, {"hn", grammar.CCP}, {"hň", grammar.CCV},
	}
	for _, c := range cases {
		got, ok := ParseCnCaseScope(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseCnCaseScope(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestIsValidCn(t *testing.T) {
	for _, c := range []string{"h", "hl", "hr", "hm", "hn", "hň",
		"w", "y", "hw", "hrw", "hmw", "hnw", "hňw"} {
		if !IsValidCn(c) {
			t.Errorf("IsValidCn(%q) = false, want true", c)
		}
	}
	for _, c := range []string{"", "x", "hp", "p", "n"} {
		if IsValidCn(c) {
			t.Errorf("IsValidCn(%q) = true, want false", c)
		}
	}
}

func TestIsPattern2Cn(t *testing.T) {
	p2 := []string{"w", "y", "hw", "hrw", "hmw", "hnw", "hňw"}
	p1 := []string{"h", "hl", "hr", "hm", "hn", "hň"}
	for _, c := range p2 {
		if !IsPattern2Cn(c) {
			t.Errorf("IsPattern2Cn(%q) = false, want true", c)
		}
	}
	for _, c := range p1 {
		if IsPattern2Cn(c) {
			t.Errorf("IsPattern2Cn(%q) = true, want false (it's Pattern 1)", c)
		}
	}
}

func TestSlotVIIIEnumCounts(t *testing.T) {
	if n := len(grammar.AllValences); n != 9 {
		t.Errorf("AllValences = %d, want 9", n)
	}
	if n := len(grammar.AllPhases); n != 9 {
		t.Errorf("AllPhases = %d, want 9", n)
	}
	if n := len(grammar.AllEffects); n != 9 {
		t.Errorf("AllEffects = %d, want 9", n)
	}
	if n := len(grammar.AllLevels); n != 9 {
		t.Errorf("AllLevels = %d, want 9", n)
	}
	if n := len(grammar.AllAspects); n != 36 {
		t.Errorf("AllAspects = %d, want 36", n)
	}
	if n := len(grammar.AllMoods); n != 6 {
		t.Errorf("AllMoods = %d, want 6", n)
	}
	if n := len(grammar.AllCaseScopes); n != 6 {
		t.Errorf("AllCaseScopes = %d, want 6", n)
	}
}

func TestMoodCaseScopeRoundTrip(t *testing.T) {
	for _, m := range grammar.AllMoods {
		if got := grammar.CaseScopeToMood(grammar.MoodToCaseScope(m)); got != m {
			t.Errorf("Mood %v → CaseScope → Mood = %v", m, got)
		}
	}
	for _, c := range grammar.AllCaseScopes {
		if got := grammar.MoodToCaseScope(grammar.CaseScopeToMood(c)); got != c {
			t.Errorf("CaseScope %v → Mood → CaseScope = %v", c, got)
		}
	}
}
