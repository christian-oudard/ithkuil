package grammar

import "testing"

// TestMoodCaseScopePairing verifies the 1:1 mapping between Mood and
// CaseScope values used when the parser disambiguates a Pattern-1 Cn
// based on the formative's grammatical category.
func TestMoodCaseScopePairing(t *testing.T) {
	cases := []struct {
		m  Mood
		cs CaseScope
	}{
		{FAC, CCN}, {SUB, CCA}, {ASM, CCS},
		{SPC, CCQ}, {COU, CCP}, {HYP, CCV},
	}
	for _, c := range cases {
		if got := MoodToCaseScope(c.m); got != c.cs {
			t.Errorf("MoodToCaseScope(%v) = %v, want %v", c.m, got, c.cs)
		}
		if got := CaseScopeToMood(c.cs); got != c.m {
			t.Errorf("CaseScopeToMood(%v) = %v, want %v", c.cs, got, c.m)
		}
	}
}
