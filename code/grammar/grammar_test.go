package grammar

import "testing"

func TestSlotIVToVr(t *testing.T) {
	cases := []struct {
		in   SlotIV
		want string
	}{
		{SlotIV{STA, BSC, EXS}, "a"},
		{SlotIV{STA, CTE, EXS}, "ä"},
		{SlotIV{STA, CSV, EXS}, "e"},
		{SlotIV{STA, OBJ, EXS}, "i"},
		{SlotIV{STA, BSC, FNC}, "ai"},
		{SlotIV{STA, BSC, RPS}, "ia"},
		{SlotIV{DYN, BSC, EXS}, "u"},
		{SlotIV{DYN, CTE, EXS}, "ü"},
		{SlotIV{DYN, CSV, EXS}, "o"},
		{SlotIV{DYN, OBJ, EXS}, "ö"},
		{SlotIV{DYN, BSC, AMG}, "oa"},
	}
	for _, c := range cases {
		if got := SlotIVToVr(c.in); got != c.want {
			t.Errorf("SlotIVToVr(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestSlotIIToVv(t *testing.T) {
	cases := []struct {
		in   SlotII
		want string
	}{
		{SlotII{S1, PRC}, "a"},
		{SlotII{S1, CPT}, "ä"},
		{SlotII{S2, PRC}, "e"},
		{SlotII{S2, CPT}, "i"},
		{SlotII{S3, PRC}, "u"},
		{SlotII{S3, CPT}, "ü"},
		{SlotII{S0, PRC}, "o"},
		{SlotII{S0, CPT}, "ö"},
	}
	for _, c := range cases {
		if got := SlotIIToVv(c.in); got != c.want {
			t.Errorf("SlotIIToVv(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}

// TestSlotIIToVv_Panic confirms the unreachable guard fires if a caller
// builds an out-of-range SlotII. There is no valid path that reaches
// this line; the test exists purely to keep the panic from rotting.
func TestSlotIIToVv_Panic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("SlotIIToVv with bogus enum should have panicked")
		}
	}()
	SlotIIToVv(SlotII{Stem: 99, Version: 99})
}

// TestSlotIVToVr_Panic is the same guard for SlotIV.
func TestSlotIVToVr_Panic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("SlotIVToVr with bogus enum should have panicked")
		}
	}()
	SlotIVToVr(SlotIV{Function: 99, Specification: 99, Context: 99})
}

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
