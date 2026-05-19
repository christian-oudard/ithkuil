package grammar

import "testing"

func TestSlotIVToVr(t *testing.T) {
	cases := []struct {
		in   SlotIV
		want string
	}{
		{SlotIV{STA, BSC, EXS}, "a"},
		{SlotIV{STA, CTE, EXS}, "ä"},
		{SlotIV{STA, BSC, FNC}, "ai"},
		{SlotIV{STA, BSC, RPS}, "ia"},
		{SlotIV{DYN, BSC, EXS}, "u"},
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
