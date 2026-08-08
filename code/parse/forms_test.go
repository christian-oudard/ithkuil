package parse

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// These cover the written form of a grammatical value, and moved here
// with the tables they test.

func TestBiasFormsUnique(t *testing.T) {
	seen := map[string]g.Bias{}
	for _, b := range g.AllBiases {
		f := BiasForm(b)
		if f == "" {
			t.Errorf("%s has empty form", b)
			continue
		}
		if other, dup := seen[f]; dup {
			t.Errorf("form %q duplicated: %s and %s", f, other, b)
		}
		seen[f] = b
	}
}

func TestBiasFormSpotCheck(t *testing.T) {
	cases := []struct {
		b    g.Bias
		want string
	}{
		{g.DOL, "řřx"},
		{g.DIS, "kff"},
		{g.ACC, "lf"},
		{g.DLC, "ẓmm"},
		{g.MNF, "pss"},
		{g.RSG, "msf"},
		{g.ARB, "xtļ"},
		{g.ADS, "lļ"},
	}
	for _, c := range cases {
		if got := BiasForm(c.b); got != c.want {
			t.Errorf("BiasForm(%s) = %q, want %q", c.b, got, c.want)
		}
	}
}

func TestSlotIVToVr(t *testing.T) {
	cases := []struct {
		in   g.SlotIV
		want string
	}{
		{g.SlotIV{Function: g.STA, Specification: g.BSC, Context: g.EXS}, "a"},
		{g.SlotIV{Function: g.STA, Specification: g.CTE, Context: g.EXS}, "ä"},
		{g.SlotIV{Function: g.STA, Specification: g.CSV, Context: g.EXS}, "e"},
		{g.SlotIV{Function: g.STA, Specification: g.OBJ, Context: g.EXS}, "i"},
		{g.SlotIV{Function: g.STA, Specification: g.BSC, Context: g.FNC}, "ai"},
		{g.SlotIV{Function: g.STA, Specification: g.BSC, Context: g.RPS}, "ia"},
		{g.SlotIV{Function: g.DYN, Specification: g.BSC, Context: g.EXS}, "u"},
		{g.SlotIV{Function: g.DYN, Specification: g.CTE, Context: g.EXS}, "ü"},
		{g.SlotIV{Function: g.DYN, Specification: g.CSV, Context: g.EXS}, "o"},
		{g.SlotIV{Function: g.DYN, Specification: g.OBJ, Context: g.EXS}, "ö"},
		{g.SlotIV{Function: g.DYN, Specification: g.BSC, Context: g.AMG}, "oa"},
	}
	for _, c := range cases {
		if got := SlotIVToVr(c.in); got != c.want {
			t.Errorf("SlotIVToVr(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestSlotIIToVv(t *testing.T) {
	cases := []struct {
		in   g.SlotII
		want string
	}{
		{g.SlotII{Stem: g.S1, Version: g.PRC}, "a"},
		{g.SlotII{Stem: g.S1, Version: g.CPT}, "ä"},
		{g.SlotII{Stem: g.S2, Version: g.PRC}, "e"},
		{g.SlotII{Stem: g.S2, Version: g.CPT}, "i"},
		{g.SlotII{Stem: g.S3, Version: g.PRC}, "u"},
		{g.SlotII{Stem: g.S3, Version: g.CPT}, "ü"},
		{g.SlotII{Stem: g.S0, Version: g.PRC}, "o"},
		{g.SlotII{Stem: g.S0, Version: g.CPT}, "ö"},
	}
	for _, c := range cases {
		if got := SlotIIToVv(c.in); got != c.want {
			t.Errorf("SlotIIToVv(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}

// TestSlotIIToVv_Panic confirms the unreachable guard fires if a caller
// builds an out-of-range g.SlotII. There is no valid path that reaches
// this line; the test exists purely to keep the panic from rotting.
func TestSlotIIToVv_Panic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("SlotIIToVv with bogus enum should have panicked")
		}
	}()
	SlotIIToVv(g.SlotII{Stem: 99, Version: 99})
}

// TestSlotIVToVr_Panic is the same guard for g.SlotIV.
func TestSlotIVToVr_Panic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("SlotIVToVr with bogus enum should have panicked")
		}
	}()
	SlotIVToVr(g.SlotIV{Function: 99, Specification: 99, Context: 99})
}

func TestRegisterInitialForms(t *testing.T) {
	cases := []struct {
		r    g.Register
		want string
	}{
		{g.NRR, ""},
		{g.DSV, "ha"},
		{g.PNT, "he"},
		{g.SPF, "hi"},
		{g.EXM, "ho"},
		{g.CGT, "hu"},
		{g.END, ""},
	}
	for _, c := range cases {
		if got := RegisterInitialForm(c.r); got != c.want {
			t.Errorf("RegisterInitialForm(%s) = %q, want %q", c.r, got, c.want)
		}
	}
}

func TestRegisterFinalForms(t *testing.T) {
	cases := []struct {
		r    g.Register
		want string
	}{
		{g.NRR, ""},
		{g.DSV, "hai"},
		{g.PNT, "hei"},
		{g.SPF, "hiu"},
		{g.EXM, "hoi"},
		{g.CGT, "hui"},
		{g.END, "hü"},
	}
	for _, c := range cases {
		if got := RegisterFinalForm(c.r); got != c.want {
			t.Errorf("RegisterFinalForm(%s) = %q, want %q", c.r, got, c.want)
		}
	}
}

func TestCaseToVcExhaustive(t *testing.T) {
	seen := map[string]g.Case{}
	for _, c := range g.AllCases {
		v := CaseToVc(c)
		if v == "" {
			t.Errorf("CaseToVc(%s) returned empty", c)
			continue
		}
		if other, dup := seen[v]; dup {
			t.Errorf("vowel %q maps to both %s and %s", v, other, c)
		}
		seen[v] = c
	}
}

// TestCaseToVc_Panic pins the stop. The table was a map, which answered
// "" for a Case outside the enum, and an empty Vc is not a failure: the
// word renders without Slot IX and reads back as THM.
func TestCaseToVc_Panic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("CaseToVc with a bogus case should have panicked")
		}
	}()
	CaseToVc(g.Case(len(g.AllCases)))
}

func TestCaseToVcSpotChecks(t *testing.T) {
	cases := []struct {
		c    g.Case
		want string
	}{
		{g.THM, "a"},
		{g.IND, "u"},
		{g.POS, "ai"},
		{g.APL, "ia"},
		{g.FUN, "ao"},
		{g.PRN, "a'a"},
		{g.ACT, "a'i"},
		{g.LOC, "i'a"},
		{g.CNR, "a'o"},
	}
	for _, c := range cases {
		if got := CaseToVc(c.c); got != c.want {
			t.Errorf("CaseToVc(%s) = %q, want %q", c.c, got, c.want)
		}
	}
}

func TestCarrierTypeForms(t *testing.T) {
	// Each g.CarrierType has a unique 2-char form.
	seen := map[string]bool{}
	for _, c := range g.AllCarrierTypes {
		f := CarrierTypeForm(c)
		if f == "" {
			t.Errorf("%s has empty form", c)
		}
		if seen[f] {
			t.Errorf("duplicate carrier form %q", f)
		}
		seen[f] = true
	}
}
