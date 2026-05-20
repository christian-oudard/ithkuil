package allomorph

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

func TestApplySubstitutions_Simple(t *testing.T) {
	cases := []struct {
		in, want string
	}{
		{"tt", "nt"},
		{"kk", "nk"},
		{"pp", "mp"},
		{"pb", "mb"},
		{"kg", "ng"},
		{"ll", "pļ"},
		{"rr", "ns"},
		{"çy", "nd"},
		{"řř", "ňš"},
		{"rř", "nš"},
		{"řr", "ňs"},
	}
	for _, c := range cases {
		if got := ApplySubstitutions(c.in); got != c.want {
			t.Errorf("ApplySubstitutions(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestApplySubstitutions_ContextNonInitial(t *testing.T) {
	cases := []struct {
		in, want string
	}{
		// Initial "gm" is preserved (UNI + ICP + N_/RPV produces "gm").
		{"gm", "gm"},
		// Non-initial "gm" becomes "x".
		{"ngm", "nx"},
		// Initial "bm" preserved; non-initial "bm" → "v" then second-pass "fv" → "vw".
		{"bm", "bm"},
		{"fbm", "vw"},
		// "bn" non-initial → "ḑ"; tḑ second-pass → ḑy.
		{"tbn", "ḑy"},
		// "gn" non-initial → "ň".
		{"tgn", "tň"},
		// "çx" non-initial → "xw".
		{"sçx", "sxw"},
	}
	for _, c := range cases {
		if got := ApplySubstitutions(c.in); got != c.want {
			t.Errorf("ApplySubstitutions(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestApplySubstitutions_Chained(t *testing.T) {
	// "kgm" → simple kg→ng → "ngm" → context gm→x non-initial → "nx".
	if got := ApplySubstitutions("kgm"); got != "nx" {
		t.Errorf("ApplySubstitutions(\"kgm\") = %q, want %q", got, "nx")
	}
}

func TestConstructCa_RoundsTrip(t *testing.T) {
	cases := []struct {
		s    g.SlotVI
		want string
	}{
		// Default: raw "l", no substitution.
		{g.DefaultSlotVI, "l"},
		// Configuration with geminate stop: MSS = "t", PRX = "t" → "tt" → "nt".
		{g.SlotVI{Configuration: g.MSS, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}, "nt"},
		{g.SlotVI{Configuration: g.MSC, Affiliation: g.CSL, Perspective: g.M_, Extension: g.ICP, Essence: g.NRM}, "nk"},
		{g.SlotVI{Configuration: g.MSF, Affiliation: g.CSL, Perspective: g.M_, Extension: g.ATV, Essence: g.NRM}, "mp"},
		{g.SlotVI{Configuration: g.MSF, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DPL, Essence: g.NRM}, "mb"},
		{g.SlotVI{Configuration: g.MSC, Affiliation: g.CSL, Perspective: g.M_, Extension: g.GRA, Essence: g.NRM}, "ng"},
		// MDF + CSL + A_ + DEL + NRM → "ç" + "y" → çy → nd
		{g.SlotVI{Configuration: g.MDF, Affiliation: g.CSL, Perspective: g.A_, Extension: g.DEL, Essence: g.NRM}, "nd"},
		// UNI + N_/ICP/RPV → "gm" initial, no substitution.
		{g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.N_, Extension: g.ICP, Essence: g.RPV}, "gm"},
		// MSC + CSL + N_/GRA/RPV → raw "kgm" → "nx".
		{g.SlotVI{Configuration: g.MSC, Affiliation: g.CSL, Perspective: g.N_, Extension: g.GRA, Essence: g.RPV}, "nx"},
	}
	for _, c := range cases {
		if got := ConstructCa(c.s); got != c.want {
			t.Errorf("ConstructCa(%v) = %q, want %q", c.s, got, c.want)
		}
	}
}
