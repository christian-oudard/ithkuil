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
		// Initial "gm" is preserved (UPX + ICP + N_/RPV produces "gm").
		{"gm", "gm"},
		// Non-initial "gm" becomes "x".
		{"ngm", "nx"},
		// Initial "bm" preserved; non-initial "bm" → "v" then second-pass "fv" → "vw".
		{"bm", "bm"},
		{"fbm", "vw"},
		// "bn" non-initial → "ḑ"; tḑ second-pass → ḑy.
		// tbn is the §2.2 case; ERRATA.md §3.6 gives it ḑw.
		{"tbn", "ḑw"},
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
		// UPX + N_/ICP/RPV → "gm" initial, no substitution.
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.N_, Extension: g.ICP, Essence: g.RPV}, "gm"},
		// MSC + CSL + N_/GRA/RPV → raw "kgm" → "nx".
		{g.SlotVI{Configuration: g.MSC, Affiliation: g.CSL, Perspective: g.N_, Extension: g.GRA, Essence: g.RPV}, "nx"},
	}
	for _, c := range cases {
		if got := ConstructCa(c.s); got != c.want {
			t.Errorf("ConstructCa(%v) = %q, want %q", c.s, got, c.want)
		}
	}
}

// The §3.6 bn-substitution family, completed. Two of these are
// Quijada's and the third is ours; ERRATA.md §3.6 has the argument.
//
// Sweeping every Ca whose raw composition ends in bm or bn, exactly
// three shapes come out unsayable under the general [C]bm → [C]v and
// [C]bn → [C]ḑ rules. §3.6 escapes two of them — the two barred by
// §2.5 — and says nothing about the third, which §2.2 bars instead.
func TestSubstitutions_BnFamily(t *testing.T) {
	cases := []struct{ raw, want, why string }{
		{"fbm", "vw", "§2.5 fv, his rule"},
		{"ţbn", "ḑy", "§2.5 ţḑ, his rule with its intermediate corrected"},
		{"tbn", "ḑw", "§2.2 tḑ, ours"},
		// The liquid Affiliation prefixes ride along untouched.
		{"lţbn", "lḑy", "ASO"},
		{"rtbn", "rḑw", "COA"},
		{"řfbm", "řvw", "VAR"},
		// Neighbours that must not be caught by the new rule.
		{"tbm", "tv", "bm after a plain stop is fine"},
		{"ţbm", "ţv", "likewise"},
		{"kbn", "kḑ", "kḑ is legal, so no escape applies"},
	}
	for _, c := range cases {
		if got := ApplySubstitutions(c.raw); got != c.want {
			t.Errorf("ApplySubstitutions(%q) = %q, want %q (%s)",
				c.raw, got, c.want, c.why)
		}
	}
}
