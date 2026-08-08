package allomorph

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// TestCa4_Panic pins the stop. ca4 answered the zero entry — two empty
// strings — for a pair outside §3.6's table, so the Ca lost its
// Perspective and composed to the cluster of a different SlotVI. That
// is the shape of the UNIPLEX Affiliation collapse, and a round trip
// cannot see it: the surviving member of the pair reads back fine.
func TestCa4_Panic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("ca4 with a bogus perspective should have panicked")
		}
	}()
	ca4(g.Perspective(len(g.AllPerspectives)), g.NRM)
}

func TestConstructCaRaw_Defaults(t *testing.T) {
	if got := ConstructCaRaw(g.DefaultSlotVI); got != "l" {
		t.Errorf("default Ca = %q, want %q", got, "l")
	}
}

func TestConstructCaRaw_StandalonePerspectives(t *testing.T) {
	cases := []struct {
		s    g.SlotVI
		want string
	}{
		// UPX/CSL/DEL with each perspective × essence
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}, "l"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}, "r"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.N_, Extension: g.DEL, Essence: g.NRM}, "v"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.A_, Extension: g.DEL, Essence: g.NRM}, "j"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.RPV}, "tļ"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.RPV}, "ř"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.N_, Extension: g.DEL, Essence: g.RPV}, "m"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.A_, Extension: g.DEL, Essence: g.RPV}, "n"},
	}
	for _, c := range cases {
		if got := ConstructCaRaw(c.s); got != c.want {
			t.Errorf("ConstructCaRaw(%v) = %q, want %q", c.s, got, c.want)
		}
	}
}

func TestConstructCaRaw_UniWithExtension(t *testing.T) {
	// UPX + Extension uses voiced standalone forms (d/g/b/gz/bz) + persp suffix.
	cases := []struct {
		s    g.SlotVI
		want string
	}{
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}, "d"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.ICP, Essence: g.NRM}, "g"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.ATV, Essence: g.NRM}, "b"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.GRA, Essence: g.NRM}, "gz"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DPL, Essence: g.NRM}, "bz"},
	}
	for _, c := range cases {
		if got := ConstructCaRaw(c.s); got != c.want {
			t.Errorf("ConstructCaRaw(%v) = %q, want %q", c.s, got, c.want)
		}
	}
}

func TestConstructCaRaw_UniWithAffiliation(t *testing.T) {
	// UPX + Affiliation alone (M_/NRM) uses long Affiliation form.
	cases := []struct {
		s    g.SlotVI
		want string
	}{
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.ASO, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}, "nļ"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.COA, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}, "rļ"},
		{g.SlotVI{Configuration: g.UPX, Affiliation: g.VAR, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}, "ň"},
	}
	for _, c := range cases {
		if got := ConstructCaRaw(c.s); got != c.want {
			t.Errorf("ConstructCaRaw(%v) = %q, want %q", c.s, got, c.want)
		}
	}
}

func TestConstructCaRaw_AffiliationPlusConfig(t *testing.T) {
	// Affiliation prefix + Configuration consonant (DEL/M_/NRM).
	cases := []struct {
		s    g.SlotVI
		want string
	}{
		{g.SlotVI{Configuration: g.DPX, Affiliation: g.ASO, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}, "ls"},
		{g.SlotVI{Configuration: g.DPX, Affiliation: g.COA, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}, "rs"},
		{g.SlotVI{Configuration: g.MDS, Affiliation: g.ASO, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}, "lţ"},
		{g.SlotVI{Configuration: g.MDS, Affiliation: g.COA, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}, "rţ"},
		{g.SlotVI{Configuration: g.MDS, Affiliation: g.VAR, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}, "řţ"},
	}
	for _, c := range cases {
		if got := ConstructCaRaw(c.s); got != c.want {
			t.Errorf("ConstructCaRaw(%v) = %q, want %q", c.s, got, c.want)
		}
	}
}

func TestConstructCaRaw_ConfigPlusPerspectiveSuffix(t *testing.T) {
	cases := []struct {
		s    g.SlotVI
		want string
	}{
		// Config alone, M perspective = no suffix.
		{g.SlotVI{Configuration: g.MFS, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}, "z"},
		{g.SlotVI{Configuration: g.DDF, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}, "š"},
		{g.SlotVI{Configuration: g.DFS, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}, "č"},
		// Config + Agglomerative G suffix "r".
		{g.SlotVI{Configuration: g.MFS, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}, "zr"},
		{g.SlotVI{Configuration: g.MSS, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}, "tr"},
		{g.SlotVI{Configuration: g.MSC, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}, "kr"},
		// Config + Extension (no allomorph).
		{g.SlotVI{Configuration: g.DPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}, "st"},
		{g.SlotVI{Configuration: g.DPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.ICP, Essence: g.NRM}, "sk"},
	}
	for _, c := range cases {
		if got := ConstructCaRaw(c.s); got != c.want {
			t.Errorf("ConstructCaRaw(%v) = %q, want %q", c.s, got, c.want)
		}
	}
}

func TestConstructCaRaw_PerspectiveStopAllomorph(t *testing.T) {
	// N_/RPV after a stop becomes "h"; A_/RPV becomes "ç".
	// MSS = "t", a stop, so N_/RPV → "h" (not "m") and A_/RPV → "ç" (not "n").
	cases := []struct {
		s    g.SlotVI
		want string
	}{
		{g.SlotVI{Configuration: g.MSS, Affiliation: g.CSL, Perspective: g.N_, Extension: g.DEL, Essence: g.RPV}, "th"},
		{g.SlotVI{Configuration: g.MSS, Affiliation: g.CSL, Perspective: g.A_, Extension: g.DEL, Essence: g.RPV}, "tç"},
		// After Configuration ending in fricative (š), keep "m"/"n".
		{g.SlotVI{Configuration: g.DDF, Affiliation: g.CSL, Perspective: g.N_, Extension: g.DEL, Essence: g.RPV}, "šm"},
		{g.SlotVI{Configuration: g.DDF, Affiliation: g.CSL, Perspective: g.A_, Extension: g.DEL, Essence: g.RPV}, "šn"},
	}
	for _, c := range cases {
		if got := ConstructCaRaw(c.s); got != c.want {
			t.Errorf("ConstructCaRaw(%v) = %q, want %q", c.s, got, c.want)
		}
	}
}
