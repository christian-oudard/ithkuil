package allomorph

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

func TestCaForwardCovers3840(t *testing.T) {
	if len(CaForward) != 3840 {
		t.Errorf("CaForward size = %d, want 3840", len(CaForward))
	}
}

func TestCaRoundTrip(t *testing.T) {
	// For every SlotVI, ConstructCa → ParseCa should return a SlotVI
	// that produces the same cluster (collisions are allowed, but the
	// chosen pre-image must round-trip).
	for s, cluster := range CaForward {
		got, ok := ParseCa(cluster)
		if !ok {
			t.Errorf("ParseCa(%q) failed for source %v", cluster, s)
			continue
		}
		if CaForward[got] != cluster {
			t.Errorf("collision mismatch: ConstructCa(%v)=%q, ParseCa(%q)=%v, ConstructCa(%v)=%q",
				s, cluster, cluster, got, got, CaForward[got])
		}
	}
}

func TestParseCa_KnownForms(t *testing.T) {
	cases := []struct {
		in   string
		want g.SlotVI
	}{
		// Standalone perspective forms
		{"l", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}},
		{"r", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}},
		{"v", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.N_, Extension: g.DEL, Essence: g.NRM}},
		{"j", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.A_, Extension: g.DEL, Essence: g.NRM}},
		{"tļ", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.RPV}},
		{"ř", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.RPV}},
		{"m", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.N_, Extension: g.DEL, Essence: g.RPV}},
		{"n", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.A_, Extension: g.DEL, Essence: g.RPV}},
		// UNI + Extension
		{"d", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}},
		{"g", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.ICP, Essence: g.NRM}},
		{"b", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.ATV, Essence: g.NRM}},
		{"gz", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.GRA, Essence: g.NRM}},
		{"bz", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DPL, Essence: g.NRM}},
		// UNI + Affiliation
		{"nļ", g.SlotVI{Configuration: g.UNI, Affiliation: g.ASO, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}},
		{"rļ", g.SlotVI{Configuration: g.UNI, Affiliation: g.COA, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}},
		{"ň", g.SlotVI{Configuration: g.UNI, Affiliation: g.VAR, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}},
		// Config + perspective suffix
		{"tr", g.SlotVI{Configuration: g.MSS, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}},
		{"kr", g.SlotVI{Configuration: g.MSC, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}},
		{"tw", g.SlotVI{Configuration: g.MSS, Affiliation: g.CSL, Perspective: g.N_, Extension: g.DEL, Essence: g.NRM}},
		{"ty", g.SlotVI{Configuration: g.MSS, Affiliation: g.CSL, Perspective: g.A_, Extension: g.DEL, Essence: g.NRM}},
		// Bare configurations
		{"z", g.SlotVI{Configuration: g.MFS, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}},
		{"š", g.SlotVI{Configuration: g.DDF, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}},
		{"č", g.SlotVI{Configuration: g.DFS, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}},
		// Affiliation + Configuration
		{"ls", g.SlotVI{Configuration: g.DPX, Affiliation: g.ASO, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}},
		{"rs", g.SlotVI{Configuration: g.DPX, Affiliation: g.COA, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}},
		{"lţ", g.SlotVI{Configuration: g.MDS, Affiliation: g.ASO, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}},
		{"rţ", g.SlotVI{Configuration: g.MDS, Affiliation: g.COA, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}},
		{"řţ", g.SlotVI{Configuration: g.MDS, Affiliation: g.VAR, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}},
		{"zr", g.SlotVI{Configuration: g.MFS, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}},
		// Config + Extension
		{"st", g.SlotVI{Configuration: g.DPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}},
		{"sk", g.SlotVI{Configuration: g.DPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.ICP, Essence: g.NRM}},
		// Substitutions in play
		{"nt", g.SlotVI{Configuration: g.MSS, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}},
		{"nk", g.SlotVI{Configuration: g.MSC, Affiliation: g.CSL, Perspective: g.M_, Extension: g.ICP, Essence: g.NRM}},
		{"mp", g.SlotVI{Configuration: g.MSF, Affiliation: g.CSL, Perspective: g.M_, Extension: g.ATV, Essence: g.NRM}},
		{"mb", g.SlotVI{Configuration: g.MSF, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DPL, Essence: g.NRM}},
		{"ng", g.SlotVI{Configuration: g.MSC, Affiliation: g.CSL, Perspective: g.M_, Extension: g.GRA, Essence: g.NRM}},
		{"gm", g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.N_, Extension: g.ICP, Essence: g.RPV}},
		{"nd", g.SlotVI{Configuration: g.MDF, Affiliation: g.CSL, Perspective: g.A_, Extension: g.DEL, Essence: g.NRM}},
		{"nš", g.SlotVI{Configuration: g.UNI, Affiliation: g.COA, Perspective: g.G_, Extension: g.DEL, Essence: g.RPV}},
		{"nx", g.SlotVI{Configuration: g.MSC, Affiliation: g.CSL, Perspective: g.N_, Extension: g.GRA, Essence: g.RPV}},
		{"řkpl", g.SlotVI{Configuration: g.MSC, Affiliation: g.VAR, Perspective: g.M_, Extension: g.ATV, Essence: g.RPV}},
	}
	for _, c := range cases {
		got, ok := ParseCa(c.in)
		if !ok {
			t.Errorf("ParseCa(%q) failed; want %v", c.in, c.want)
			continue
		}
		if got != c.want {
			t.Errorf("ParseCa(%q) = %v, want %v", c.in, got, c.want)
		}
	}
}

func TestParseCa_RejectsJunk(t *testing.T) {
	for _, s := range []string{"", "xxxxxx", "qzz", "blah"} {
		if v, ok := ParseCa(s); ok {
			t.Errorf("ParseCa(%q) = %v, want failure", s, v)
		}
	}
}
