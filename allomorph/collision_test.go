package allomorph

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// TestConstructCa_KnownCollision_UNINonCSLNonDEL pins the §3.6 Ca
// allomorph bug surfaced by the full Slot-VI round-trip grid.
//
// ConstructCaRaw's first branch fires when Configuration == UNI and
// Extension != DEL and returns ca2Standalone[Extension]+perspective
// without ever consulting ca3[Affiliation]. So a non-CSL Affiliation
// silently collapses onto its CSL form, losing information across
// 120 cells of the 3840-cell space (3 non-CSL affiliations × 4
// perspectives × 5 non-DEL extensions × 2 essences).
//
// Each row below is a concrete example of the collision. When the
// underlying ConstructCa is fixed to encode Affiliation in this
// branch, this test will fail loudly — at that point the entries
// here should be deleted and the BUG note in
// fullparse.TestRoundTrip_Grid_AllSlotVI's known-collision class
// removed.
func TestConstructCa_KnownCollision_UNINonCSLNonDEL(t *testing.T) {
	cases := []struct {
		csl, nonCSL g.SlotVI
	}{
		{
			csl:    g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM},
			nonCSL: g.SlotVI{Configuration: g.UNI, Affiliation: g.ASO, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM},
		},
		{
			csl:    g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.A_, Extension: g.GRA, Essence: g.NRM},
			nonCSL: g.SlotVI{Configuration: g.UNI, Affiliation: g.VAR, Perspective: g.A_, Extension: g.GRA, Essence: g.NRM},
		},
		{
			csl:    g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.G_, Extension: g.ICP, Essence: g.RPV},
			nonCSL: g.SlotVI{Configuration: g.UNI, Affiliation: g.COA, Perspective: g.G_, Extension: g.ICP, Essence: g.RPV},
		},
		{
			csl:    g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.N_, Extension: g.DPL, Essence: g.RPV},
			nonCSL: g.SlotVI{Configuration: g.UNI, Affiliation: g.VAR, Perspective: g.N_, Extension: g.DPL, Essence: g.RPV},
		},
	}
	for _, c := range cases {
		a, b := ConstructCa(c.csl), ConstructCa(c.nonCSL)
		if a != b {
			t.Errorf("collision lifted for %v vs %v (Ca=%q vs %q) — delete this case and fix the surrounding bug docs",
				c.csl, c.nonCSL, a, b)
		}
	}
}
