package view

import (
	"testing"
)

// side is the model half of `ithkuil compare`: everything but the
// table drawing. The CLI and the MCP server both build on it, so the
// pairing and diffing are tested here rather than through either one.

func TestSlotDiff_OneSlotApart(t *testing.T) {
	// marçat and marcat differ only in Ca configuration.
	a, err := BuildSide("marçat", nil)
	if err != nil {
		t.Fatalf("BuildSide(marçat): %v", err)
	}
	b, err := BuildSide("marcat", nil)
	if err != nil {
		t.Fatalf("BuildSide(marcat): %v", err)
	}
	rows := SlotDiff(a.Blocks[0], b.Blocks[0])
	var marked []string
	for _, r := range rows {
		if r.Differs {
			marked = append(marked, r.Slot)
		}
	}
	if len(marked) != 1 || marked[0] != "Ca" {
		t.Errorf("marked slots = %v, want [Ca]", marked)
	}

	diffs := GlossDiff(a.Blocks[0], b.Blocks[0])
	if len(diffs) != 1 {
		t.Fatalf("gloss diffs = %v, want one row", diffs)
	}
	if diffs[0].Category != "configuration" || diffs[0].A.Code != "MDF" || diffs[0].B.Code != "DSS" {
		t.Errorf("diff row = %+v, want configuration MDF vs DSS", diffs[0])
	}
}

func TestSlotDiff_Identical(t *testing.T) {
	s, err := BuildSide("marcat", nil)
	if err != nil {
		t.Fatalf("BuildSide: %v", err)
	}
	for _, r := range SlotDiff(s.Blocks[0], s.Blocks[0]) {
		if r.Differs {
			t.Errorf("slot %s marked as differing from itself", r.Slot)
		}
	}
	if d := GlossDiff(s.Blocks[0], s.Blocks[0]); len(d) != 0 {
		t.Errorf("gloss diffs = %v, want none", d)
	}
}

func TestSlotDiff_ByShapeWhenOneSideFails(t *testing.T) {
	// mavẓorf is mavẓorff with the §3.6.1 Ca gemination removed, which
	// re-splits the word and leaves it undecodable. Only shape is
	// comparable then: the decoded side's codes must not mark rows
	// whose conjuncts match.
	good, err := BuildSide("mavẓorff", nil)
	if err != nil {
		t.Fatalf("BuildSide(mavẓorff): %v", err)
	}
	bad, err := BuildSide("mavẓorf", nil)
	if err != nil {
		t.Fatalf("BuildSide(mavẓorf): %v", err)
	}
	if bad.Blocks[0].Decoded {
		t.Fatal("mavẓorf should not decode; the comparison rests on that")
	}
	if bad.Blocks[0].Note == "" {
		t.Error("an undecoded block must carry the decoder's complaint")
	}
	for _, r := range SlotDiff(good.Blocks[0], bad.Blocks[0]) {
		if r.A.Chunk == r.B.Chunk && r.Differs {
			t.Errorf("slot %s has the same conjunct on both sides but is marked", r.Slot)
		}
	}
	if d := GlossDiff(good.Blocks[0], bad.Blocks[0]); len(d) != 0 {
		t.Errorf("an undecoded side has no codes to diff; got %v", d)
	}
}

func TestPairSides_FromTheParentEnd(t *testing.T) {
	// A chain's dependents lead and its parent comes last (§3.1.7), so
	// a standalone word is the counterpart of the last member, and the
	// leading dependent goes unpaired.
	chain, err := BuildSide("hakšal-uḑfarf", nil)
	if err != nil {
		t.Fatalf("BuildSide(chain): %v", err)
	}
	lone, err := BuildSide("marcat", nil)
	if err != nil {
		t.Fatalf("BuildSide(marcat): %v", err)
	}
	pairs, extra := PairSides(chain, lone)
	if len(pairs) != 1 {
		t.Fatalf("pairs = %d, want 1", len(pairs))
	}
	last := chain.Blocks[len(chain.Blocks)-1]
	if pairs[0].A.Word != last.Word {
		t.Errorf("paired %q, want the parent %q", pairs[0].A.Word, last.Word)
	}
	if len(extra) != 1 || extra[0].Owner != chain.Word {
		t.Errorf("unpaired = %+v, want one member of %q", extra, chain.Word)
	}
}

func TestBuildSide_NoSlotStructure(t *testing.T) {
	// A referential has no slots to lay out, so it is refused rather
	// than compared against nothing.
	if _, err := BuildSide("khe", nil); err == nil {
		t.Error("expected an error comparing a referential")
	}
}

func TestAlignByKey_Gaps(t *testing.T) {
	// A key present on one side only gets its own row, marked -1 on
	// the side that lacks it, and shared keys stay in order.
	got := AlignByKey([]string{"Cr", "Vr", "Ca"}, []string{"Cr", "Ca"})
	want := [][2]int{{0, 0}, {1, -1}, {2, 1}}
	if len(got) != len(want) {
		t.Fatalf("rows = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Errorf("row %d = %v, want %v", i, got[i], want[i])
		}
	}
}
