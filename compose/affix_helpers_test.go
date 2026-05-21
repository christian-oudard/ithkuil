package compose

import (
	"testing"

	"github.com/christian-oudard/ithkuil/fullparse"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/render"
)

func TestMCSDegreeForMood(t *testing.T) {
	cases := []struct {
		m  g.Mood
		d  int
		ok bool
	}{
		{g.FAC, 0, false}, // unmarked default
		{g.SUB, 1, true},
		{g.ASM, 2, true},
		{g.SPC, 3, true},
		{g.COU, 4, true},
		{g.HYP, 5, true},
	}
	for _, c := range cases {
		d, ok := MCSDegreeForMood(c.m)
		if ok != c.ok || (ok && d != c.d) {
			t.Errorf("MCSDegreeForMood(%v) = %d/%v, want %d/%v", c.m, d, ok, c.d, c.ok)
		}
	}
}

func TestMCSDegreeForCaseScope(t *testing.T) {
	cases := []struct {
		cs g.CaseScope
		d  int
		ok bool
	}{
		{g.CCN, 0, false}, // unmarked default
		{g.CCV, 0, true},
		{g.CCA, 6, true},
		{g.CCS, 7, true},
		{g.CCQ, 8, true},
		{g.CCP, 9, true},
	}
	for _, c := range cases {
		d, ok := MCSDegreeForCaseScope(c.cs)
		if ok != c.ok || (ok && d != c.d) {
			t.Errorf("MCSDegreeForCaseScope(%v) = %d/%v, want %d/%v", c.cs, d, ok, c.d, c.ok)
		}
	}
}

func TestWithMCSMood_RoundTrip(t *testing.T) {
	// Build a FRAMED verbal formative and add MCS for SUB. The result
	// should render to a surface that parses back with an MCS affix
	// (consonant "bẓ") at degree 1 in Slot VII.
	f := g.MinimalFormative("ml")
	f.Final = g.FramedVerbal{Case: g.THM}
	f, err := WithMCSMood(f, g.SUB)
	if err != nil {
		t.Fatalf("WithMCSMood: %v", err)
	}
	surf := render.Formative(f)
	back, err := fullparse.ParseFormative(surf)
	if err != nil {
		t.Fatalf("ParseFormative(%q): %v", surf, err)
	}
	var found *g.Affix
	for i, a := range back.SlotVII {
		if a.Consonant == MCSCs {
			found = &back.SlotVII[i]
			break
		}
	}
	if found == nil {
		t.Fatalf("no MCS affix in round-tripped SlotVII (surface %q, slot7=%+v)", surf, back.SlotVII)
	}
	if found.Degree != 1 {
		t.Errorf("MCS degree = %d, want 1 (SUB)", found.Degree)
	}
}

func TestWithMCSMood_RejectsFAC(t *testing.T) {
	f := g.MinimalFormative("ml")
	if _, err := WithMCSMood(f, g.FAC); err == nil {
		t.Error("WithMCSMood(FAC) should fail (FAC is default, no MCS encoding)")
	}
}

func TestWithMCSCaseScope_RejectsCCN(t *testing.T) {
	f := g.MinimalFormative("ml")
	if _, err := WithMCSCaseScope(f, g.CCN); err == nil {
		t.Error("WithMCSCaseScope(CCN) should fail (CCN is default, no MCS encoding)")
	}
}

func TestWithCHC_RoundTrip(t *testing.T) {
	for deg := 1; deg <= 9; deg++ {
		f := g.MinimalFormative("ml")
		f, err := WithCHC(f, deg)
		if err != nil {
			t.Errorf("WithCHC(%d): %v", deg, err)
			continue
		}
		surf := render.Formative(f)
		back, err := fullparse.ParseFormative(surf)
		if err != nil {
			t.Errorf("ParseFormative(%q): %v", surf, err)
			continue
		}
		var found *g.Affix
		for i, a := range back.SlotVII {
			if a.Consonant == CHCCs {
				found = &back.SlotVII[i]
				break
			}
		}
		if found == nil {
			t.Errorf("CHC degree=%d: no CHC affix in round-trip (surface %q)", deg, surf)
			continue
		}
		if found.Degree != deg {
			t.Errorf("CHC degree round-trip: got %d, want %d", found.Degree, deg)
		}
	}
}

func TestWithCHC_RejectsBadDegree(t *testing.T) {
	f := g.MinimalFormative("ml")
	for _, deg := range []int{0, -1, 10, 100} {
		if _, err := WithCHC(f, deg); err == nil {
			t.Errorf("WithCHC(%d) should fail", deg)
		}
	}
}

// TestMCSCs_LexiconEntry confirms the constants we use are real affixes
// in the bundled lexicon. Guards against silent drift if the lexicon
// is regenerated and renames a Cs.
func TestMCSCs_LexiconEntry(t *testing.T) {
	lex, err := lexicon.LoadDefault()
	if err != nil {
		t.Fatalf("LoadDefault: %v", err)
	}
	for _, cs := range []string{MCSCs, CHCCs} {
		entry, ok := lex.Affixes[cs]
		if !ok {
			t.Errorf("affix %q not in lexicon", cs)
			continue
		}
		if entry.Cs != cs {
			t.Errorf("lexicon[%q].Cs = %q, want %q", cs, entry.Cs, cs)
		}
	}
	if entry := lex.Affixes[MCSCs]; entry.Abbrev != "MCS" {
		t.Errorf("MCSCs abbrev = %q, want MCS", entry.Abbrev)
	}
	if entry := lex.Affixes[CHCCs]; entry.Abbrev != "CHC" {
		t.Errorf("CHCCs abbrev = %q, want CHC", entry.Abbrev)
	}
}
