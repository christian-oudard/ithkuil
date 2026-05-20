package view

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/tokenize"
)

func tok(t *testing.T, w string) tokenize.WordToken {
	t.Helper()
	return tokenize.ClassifyWord(w)
}

func TestType_All(t *testing.T) {
	cases := []struct {
		w    string
		want string
	}{
		{"malëuţřait", "Form"},
		{"hamlala-amlala", "Concat"},
		{"khe", "Ref"},
		{"ţnaxeka", "CombRef"},
		{"řřx", "Bias"},
		{"ha", "Reg"},
		{"hai", "Reg"},
		{"ah", "Mod"},
		{"hla", "Carrier"},
		{"xyzzy", "?"},
	}
	for _, c := range cases {
		got := Type(tok(t, c.w))
		if got != c.want {
			t.Errorf("Type(%q) = %q, want %q", c.w, got, c.want)
		}
	}
}

func TestSegments_FormativeWalkable(t *testing.T) {
	fw := tok(t, "malëuţřait").(tokenize.FormativeWord)
	segs := Segments(fw.Text, fw.Formative, nil)
	if len(segs) == 0 {
		t.Fatal("Segments returned empty slice for malëuţřait")
	}
	for i, s := range segs {
		if s.Slot == "" {
			t.Errorf("Segments[%d] has empty Slot", i)
		}
	}
}

func TestHeadword_NoLexicon(t *testing.T) {
	fw := tok(t, "lalu").(tokenize.FormativeWord)
	h := Headword(fw.Formative, nil)
	if h.Code == "" {
		t.Errorf("Headword without lex: Code empty, expected the Cr cluster")
	}
}

func TestSegmentsModular_BasicAndScope(t *testing.T) {
	mw := tok(t, "ah").(tokenize.ModularWord)
	segs := SegmentsModular(mw.Text, mw.Modular, nil)
	if len(segs) == 0 {
		t.Fatal("SegmentsModular(ah) returned empty")
	}
	glossary := GlossaryModular(segs)
	if len(glossary) == 0 {
		t.Error("GlossaryModular returned empty for ah")
	}
}

func TestSegmentsModular_VerbalVsNominal(t *testing.T) {
	mw := tok(t, "ah").(tokenize.ModularWord)
	verbal := true
	verbalSegs := SegmentsModular(mw.Text, mw.Modular, &verbal)
	nominal := false
	nominalSegs := SegmentsModular(mw.Text, mw.Modular, &nominal)
	getCn := func(segs []Segment) string {
		for _, s := range segs {
			if strings.HasPrefix(s.Slot, "Cn") {
				if len(s.Encodes) > 0 {
					return s.Encodes[0]
				}
			}
		}
		return ""
	}
	v, n := getCn(verbalSegs), getCn(nominalSegs)
	if v == "" || n == "" {
		t.Skipf("Cn not found in segments: %v / %v", verbalSegs, nominalSegs)
	}
	if v == n {
		t.Errorf("Cn gloss didn't change with marksMood: verbal=%q nominal=%q", v, n)
	}
}
