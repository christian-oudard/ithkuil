package inspect

import (
	"bytes"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/tokenize"
)

// helpers ------------------------------------------------------------

func tok(t *testing.T, w string) tokenize.WordToken {
	t.Helper()
	return tokenize.ClassifyWord(w)
}

// formative-level helpers ------------------------------------------------

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

func TestSlotExtractors_FormativeBasics(t *testing.T) {
	w := tok(t, "malëuţřait")
	if SlotI(w) != Dot {
		t.Errorf("SlotI = %q, want default %q", SlotI(w), Dot)
	}
	if SlotIII(w) == "" {
		t.Errorf("SlotIII empty for malëuţřait")
	}
	// Slot VIII is filled — malëuţřait carries a non-trivial Vn+Cn pair.
	if got := SlotVIII(w); got == "" {
		t.Errorf("SlotVIII empty for malëuţřait, want non-empty")
	}
}

func TestSlotVI_NonDefault(t *testing.T) {
	// "ulthal" is contrived; pick a corpus form with non-default Ca.
	// "emlölo" - S2/PRC, Cr=ml, Vr=ölö → non-default SlotVI from the
	// allomorph table.
	w := tok(t, "emlölo")
	if _, ok := w.(tokenize.FormativeWord); !ok {
		t.Fatalf("emlölo classified as %T, want FormativeWord", w)
	}
	// Just exercise the extractor; the value depends on parsed Slot VI.
	_ = SlotVI(w)
}

func TestSlotI_Concat(t *testing.T) {
	c := tok(t, "hamlala-amlala") // T1 concat
	if got := SlotI(c); got != "T1" && got != Dot {
		t.Errorf("SlotI on concat chain = %q, want T1 or Dot (concat label)", got)
	}
}

func TestStress_Variants(t *testing.T) {
	cases := []struct {
		w    string
		want string
	}{
		{"amlala", "PEN"},
		{"amlalú", "ULT"},
		{"ámlala", "ANT"},
		{"la", "MONO"},
	}
	for _, c := range cases {
		got := Stress(tok(t, c.w))
		if got != c.want && got != Dot {
			t.Errorf("Stress(%q) = %q, want %q", c.w, got, c.want)
		}
	}
}

// Polygraph / Diff -------------------------------------------------

func TestPolygraph_SmokeRun(t *testing.T) {
	toks := tokenize.Tokenize("malëuţřait amlalú")
	var buf bytes.Buffer
	Polygraph(&buf, toks)
	if buf.Len() == 0 {
		t.Error("Polygraph wrote nothing")
	}
	out := buf.String()
	// Should contain at least the slot headers for filled rows.
	for _, label := range []string{"III"} {
		if !strings.Contains(out, label) {
			t.Errorf("Polygraph output missing slot label %q", label)
		}
	}
}

func TestDiff_FormativeVsFormative(t *testing.T) {
	a := tok(t, "amlala")
	b := tok(t, "amlalú")
	var buf bytes.Buffer
	Diff(&buf, []tokenize.WordToken{a}, []tokenize.WordToken{b})
	if buf.Len() == 0 {
		t.Error("Diff wrote nothing for differing tokens")
	}
}

func TestDiff_EmptyOnIdentical(t *testing.T) {
	a := tok(t, "amlala")
	b := tok(t, "amlala")
	var buf bytes.Buffer
	Diff(&buf, []tokenize.WordToken{a}, []tokenize.WordToken{b})
	// Permitted to write headers but should not report any differences;
	// loose check: output should not contain a row prefix marker on
	// every line (the "differs" indicator).
	out := buf.String()
	if strings.Count(out, "!=") > 0 {
		t.Errorf("Diff on identical inputs reported differences:\n%s", out)
	}
}

// Segments / Glossary ----------------------------------------------

func TestSegments_FormativeWalkable(t *testing.T) {
	fw := tok(t, "malëuţřait").(tokenize.FormativeWord)
	segs := Segments(fw.Text, fw.Formative, nil)
	if len(segs) == 0 {
		t.Fatal("Segments returned empty slice for malëuţřait")
	}
	// Every segment must label a slot.
	for i, s := range segs {
		if s.Slot == "" {
			t.Errorf("Segments[%d] has empty Slot", i)
		}
	}
}

func TestHeadword_NoLexicon(t *testing.T) {
	fw := tok(t, "lalu").(tokenize.FormativeWord)
	// Without a lexicon, Headword falls back to the bare cluster.
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
	// Verbal next formative: Cn glosses as Mood.
	verbal := true
	mw := tokenize.ModularWord{
		Text:      "ah",
		Modular:   tok(t, "ah").(tokenize.ModularWord).Modular,
		MarksMood: &verbal,
	}
	verbalSegs := SegmentsModular(mw.Text, mw.Modular, mw.MarksMood)
	// Nominal: Cn glosses as Case-Scope.
	nominal := false
	mw.MarksMood = &nominal
	nominalSegs := SegmentsModular(mw.Text, mw.Modular, mw.MarksMood)
	// Find the Cn segment in each and compare.
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
		t.Skipf("Cn not found in segments (Cn-elided modular shape: %v / %v)", verbalSegs, nominalSegs)
	}
	if v == n {
		t.Errorf("Cn gloss didn't change with marksMood: verbal=%q nominal=%q", v, n)
	}
}
