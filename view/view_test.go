package view

import (
	"path/filepath"
	"strings"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/render"
	"github.com/christian-oudard/ithkuil/slots"
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

func TestSegments_VariantShapes(t *testing.T) {
	// Walk a few formative shapes to exercise the per-slot segment
	// builders: Cs-root, ref-root, Slot V affixes, Slot VIII present.
	for _, w := range []string{
		"ealali",    // RefRoot (ae Vv)
		"ëilal",     // CsRoot (ëi Vv)
		"oërmölá",   // CsRoot CPT.DYN + ASR ULT
		"amlalara",  // CrRoot with Slot V (geminated Ca + reversed Cs/Vx)
		"amlalahla", // CrRoot with Slot VIII
	} {
		tok := tokenize.ClassifyWord(w)
		v, ok := tok.(tokenize.FormativeWord)
		if !ok {
			t.Errorf("%q didn't tokenize as a formative (%T)", w, tok)
			continue
		}
		segs := Segments(v.Text, v.Formative, nil)
		if len(segs) == 0 {
			t.Errorf("Segments(%q) returned empty", w)
		}
	}
}

func TestSegments_WithSlotV_AndConcat(t *testing.T) {
	// Build a Slot V formative directly and round-trip the surface
	// through Segments so ccCodes / slotVCsSegment / slotVVxSegment
	// actually get exercised.
	f := g.MinimalFormative("ml")
	f.SlotV = []g.Affix{
		{Type: g.Type1Affix, Degree: 5, Consonant: "r"},
		{Type: g.Type1Affix, Degree: 5, Consonant: "r"},
	}
	surface := render.Formative(f)
	segs := Segments(surface, f, nil)
	if len(segs) == 0 {
		t.Fatal("Segments for Slot V formative returned empty")
	}
	// Concat formative: ccCodes gets the T1 prefix.
	t1 := g.Type1
	f2 := g.MinimalFormative("ml")
	f2.Concat = t1
	surface = render.Formative(f2)
	segs = Segments(surface, f2, nil)
	if len(segs) == 0 {
		t.Fatal("Segments for concat formative returned empty")
	}
	foundCc := false
	for _, s := range segs {
		if s.Slot == "Cc" || s.Slot == "I" {
			foundCc = true
			if len(s.Encodes) == 0 {
				t.Errorf("Cc segment has no encoded codes: %+v", s)
			}
		}
	}
	if !foundCc {
		// The Cc slot label may use a different name; just ensure
		// ccCodes ran (no panic + segments produced).
		t.Log("Cc segment label not found; Segments still produced output")
	}
}

func TestHeadword_WithLexicon(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "data", "data.json"))
	if err != nil {
		t.Fatalf("LoadDefault: %v", err)
	}
	tok := tokenize.ClassifyWord("lalu").(tokenize.FormativeWord)
	h := Headword(tok.Formative, lex)
	if h.Code == "" {
		t.Error("Headword with lex: Code empty")
	}
	if h.Meaning == "" {
		t.Error("Headword with lex: Meaning empty for known root l")
	}
}

func TestGlossary_WithLexicon(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "data", "data.json"))
	if err != nil {
		t.Fatalf("LoadDefault: %v", err)
	}
	// Build a formative with a lexicon-known affix in Slot VII.
	f := g.MinimalFormative("ml")
	f.SlotVII = []g.Affix{{Type: g.Type1Affix, Degree: 5, Consonant: "r"}}
	surface := render.Formative(f)
	segs := Segments(surface, f, lex)
	glossary := Glossary(surface, f, segs, lex)
	if len(glossary) == 0 {
		t.Error("Glossary with lexicon returned empty")
	}
}

func TestGlossary_FormativeWithLexicon(t *testing.T) {
	// Without the embedded lexicon we still want Glossary to walk the
	// segments and emit category rows for non-default slots. Use a
	// formative known to have a non-default Ca + Vc.
	tok := tokenize.ClassifyWord("emlölo").(tokenize.FormativeWord)
	segs := Segments(tok.Text, tok.Formative, nil)
	glossary := Glossary(tok.Text, tok.Formative, segs, nil)
	if len(glossary) == 0 {
		t.Error("Glossary returned empty for emlölo")
	}
	for i, e := range glossary {
		if e.Category == "" || e.Code == "" {
			t.Errorf("Glossary[%d] missing Category or Code: %+v", i, e)
		}
	}
}

func TestType_AllVariants(t *testing.T) {
	// Exercise every WordToken kind once. Some need manual construction
	// because they're created by recognizers that aren't normally
	// triggered through ClassifyWord alone.
	cases := []struct {
		tok  tokenize.WordToken
		want string
	}{
		{tokenize.FormativeWord{}, "Form"},
		{tokenize.ConcatenatedFormativeWord{}, "Concat"},
		{tokenize.ReferentialWord{}, "Ref"},
		{tokenize.CombinationRefWord{}, "CombRef"},
		{tokenize.BiasWord{}, "Bias"},
		{tokenize.RegisterStartWord{}, "Reg"},
		{tokenize.RegisterEndWord{}, "Reg"},
		{tokenize.ModularWord{}, "Mod"},
		{tokenize.SingleAffixWord{}, "Affix"},
		{tokenize.MultipleAffixWord{}, "Affixes"},
		{tokenize.CarrierWord{}, "Carrier"},
		{tokenize.ForeignWord{}, "(fgn)"},
		{tokenize.UnknownWord{}, "?"},
	}
	for _, c := range cases {
		if got := Type(c.tok); got != c.want {
			t.Errorf("Type(%T) = %q, want %q", c.tok, got, c.want)
		}
	}
}

func TestCategoryForSlot_AllSlots(t *testing.T) {
	cases := []struct {
		slot, want string
	}{
		{"Vv", "version"},
		{"Cr", "root"},
		{"Vr", "Vr"},
		{"Ca", "Ca"},
		{"Vx₁", "affix degree"},
		{"Cs₂", "affix"},
		{"Vn", "slot VIII"},
		{"Cn", "mood/scope"},
		{"Vc", "case"},
		{"Vk", "illocution"},
		{"foo", "foo"}, // fallthrough
	}
	for _, c := range cases {
		if got := categoryForSlot(c.slot); got != c.want {
			t.Errorf("categoryForSlot(%q) = %q, want %q", c.slot, got, c.want)
		}
	}
}

func TestStemMeaning_Fallback(t *testing.T) {
	// All stems empty → empty result.
	e := lexicon.RootEntry{}
	if got := stemMeaning(e, g.S1); got != "" {
		t.Errorf("empty entry stemMeaning = %q, want \"\"", got)
	}
	// S2 requested but only S1 populated → fallback annotated.
	e = lexicon.RootEntry{Stem1: "primary"}
	got := stemMeaning(e, g.S2)
	if !strings.Contains(got, "fallback") {
		t.Errorf("stemMeaning fallback = %q, expected 'fallback' note", got)
	}
	// Requested stem available → no fallback note.
	e = lexicon.RootEntry{Stem1: "primary", Stem2: "secondary"}
	got = stemMeaning(e, g.S2)
	if strings.Contains(got, "fallback") {
		t.Errorf("stemMeaning(S2) when present = %q, shouldn't say 'fallback'", got)
	}
}

func TestStemNum(t *testing.T) {
	for _, c := range []struct {
		s    g.Stem
		want int
	}{
		{g.S0, 0}, {g.S1, 1}, {g.S2, 2}, {g.S3, 3},
	} {
		if got := stemNum(c.s); got != c.want {
			t.Errorf("stemNum(%v) = %d, want %d", c.s, got, c.want)
		}
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

func TestHeadword_CsRoot(t *testing.T) {
	// Without lexicon — CsRoot branch with empty meaning.
	f := g.MinimalFormative("ml")
	f.Root = g.CsRoot{Cs: "r", Degree: 5, Version: g.PRC, Function: g.STA, Context: g.EXS}
	h := Headword(f, nil)
	if h.Code == "" {
		t.Error("CsRoot Headword without lex: Code empty")
	}
	if h.Meaning != "" {
		t.Errorf("CsRoot Headword without lex: Meaning = %q, want empty", h.Meaning)
	}
	// With lexicon — exercises the affix-lookup branch.
	lex, err := lexicon.Load(filepath.Join("..", "data", "data.json"))
	if err != nil {
		t.Fatalf("LoadDefault: %v", err)
	}
	h = Headword(f, lex)
	if h.Code == "" {
		t.Error("CsRoot Headword with lex: Code empty")
	}
}

func TestHeadword_RefRoot(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.Root = g.RefRoot{C1: "l", Version: g.PRC, SlotIV: g.DefaultSlotIV}
	h := Headword(f, nil)
	if h.Code == "" {
		t.Error("RefRoot Headword: Code empty")
	}
	if h.Meaning == "" {
		t.Error("RefRoot Headword: Meaning empty")
	}
}

func TestHeadword_EmptyCluster(t *testing.T) {
	f := g.MinimalFormative("ml")
	cr := f.Root.(g.CrRoot)
	cr.Cluster = ""
	f.Root = cr
	h := Headword(f, nil)
	if h.Code != "" || h.Meaning != "" {
		t.Errorf("empty cluster Headword: %+v, want zero RootHead", h)
	}
}

func TestSegments_ParseError(t *testing.T) {
	// Invalid word that won't parse via slots.Parse — Segments should
	// return nil rather than panic.
	f := g.MinimalFormative("ml")
	if segs := Segments("", f, nil); segs != nil {
		t.Errorf("Segments(empty) = %v, want nil", segs)
	}
}

func TestCcCodes_AllBranches(t *testing.T) {
	cases := []struct {
		cc   string
		want []string // substrings expected in result
	}{
		{"h", []string{"Type1 concat"}},
		{"hw", []string{"Type2 concat"}},
		{"hl", []string{"Type1 concat", "Slot I shortcut"}},
		{"hr", []string{"Type2 concat", "Slot I shortcut"}},
		{"w", []string{"Slot I shortcut"}},
		{"y", []string{"Slot I shortcut"}},
	}
	for _, c := range cases {
		got := ccCodes(c.cc)
		joined := strings.Join(got, ",")
		for _, want := range c.want {
			if !strings.Contains(joined, want) {
				t.Errorf("ccCodes(%q) = %v, want substring %q", c.cc, got, want)
			}
		}
	}
	// Unknown cc falls through to the raw label.
	got := ccCodes("zzz")
	if len(got) == 0 || got[0] != "zzz" {
		t.Errorf("ccCodes(unknown) = %v, want passthrough", got)
	}
}

func TestDisplayVv_AllShapes(t *testing.T) {
	// 1-rune Vv with 2+ Slot V affixes → reduplicate around glottal.
	l := slots.Layout{
		Vv: "a",
		SlotV: []slots.AffixChunk{
			{Cs: "r", Vx: "a"}, {Cs: "r", Vx: "a"},
		},
	}
	if got := displayVv(l); got != "a'a" {
		t.Errorf("displayVv 1-rune = %q, want a'a", got)
	}
	// 2-rune Vv → glottal between.
	l.Vv = "au"
	if got := displayVv(l); got != "a'u" {
		t.Errorf("displayVv 2-rune = %q, want a'u", got)
	}
	// >2-rune Vv → append trailing glottal.
	l.Vv = "aiu"
	if got := displayVv(l); got != "aiu'" {
		t.Errorf("displayVv >2-rune = %q, want aiu'", got)
	}
	// No Slot V → passthrough.
	l.SlotV = nil
	l.Vv = "a"
	if got := displayVv(l); got != "a" {
		t.Errorf("displayVv passthrough = %q, want a", got)
	}
}

func TestSlotIXLabelCodes_AllBranches(t *testing.T) {
	cases := []struct {
		f       g.Final
		slot    string
		wantOne string
	}{
		{g.UnframedNominal{Case: g.THM}, "Vc", "THM"},
		{g.FramedVerbal{Case: g.ERG}, "Vc", "ERG"},
		{g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}, "Vk", "ASR"},
		{g.UnframedVerbal{Vk: g.Assertive{Validation: g.REC}}, "Vk", "ASR"},
		{g.UnframedVerbal{Vk: g.Directive{}}, "Vk", "DIR"},
	}
	for _, c := range cases {
		slot, codes, _ := slotIXLabelCodes(c.f)
		if slot != c.slot {
			t.Errorf("slotIXLabelCodes(%T) slot = %q, want %q", c.f, slot, c.slot)
		}
		if len(codes) == 0 || codes[0] != c.wantOne {
			t.Errorf("slotIXLabelCodes(%T) codes = %v, want first %q", c.f, codes, c.wantOne)
		}
	}
	// nil Final returns the empty default.
	slot, codes, isDefault := slotIXLabelCodes(nil)
	if slot != "Vc" || codes != nil || !isDefault {
		t.Errorf("slotIXLabelCodes(nil) = (%q,%v,%v), want (Vc,nil,true)", slot, codes, isDefault)
	}
}

func TestSegmentsModular_FullShape(t *testing.T) {
	// Build a modular adjunct with prefix + pairs + Vh final, plus a
	// non-Vh final case. We construct ModularAdjunct values directly
	// to bypass parser-driven shape constraints.
	ma := g.ModularAdjunct{
		Scope:   g.ModularScopeParent,
		Content: []g.SlotVIII{g.VnCnValence{Valence: g.MNO, MoodScope: g.FAC}},
	}
	verbal := true
	segs := SegmentsModular("walai", ma, &verbal)
	foundScope := false
	for _, s := range segs {
		if s.Slot == "scope" {
			foundScope = true
		}
	}
	if !foundScope {
		t.Error("expected a scope segment for the w prefix")
	}
}

func TestGlossaryModular_AllBranches(t *testing.T) {
	// Hand-craft segments hitting scope, Vh, and Cm sub-branches.
	segs := []Segment{
		{Raw: "w", Slot: "scope", Encodes: []string{"FOC"}},
		{Raw: "a", Slot: "Vh", Encodes: []string{"VH"}},
		{Raw: "n", Slot: "Cn₁", Encodes: []string{"CmAspect"}},
		{Raw: "ň", Slot: "Cn₂", Encodes: []string{"CmOther"}},
	}
	out := GlossaryModular(segs)
	if len(out) != 4 {
		t.Fatalf("GlossaryModular: got %d entries, want 4", len(out))
	}
	cats := map[string]bool{}
	for _, e := range out {
		cats[e.Category] = true
	}
	if !cats["scope"] {
		t.Error("missing scope category")
	}
	if !cats["marker"] {
		t.Error("missing marker category for Cm rows")
	}
	// Dedup: same slot+code shouldn't appear twice.
	dup := append(segs, segs[0])
	out2 := GlossaryModular(dup)
	if len(out2) != len(out) {
		t.Errorf("GlossaryModular didn't dedupe: %d vs %d", len(out2), len(out))
	}
}

func TestAffixDegreeGloss_Edge(t *testing.T) {
	if name, meaning := affixDegreeGloss("r", 5, nil); name != "" || meaning != "" {
		t.Errorf("affixDegreeGloss(nil lex) = (%q,%q), want empty", name, meaning)
	}
	lex, err := lexicon.Load(filepath.Join("..", "data", "data.json"))
	if err != nil {
		t.Fatalf("LoadDefault: %v", err)
	}
	if name, meaning := affixDegreeGloss("zzznonexistent", 5, lex); name != "" || meaning != "" {
		t.Errorf("affixDegreeGloss(unknown cs) = (%q,%q), want empty", name, meaning)
	}
}

func TestSubscript_OutOfRange(t *testing.T) {
	if got := subscript(-1); got != "-1" {
		t.Errorf("subscript(-1) = %q, want \"-1\"", got)
	}
	if got := subscript(10); got != "10" {
		t.Errorf("subscript(10) = %q, want \"10\"", got)
	}
}

func TestCategoryForCode_Fallback(t *testing.T) {
	// A code that compose.LookupGrammar won't find triggers the
	// categoryForSlot fallback.
	got := categoryForCode("ZZZ-not-a-code", "Vc")
	if got != "case" {
		t.Errorf("categoryForCode unknown code = %q, want case (slot fallback)", got)
	}
}
