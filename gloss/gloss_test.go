package gloss

import (
	"path/filepath"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/fullparse"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
)

func dataPath(name string) string {
	return filepath.Join("..", "data", name)
}

func loadLex(t *testing.T) *lexicon.Lexicon {
	t.Helper()
	lex, err := lexicon.Load(dataPath("roots.json"), dataPath("affixes.json"))
	if err != nil {
		t.Fatalf("lexicon load: %v", err)
	}
	return lex
}

func TestFormative_Minimal(t *testing.T) {
	// MinimalFormative("ml") is all defaults except the root.
	// Gloss should just be "-ml-".
	f := g.MinimalFormative("ml")
	got := Formative(f)
	want := "-ml-"
	if got != want {
		t.Errorf("Formative(minimal) = %q, want %q", got, want)
	}
}

func TestFormative_NonDefaultSlotII(t *testing.T) {
	f := g.MinimalFormative("ml")
	cr := f.Root.(g.CrRoot)
	cr.Stem = g.S2
	cr.Version = g.CPT
	f.Root = cr
	got := Formative(f)
	want := "S2/CPT--ml-"
	if got != want {
		t.Errorf("Formative(S2/CPT) = %q, want %q", got, want)
	}
}

func TestFormative_NonDefaultSlotIV(t *testing.T) {
	f := g.MinimalFormative("ml")
	cr := f.Root.(g.CrRoot)
	cr.SlotIV = g.SlotIV{Function: g.DYN, Specification: g.OBJ, Context: g.EXS}
	f.Root = cr
	got := Formative(f)
	want := "-ml--DYN/OBJ/EXS"
	if got != want {
		t.Errorf("Formative(DYN/OBJ/EXS) = %q, want %q", got, want)
	}
}

func TestFormative_NonDefaultCa(t *testing.T) {
	// MSS configuration, G perspective, others default.
	f := g.MinimalFormative("ml")
	f.SlotVI = g.SlotVI{
		Configuration: g.MSS, Affiliation: g.CSL,
		Perspective: g.G_, Extension: g.DEL, Essence: g.NRM,
	}
	got := Formative(f)
	want := "-ml--MSS.G"
	if got != want {
		t.Errorf("Formative(MSS/G) = %q, want %q", got, want)
	}
}

func TestFormative_ErgCase(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.Final = g.UnframedNominal{Case: g.ERG}
	got := Formative(f)
	want := "-ml--ERG"
	if got != want {
		t.Errorf("Formative(ERG) = %q, want %q", got, want)
	}
}

func TestFormative_Verbal(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.Final = g.UnframedVerbal{Vk: g.Directive{}}
	got := Formative(f)
	want := "-ml--DIR-ULT"
	if got != want {
		t.Errorf("Formative(DIR) = %q, want %q", got, want)
	}
	// Non-default validation should appear.
	f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.INF}}
	got = Formative(f)
	want = "-ml--ASR/INF-ULT"
	if got != want {
		t.Errorf("Formative(ASR/INF) = %q, want %q", got, want)
	}
}

func TestFormative_SlotVIIIValence(t *testing.T) {
	// MinimalFormative defaults to UnframedNominal (nominal context),
	// so MoodScope glosses as a CaseScope label.
	f := g.MinimalFormative("ml")
	f.SlotVIII = g.VnCnValence{
		Valence:   g.PRL,
		MoodScope: g.FAC, // FAC/CCN suppressed
	}
	got := Formative(f)
	want := "-ml--PRL"
	if got != want {
		t.Errorf("Formative(PRL/FAC) = %q, want %q", got, want)
	}
	// Nominal context labels MoodScope as CaseScope: SUB ↔ CCA.
	f.SlotVIII = g.VnCnValence{
		Valence:   g.PRL,
		MoodScope: g.SUB,
	}
	got = Formative(f)
	want = "-ml--PRL.CCA"
	if got != want {
		t.Errorf("Formative(PRL/CCA) = %q, want %q", got, want)
	}
	// Verbal context labels the same MoodScope as Mood: SUB.
	f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
	got = Formative(f)
	want = "-ml--PRL.SUB-ASR-ULT"
	if got != want {
		t.Errorf("Formative(verbal PRL/SUB) = %q, want %q", got, want)
	}
}

func TestFormative_SlotVIIIAspect(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SlotVIII = g.VnCnAspect{
		Aspect:    g.RTR,
		MoodScope: g.SUB,
	}
	// Nominal context → CaseScope label.
	got := Formative(f)
	want := "-ml--RTR.CCA"
	if got != want {
		t.Errorf("Formative(nominal RTR/CCA) = %q, want %q", got, want)
	}
}

func TestFormative_FramedVerbal_SlotVIIIAsCaseScope(t *testing.T) {
	// Per §3.8.1, FRAMED-verbal formatives gloss Slot VIII Cn as
	// Case-Scope (not Mood). Only UNFRAMED verbal formatives (ultimate
	// stress) take the Mood reading.
	f := g.MinimalFormative("ml")
	f.Final = g.FramedVerbal{Case: g.THM}
	f.SlotVIII = g.VnCnValence{Valence: g.PRL, MoodScope: g.SUB}
	got := Formative(f)
	want := "-ml--PRL.CCA-ANT"
	if got != want {
		t.Errorf("Formative(framed PRL/SUB) = %q, want %q", got, want)
	}
}

func TestFormative_FinalTag(t *testing.T) {
	cases := []struct {
		name  string
		final g.Final
		want  string
	}{
		{"nominal", g.UnframedNominal{Case: g.THM}, "-ml-"},
		{"verbal", g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}, "-ml--ASR-ULT"},
		{"framed", g.FramedVerbal{Case: g.THM}, "-ml--ANT"},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.Final = c.final
		got := Formative(f)
		if got != c.want {
			t.Errorf("Formative(Final=%v) = %q, want %q", c.final, got, c.want)
		}
	}
}

func TestFormative_Concatenation(t *testing.T) {
	f := g.MinimalFormative("ml")
	t1 := g.Type1
	f.Concat = t1
	got := Formative(f)
	want := "T1--ml-"
	if got != want {
		t.Errorf("Formative(T1) = %q, want %q", got, want)
	}
}

func TestGlosser_RootWithLexicon(t *testing.T) {
	lex := loadLex(t)
	gl := &Glosser{Lex: lex}

	f := g.MinimalFormative("m")
	// S1/PRC is default, so it doesn't appear; the root for "m" at
	// stem 1 should show its lexicon meaning.
	got := gl.Formative(f)
	entry, ok := lex.Roots["m"]
	if !ok {
		t.Fatal("lexicon missing root \"m\"")
	}
	want := "-m-'" + entry.Stem1 + "'"
	if got != want {
		t.Errorf("Formative(m, with lex) = %q, want %q", got, want)
	}
}

func TestGlosser_RootDifferentStem(t *testing.T) {
	lex := loadLex(t)
	gl := &Glosser{Lex: lex}
	f := g.MinimalFormative("m")
	cr := f.Root.(g.CrRoot)
	cr.Stem = g.S3
	f.Root = cr
	got := gl.Formative(f)
	entry := lex.Roots["m"]
	want := "S3/PRC--m-'" + entry.Stem3 + "'"
	if got != want {
		t.Errorf("Formative(m, S3) = %q, want %q", got, want)
	}
}

func TestGlosser_AffixWithLexicon(t *testing.T) {
	lex := loadLex(t)
	gl := &Glosser{Lex: lex}
	f := g.MinimalFormative("m")
	// Add a Slot VII affix with consonant "b" and vowel "a" (degree 1).
	f.SlotVII = []g.Affix{{Type: g.Type1Affix, Degree: 1, Consonant: "b"}}
	got := gl.Formative(f)
	entry, ok := lex.Affixes["b"]
	if !ok {
		t.Fatal("lexicon missing affix \"b\"")
	}
	// "m" root has a meaning, affix shows ABBREV/degree.
	mEntry := lex.Roots["m"]
	want := "-m-'" + mEntry.Stem1 + "'-" + entry.Abbrev + "/1"
	if got != want {
		t.Errorf("Formative(m + b:a) = %q, want %q", got, want)
	}
}

func TestGlosser_CategoryValuedAffix(t *testing.T) {
	lex := loadLex(t)
	gl := &Glosser{Lex: lex}
	cases := []struct {
		cs       string
		atype    g.AffixType
		degree   int
		want     string
	}{
		{"bẓ", g.Type1Affix, 3, "MCS:SPC"},
		{"bž", g.Type1Affix, 1, "PHS:PCT"},
		{"mc", g.Type1Affix, 1, "AP1:RTR"},
		{"nļ", g.Type1Affix, 1, "IVL:ASR"},
		{"nļ", g.Type2Affix, 1, "IVL:OBS"}, // bracketed type-2 alternate
		{"ẓk", g.Type1Affix, 1, "VAL:MNO"},
		// Non-category-valued affix still shows degree
		{"b", g.Type1Affix, 1, "DEV/1"},
	}
	for _, c := range cases {
		f := g.MinimalFormative("m")
		f.SlotVII = []g.Affix{{Type: c.atype, Degree: c.degree, Consonant: c.cs}}
		got := gl.Formative(f)
		if !strings.HasSuffix(got, "-"+c.want) {
			t.Errorf("affix %s type%d/%d: gloss %q does not end with -%s", c.cs, int(c.atype)+1, c.degree, got, c.want)
		}
	}
}

func TestGlosser_VariantAwareRootMeaning(t *testing.T) {
	lex := loadLex(t)
	gl := &Glosser{Lex: lex}
	tEntry := lex.Roots["t"]
	// Confirm fixture: "t" carries Objective alternates.
	if len(tEntry.Objective) != 3 || tEntry.Objective[0] == "" {
		t.Skipf("fixture root \"t\" lacks Objective variants: %+v", tEntry)
	}
	// Default spec (BSC) uses Stem(n).
	f := g.MinimalFormative("t")
	cr := f.Root.(g.CrRoot)
	cr.Stem = g.S1
	f.Root = cr
	got := gl.Formative(f)
	if !strings.Contains(got, "'"+tEntry.Stem1+"'") {
		t.Errorf("BSC gloss = %q, expected Stem1=%q", got, tEntry.Stem1)
	}
	// Spec=OBJ should pick the Objective alternate for the stem.
	cr.SlotIV.Specification = g.OBJ
	f.Root = cr
	got = gl.Formative(f)
	if !strings.Contains(got, "'"+tEntry.Objective[0]+"'") {
		t.Errorf("OBJ gloss = %q, expected Objective[0]=%q", got, tEntry.Objective[0])
	}
	// A root without the alternate should still fall back to Stem.
	mlEntry := lex.Roots["ml"]
	if len(mlEntry.Objective) > 0 {
		t.Skip("ml unexpectedly has Objective variants")
	}
	f2 := g.MinimalFormative("ml")
	cr2 := f2.Root.(g.CrRoot)
	cr2.Stem = g.S1
	cr2.SlotIV.Specification = g.OBJ
	f2.Root = cr2
	got2 := gl.Formative(f2)
	if !strings.Contains(got2, "'"+mlEntry.Stem1+"'") {
		t.Errorf("OBJ fallback gloss = %q, expected Stem1=%q", got2, mlEntry.Stem1)
	}
}

func TestGlosser_UnknownClusterFallsBack(t *testing.T) {
	lex := loadLex(t)
	gl := &Glosser{Lex: lex}
	f := g.MinimalFormative("zzzzz") // fictional root
	got := gl.Formative(f)
	want := "-zzzzz-"
	if got != want {
		t.Errorf("Formative(unknown root, with lex) = %q, want %q", got, want)
	}
}

func TestGlosser_NilLexiconBehavesLikePackageFn(t *testing.T) {
	f := g.MinimalFormative("ml")
	if (&Glosser{}).Formative(f) != Formative(f) {
		t.Error("Glosser{} should match package-level Formative")
	}
}

func TestFormative_Maleuţřait(t *testing.T) {
	// End-to-end: parse the canonical test word and gloss it.
	parsed, err := fullparse.ParseFormative("malëuţřait")
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	got := Formative(parsed)
	// Cr=m, two Slot VII affixes: ëu→Type2 degree 5 + ţř, ai→Type2
	// degree 1 + t. Type 2 emits a "₂" subscript in display mode (silent
	// in canonical mode); Type 1 stays silent in both modes.
	want := "-m--ţř/5₂-t/1₂"
	if got != want {
		t.Errorf("Formative(Malëuţřait) = %q, want %q", got, want)
	}
}
