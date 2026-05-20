package gloss

import (
	"path/filepath"
	"testing"

	"github.com/coudard/ithkuil/go/fullparse"
	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/lexicon"
)

func dataPath(name string) string {
	return filepath.Join("..", "..", "data", name)
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
	f.SlotII = g.SlotII{Stem: g.S2, Version: g.CPT}
	got := Formative(f)
	want := "S2/CPT--ml-"
	if got != want {
		t.Errorf("Formative(S2/CPT) = %q, want %q", got, want)
	}
}

func TestFormative_NonDefaultSlotIV(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SlotIV = g.SlotIV{Function: g.DYN, Specification: g.OBJ, Context: g.EXS}
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
	f.SlotIX = g.CaseSlot{Case: g.ERG}
	got := Formative(f)
	want := "-ml--ERG"
	if got != want {
		t.Errorf("Formative(ERG) = %q, want %q", got, want)
	}
}

func TestFormative_Verbal(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.Stress = g.Ultimate
	f.SlotIX = g.Directive{}
	got := Formative(f)
	want := "-ml--DIR-ULT"
	if got != want {
		t.Errorf("Formative(DIR) = %q, want %q", got, want)
	}
	// Non-default validation should appear.
	f.SlotIX = g.Assertive{Validation: g.INF}
	got = Formative(f)
	want = "-ml--ASR/INF-ULT"
	if got != want {
		t.Errorf("Formative(ASR/INF) = %q, want %q", got, want)
	}
}

func TestFormative_SlotVIIIValence(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SlotVIII = g.VnCnValence{
		Valence: g.PRL,
		MS:      g.MoodVal{Mood: g.FAC}, // FAC suppressed
	}
	got := Formative(f)
	want := "-ml--PRL"
	if got != want {
		t.Errorf("Formative(PRL/FAC) = %q, want %q", got, want)
	}
	// With non-default mood:
	f.SlotVIII = g.VnCnValence{
		Valence: g.PRL,
		MS:      g.MoodVal{Mood: g.SUB},
	}
	got = Formative(f)
	want = "-ml--PRL.SUB"
	if got != want {
		t.Errorf("Formative(PRL/SUB) = %q, want %q", got, want)
	}
}

func TestFormative_SlotVIIIAspect(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SlotVIII = g.VnCnAspect{
		Aspect: g.RTR,
		MS:     g.CaseScopeVal{CaseScope: g.CCA},
	}
	got := Formative(f)
	want := "-ml--RTR.CCA"
	if got != want {
		t.Errorf("Formative(RTR/CCA) = %q, want %q", got, want)
	}
}

func TestFormative_SentenceStarter(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SentenceStarter = true
	got := Formative(f)
	if got != "§ -ml-" {
		t.Errorf("Formative(sentence-starter) = %q, want \"§ -ml-\"", got)
	}
}

func TestFormative_StressTag(t *testing.T) {
	cases := []struct {
		stress g.Stress
		want   string
	}{
		{g.Penultimate, "-ml-"},
		{g.Monosyllabic, "-ml--MONO"},
		{g.Ultimate, "-ml--ULT"},
		{g.Antepenultimate, "-ml--ANT"},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.Stress = c.stress
		got := Formative(f)
		if got != c.want {
			t.Errorf("Formative(Stress=%v) = %q, want %q", c.stress, got, c.want)
		}
	}
}

func TestFormative_Concatenation(t *testing.T) {
	f := g.MinimalFormative("ml")
	t1 := g.Type1
	f.SlotI = &t1
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
	want := "-m- '" + entry.Stem1 + "'"
	if got != want {
		t.Errorf("Formative(m, with lex) = %q, want %q", got, want)
	}
}

func TestGlosser_RootDifferentStem(t *testing.T) {
	lex := loadLex(t)
	gl := &Glosser{Lex: lex}
	f := g.MinimalFormative("m")
	f.SlotII = g.SlotII{Stem: g.S3, Version: g.PRC}
	got := gl.Formative(f)
	entry := lex.Roots["m"]
	want := "S3/PRC--m- '" + entry.Stem3 + "'"
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
	want := "-m- '" + mEntry.Stem1 + "'-" + entry.Abbrev + "/1"
	if got != want {
		t.Errorf("Formative(m + b:a) = %q, want %q", got, want)
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
	// Cr=m, two Slot VII affixes: ëu→type2 degree 5 + ţř, ai→type2
	// degree 1 + t. Without a lexicon the affix shows as Cs/degree.
	want := "-m--ţř/5-t/1"
	if got != want {
		t.Errorf("Formative(Malëuţřait) = %q, want %q", got, want)
	}
}
