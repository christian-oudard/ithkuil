package gloss

import (
	"path/filepath"
	"strings"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/roman"
)

func dataPath(name string) string {
	return filepath.Join("..", "..", "data", name)
}

func loadLex(t *testing.T) *lexicon.Lexicon {
	t.Helper()
	lex, err := lexicon.Load(dataPath("data.json"))
	if err != nil {
		t.Fatalf("lexicon load: %v", err)
	}
	return lex
}

func TestFormative_Minimal(t *testing.T) {
	// MinimalFormative("ml") is all defaults except the root.
	// Gloss should just be "ml".
	f := g.MinimalFormative("ml")
	got := Formative(f)
	want := "ml"
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
	want := "S2.CPT-ml"
	if got != want {
		t.Errorf("Formative(S2.CPT) = %q, want %q", got, want)
	}
}

func TestFormative_NonDefaultSlotIV(t *testing.T) {
	f := g.MinimalFormative("ml")
	cr := f.Root.(g.CrRoot)
	cr.SlotIV = g.SlotIV{Function: g.DYN, Specification: g.OBJ, Context: g.EXS}
	f.Root = cr
	got := Formative(f)
	want := "ml-DYN.OBJ.EXS"
	if got != want {
		t.Errorf("Formative(DYN.OBJ.EXS) = %q, want %q", got, want)
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
	want := "ml-MSS.G"
	if got != want {
		t.Errorf("Formative(MSS/G) = %q, want %q", got, want)
	}
}

func TestFormative_ErgCase(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.Final = g.UnframedNominal{Case: g.ERG}
	got := Formative(f)
	want := "ml-ERG"
	if got != want {
		t.Errorf("Formative(ERG) = %q, want %q", got, want)
	}
}

func TestFormative_Verbal(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.Final = g.UnframedVerbal{Vk: g.Directive{}}
	got := Formative(f)
	want := "ml-DIR-ULT"
	if got != want {
		t.Errorf("Formative(DIR) = %q, want %q", got, want)
	}
	// Non-default validation should appear.
	f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.INF}}
	got = Formative(f)
	want = "ml-ASR.INF-ULT"
	if got != want {
		t.Errorf("Formative(ASR.INF) = %q, want %q", got, want)
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
	want := "ml-PRL"
	if got != want {
		t.Errorf("Formative(PRL/FAC) = %q, want %q", got, want)
	}
	// Nominal context labels MoodScope as CaseScope: SUB ↔ CCA.
	f.SlotVIII = g.VnCnValence{
		Valence:   g.PRL,
		MoodScope: g.SUB,
	}
	got = Formative(f)
	want = "ml-PRL.CCA"
	if got != want {
		t.Errorf("Formative(PRL/CCA) = %q, want %q", got, want)
	}
	// Verbal context labels the same MoodScope as Mood: SUB.
	f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
	got = Formative(f)
	want = "ml-PRL.SUB-ASR-ULT"
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
	want := "ml-RTR.CCA"
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
	want := "ml-PRL.CCA-ANT"
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
		{"nominal", g.UnframedNominal{Case: g.THM}, "ml"},
		{"verbal", g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}, "ml-ASR-ULT"},
		{"framed", g.FramedVerbal{Case: g.THM}, "ml-ANT"},
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
	want := "T1-ml"
	if got != want {
		t.Errorf("Formative(T1) = %q, want %q", got, want)
	}
}

// A root glosses to its cluster whether or not a lexicon is at hand.
// The gloss used to inline the English meaning when one was — "-m-
// 'linguistic utterance for communication'" — which made the gloss of
// a formative depend on which lexicon was loaded, and produced a
// string nothing could read back.
func TestGlosser_RootIsLexiconIndependent(t *testing.T) {
	f := g.MinimalFormative("m")
	withLex := (&Glosser{Lex: loadLex(t)}).Formative(f)
	withoutLex := (&Glosser{}).Formative(f)
	if withLex != withoutLex {
		t.Errorf("lexicon changed the gloss: %q with, %q without", withLex, withoutLex)
	}
	if withLex != "m" {
		t.Errorf("gloss = %q, want %q", withLex, "m")
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
	want := "S3.PRC-m"
	if got != want {
		t.Errorf("ParseFormative(m, S3) = %q, want %q", got, want)
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
	// The lexicon supplies the affix abbreviation; the degree follows
	// it, and the root stays the bare cluster.
	want := "m-" + entry.Abbrev + "/1"
	if got != want {
		t.Errorf("Formative(m + b:a) = %q, want %q", got, want)
	}
}

func TestGlosser_CategoryValuedAffix(t *testing.T) {
	lex := loadLex(t)
	gl := &Glosser{Lex: lex}
	cases := []struct {
		cs     string
		atype  g.AffixType
		degree int
		want   string
	}{
		{"bẓ", g.Type1Affix, 3, "MCS/3"},
		{"bž", g.Type1Affix, 1, "PHS/1"},
		{"mc", g.Type1Affix, 1, "AP1/1"},
		{"nļ", g.Type1Affix, 1, "IVL/1"},
		{"nļ", g.Type2Affix, 1, "IVL/1_2"}, // bracketed type-2 alternate
		{"ẓk", g.Type1Affix, 1, "VAL/1"},
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

func TestGlosser_UnknownClusterFallsBack(t *testing.T) {
	lex := loadLex(t)
	gl := &Glosser{Lex: lex}
	f := g.MinimalFormative("zzzzz") // fictional root
	got := gl.Formative(f)
	want := "zzzzz"
	if got != want {
		t.Errorf("ParseFormative(unknown root, with lex) = %q, want %q", got, want)
	}
}

func TestGlosser_NilLexiconBehavesLikePackageFn(t *testing.T) {
	f := g.MinimalFormative("ml")
	if (&Glosser{}).Formative(f) != Formative(f) {
		t.Error("Glosser{} should match package-level Formative")
	}
}

func TestFormative_CanonicalWord(t *testing.T) {
	// End-to-end: parse the canonical test word and gloss it, alongside
	// the older spelling it replaced. Cr=m in both; ţř/5 sits in Slot V
	// in the first and Slot VII in the second, and the "{Ca}" marker is
	// what keeps the two glosses apart. Type 2 emits "_2"; Type 1, the
	// default, stays silent.
	cases := []struct{ rom, want string }{
		{"maţřëullait", "m-t,rq/5_2-{Ca}-t/1_2"},
		{"malëuţřait", "m-t,rq/5_2-t/1_2"},
	}
	for _, c := range cases {
		parsed, err := roman.ParseFormative(c.rom)
		if err != nil {
			t.Fatalf("parse %q: %v", c.rom, err)
		}
		if got := Formative(parsed); got != c.want {
			t.Errorf("Formative(%s) = %q, want %q", c.rom, got, c.want)
		}
	}
}

// TestGlosser_SlotVDistinctFromSlotVII pins the Ca boundary. An affix
// in Slot V applies to the stem alone; the same affix in Slot VII has
// scope over the Ca complex. Positionally the gloss already separates
// them, but a default Ca is suppressed — so without an explicit marker
// the two formatives glossed identically. Slot V forces the Ca to be
// shown, as "{Ca}" when it holds nothing but defaults.
//
// This is the "maţřëullait" vs "malëuţřait" distinction.
func TestGlosser_SlotVDistinctFromSlotVII(t *testing.T) {
	gl := &Glosser{}
	affix := g.Affix{Type: g.Type2Affix, Degree: 5, Consonant: "ţř"}

	inner := g.MinimalFormative("m")
	inner.SlotV = []g.Affix{affix}
	outer := g.MinimalFormative("m")
	outer.SlotVII = []g.Affix{affix}

	gotInner, gotOuter := gl.Formative(inner), gl.Formative(outer)
	if gotInner == gotOuter {
		t.Fatalf("Slot V and Slot VII gloss identically: %q", gotInner)
	}
	if want := "m-t,rq/5_2-{Ca}"; gotInner != want {
		t.Errorf("Slot V gloss = %q, want %q", gotInner, want)
	}
	if want := "m-t,rq/5_2"; gotOuter != want {
		t.Errorf("Slot VII gloss = %q, want %q", gotOuter, want)
	}
}

// TestGlosser_SlotVWithNonDefaultCa checks that a Slot V affix does not
// add the "{Ca}" placeholder when the Ca already prints its own values.
func TestGlosser_SlotVWithNonDefaultCa(t *testing.T) {
	gl := &Glosser{}
	f := g.MinimalFormative("m")
	f.SlotV = []g.Affix{{Type: g.Type2Affix, Degree: 5, Consonant: "ţř"}}
	f.SlotVI = g.SlotVI{
		Configuration: g.MSS, Affiliation: g.CSL,
		Perspective: g.G_, Extension: g.DEL, Essence: g.NRM,
	}
	if want, got := "m-t,rq/5_2-MSS.G", gl.Formative(f); got != want {
		t.Errorf("Formative = %q, want %q", got, want)
	}
}
