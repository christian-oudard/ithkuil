package gloss

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/tokenize"
)

// Corpus tests over concrete Ithkuil forms. Each test parses a form and
// asserts its gloss contains the intended morphological features.
//
// Most forms here come from translating "I will be home late tonight,
// don't wait for me, just sleep."

// glossOne deliberately runs without a lexicon so affixes show as
// "Cs/degree". This keeps the assertions structural rather than tied to
// specific lexicon entries.
func glossOne(_ *testing.T, word string) string {
	gl := &Glosser{}
	return gl.Token(tokenize.ClassifyWord(word))
}

func assertContains(t *testing.T, word, gloss, want string) {
	t.Helper()
	if !strings.Contains(gloss, want) {
		t.Errorf("gloss of %q = %q, missing %q", word, gloss, want)
	}
}

func TestCorpus_Ärmaläwia(t *testing.T) {
	g := glossOne(t, "ärmaläwi'a")
	assertContains(t, "ärmaläwi'a", g, "CPT")
	assertContains(t, "ärmaläwi'a", g, "PRS")
	assertContains(t, "ärmaläwi'a", g, "LOC")
}

func TestCorpus_La(t *testing.T) {
	g := glossOne(t, "la")
	assertContains(t, "la", g, "1m")
	assertContains(t, "la", g, "THM")
}

func TestCorpus_WäḑḑáiSleep(t *testing.T) {
	g := glossOne(t, "wäḑḑái")
	assertContains(t, "wäḑḑái", g, "CPT")
	assertContains(t, "wäḑḑái", g, "DIR")
}

func TestCorpus_Wäfsöróu(t *testing.T) {
	g := glossOne(t, "wäfsöróu")
	assertContains(t, "wäfsöróu", g, "CPT")
	assertContains(t, "wäfsöróu", g, "r/6")
	assertContains(t, "wäfsöróu", g, "ADM")
}

func TestCorpus_Lü(t *testing.T) {
	g := glossOne(t, "lü")
	assertContains(t, "lü", g, "1m")
	assertContains(t, "lü", g, "DAT")
}

// Counter-example: malformed verbal forms must NOT parse as a Formative.
func TestCorpus_Malformed_aḑḑái(t *testing.T) {
	tok := tokenize.ClassifyWord("aḑḑái")
	if _, isFormative := tok.(tokenize.FormativeWord); isFormative {
		t.Error("aḑḑái should not parse as a Formative; verbal forms need w-/y- prefix")
	}
}

// Stem and aspect variants of the same forms.

func TestCorpus_Ürmaläwia(t *testing.T) {
	g := glossOne(t, "ürmaläwi'a")
	assertContains(t, "ürmaläwi'a", g, "S3")
	assertContains(t, "ürmaläwi'a", g, "CPT")
	assertContains(t, "ürmaläwi'a", g, "PRS")
	assertContains(t, "ürmaläwi'a", g, "LOC")
}

func TestCorpus_Wükmao(t *testing.T) {
	g := glossOne(t, "wükmao")
	assertContains(t, "wükmao", g, "S3")
	assertContains(t, "wükmao", g, "FUN")
}

func TestCorpus_Ilnalia(t *testing.T) {
	g := glossOne(t, "ilnali'a")
	assertContains(t, "ilnali'a", g, "S2")
	assertContains(t, "ilnali'a", g, "LOC")
}

func TestCorpus_Ümtyile(t *testing.T) {
	g := glossOne(t, "ümtyile")
	assertContains(t, "ümtyile", g, "S3")
	assertContains(t, "ümtyile", g, "OBJ")
	assertContains(t, "ümtyile", g, "ABS")
}

func TestCorpus_Äklläla(t *testing.T) {
	g := glossOne(t, "äklläla")
	assertContains(t, "äklläla", g, "CPT")
	assertContains(t, "äklläla", g, "CTE")
}

func TestCorpus_Wämžwüxëiwói(t *testing.T) {
	g := glossOne(t, "wämžwüxëiwói")
	assertContains(t, "wämžwüxëiwói", g, "CPT")
	assertContains(t, "wämžwüxëiwói", g, "x/8")
	assertContains(t, "wämžwüxëiwói", g, "IMM")
	assertContains(t, "wämžwüxëiwói", g, "POT")
}

// Affix-degree variants and affix-compressed forms.

func TestCorpus_Ärmalöxbäwia(t *testing.T) {
	g := glossOne(t, "ärmalöxbäwi'a")
	assertContains(t, "ärmalöxbäwi'a", g, "CPT")
	assertContains(t, "ärmalöxbäwi'a", g, "xb/6")
	assertContains(t, "ärmalöxbäwi'a", g, "PRS")
	assertContains(t, "ärmalöxbäwi'a", g, "LOC")
}

func TestCorpus_Wäfsiróu_SoftNeg(t *testing.T) {
	g := glossOne(t, "wäfsiróu")
	assertContains(t, "wäfsiróu", g, "r/4")
	assertContains(t, "wäfsiróu", g, "ADM")
}

func TestCorpus_Wäčnüróu_EmphaticNeg(t *testing.T) {
	g := glossOne(t, "wäčnüróu")
	assertContains(t, "wäčnüróu", g, "r/8")
	assertContains(t, "wäčnüróu", g, "ADM")
}

func TestCorpus_Wämžwoxëiwói(t *testing.T) {
	g := glossOne(t, "wämžwoxëiwói")
	assertContains(t, "wämžwoxëiwói", g, "x/7")
	assertContains(t, "wämžwoxëiwói", g, "IMM")
	assertContains(t, "wämžwoxëiwói", g, "POT")
}

func TestCorpus_Wafsöţpao(t *testing.T) {
	g := glossOne(t, "wafsöţpao")
	assertContains(t, "wafsöţpao", g, "ţp/6")
	assertContains(t, "wafsöţpao", g, "FUN")
}

func TestCorpus_Iklläla(t *testing.T) {
	g := glossOne(t, "iklläla")
	assertContains(t, "iklläla", g, "S2")
	assertContains(t, "iklläla", g, "CTE")
}

// Mood vs CaseScope disambiguation. Ultimate stress (verbs) get Mood;
// other stresses (nouns) get CaseScope.
func TestCorpus_DisambiguateSUB_CCA(t *testing.T) {
	gVerb := glossOne(t, "agulahlá") // ultimate stress: SUB mood
	assertContains(t, "agulahlá", gVerb, "SUB")
	gNoun := glossOne(t, "agulahla") // penultimate: CCA case-scope
	assertContains(t, "agulahla", gNoun, "CCA")
}

func TestCorpus_FramedVerb_ANT(t *testing.T) {
	// Antepenultimate stress = framed verb → ANT tag in gloss.
	g := glossOne(t, "ágala")
	assertContains(t, "ágala", g, "ANT")
}

func TestCorpus_FramedVerb_MoodNotScope(t *testing.T) {
	// Framed verb with VnCn ("arţtúliwa") uses Mood (FAC, suppressed),
	// not CaseScope (CCN). The ANT tag should appear.
	g := glossOne(t, "arţtúliwa")
	assertContains(t, "arţtúliwa", g, "ANT")
	// PRG aspect should appear (from Vn "i" + Cn "w").
	assertContains(t, "arţtúliwa", g, "PRG")
}

func TestCorpus_BiasAdjuncts(t *testing.T) {
	cases := []struct {
		word     string
		biasAbbr string
	}{
		{"řs", "APB"},
		{"ňňs", "SOL"},
		{"lst", "ANP"},
		{"mmh", "GRT"},
	}
	for _, c := range cases {
		tok := tokenize.ClassifyWord(c.word)
		b, ok := tok.(tokenize.BiasWord)
		if !ok {
			t.Errorf("%q should parse as a BiasWord, got %T", c.word, tok)
			continue
		}
		if b.Bias.String() != c.biasAbbr {
			t.Errorf("%q parsed as %s, want %s", c.word, b.Bias, c.biasAbbr)
		}
	}
}
