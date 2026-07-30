package gloss

import (
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/roman"
	"strings"
	"testing"
)

// Corpus tests over concrete Ithkuil forms. Each test parses a form and
// asserts its gloss contains the intended morphological features.
//
// Most forms here come from translating "I will be home late tonight,
// don't wait for me, just sleep."

// glossOne deliberately runs without a lexicon so affixes show as
// "Cs/degree". This keeps the assertions structural rather than tied to
// specific lexicon entries.
func glossOne(t *testing.T, word string) string {
	gl := &Glosser{}
	w, err := roman.ParseWord(word)
	if err != nil {
		return "?" + word
	}
	return gl.Token(w)
}

func assertContains(t *testing.T, word, gloss, want string) {
	t.Helper()
	if !strings.Contains(gloss, want) {
		t.Errorf("gloss of %q = %q, missing %q", word, gloss, want)
	}
}

func TestCorpus_Completive_Prospective_Locative(t *testing.T) {
	g := glossOne(t, "ärmaläwi'a")
	assertContains(t, "ärmaläwi'a", g, "CPT")
	assertContains(t, "ärmaläwi'a", g, "PRS")
	assertContains(t, "ärmaläwi'a", g, "LOC")
}

func TestCorpus_Referential_1m_Thematic(t *testing.T) {
	g := glossOne(t, "la")
	assertContains(t, "la", g, "1m")
	assertContains(t, "la", g, "THM")
}

func TestCorpus_Completive_Directive(t *testing.T) {
	g := glossOne(t, "wäḑḑái")
	assertContains(t, "wäḑḑái", g, "CPT")
	assertContains(t, "wäḑḑái", g, "DIR")
}

func TestCorpus_Completive_Admonitive_Negation6(t *testing.T) {
	g := glossOne(t, "wäfsöróu")
	assertContains(t, "wäfsöróu", g, "CPT")
	assertContains(t, "wäfsöróu", g, "r/6")
	assertContains(t, "wäfsöróu", g, "ADM")
}

func TestCorpus_Referential_1m_Dative(t *testing.T) {
	g := glossOne(t, "lü")
	assertContains(t, "lü", g, "1m")
	assertContains(t, "lü", g, "DAT")
}

// Counter-example: malformed verbal forms must NOT parse as a Formative.
func TestCorpus_VerbalFormRequiresPrefix(t *testing.T) {
	tok, err := roman.ParseWord("aḑḑái")
	if err != nil {
		return // not read at all, which is stronger than not a formative
	}
	if _, isFormative := tok.(g.Formative); isFormative {
		t.Error("aḑḑái should not parse as a Formative; verbal forms need w-/y- prefix")
	}
}

// Stem and aspect variants of the same forms.

func TestCorpus_Stem3_Completive_Prospective_Locative(t *testing.T) {
	g := glossOne(t, "ürmaläwi'a")
	assertContains(t, "ürmaläwi'a", g, "S3")
	assertContains(t, "ürmaläwi'a", g, "CPT")
	assertContains(t, "ürmaläwi'a", g, "PRS")
	assertContains(t, "ürmaläwi'a", g, "LOC")
}

func TestCorpus_Stem3_Functive(t *testing.T) {
	g := glossOne(t, "wükmao")
	assertContains(t, "wükmao", g, "S3")
	assertContains(t, "wükmao", g, "FUN")
}

func TestCorpus_Stem2_Locative(t *testing.T) {
	g := glossOne(t, "ilnali'a")
	assertContains(t, "ilnali'a", g, "S2")
	assertContains(t, "ilnali'a", g, "LOC")
}

func TestCorpus_Stem3_Objective_Absolutive(t *testing.T) {
	g := glossOne(t, "ümtyile")
	assertContains(t, "ümtyile", g, "S3")
	assertContains(t, "ümtyile", g, "OBJ")
	assertContains(t, "ümtyile", g, "ABS")
}

func TestCorpus_Completive_Contential(t *testing.T) {
	g := glossOne(t, "äklläla")
	assertContains(t, "äklläla", g, "CPT")
	assertContains(t, "äklläla", g, "CTE")
}

func TestCorpus_Completive_Imminent_Potentiative_Size8(t *testing.T) {
	g := glossOne(t, "wämžwüxëiwói")
	assertContains(t, "wämžwüxëiwói", g, "CPT")
	assertContains(t, "wämžwüxëiwói", g, "x/8")
	assertContains(t, "wämžwüxëiwói", g, "IMM")
	assertContains(t, "wämžwüxëiwói", g, "POT")
}

// Affix-degree variants and affix-compressed forms.

func TestCorpus_Completive_Prospective_Locative_ShortTermTime6(t *testing.T) {
	g := glossOne(t, "ärmalöxbäwi'a")
	assertContains(t, "ärmalöxbäwi'a", g, "CPT")
	assertContains(t, "ärmalöxbäwi'a", g, "xb/6")
	assertContains(t, "ärmalöxbäwi'a", g, "PRS")
	assertContains(t, "ärmalöxbäwi'a", g, "LOC")
}

func TestCorpus_Admonitive_Negation4_Soft(t *testing.T) {
	g := glossOne(t, "wäfsiróu")
	assertContains(t, "wäfsiróu", g, "r/4")
	assertContains(t, "wäfsiróu", g, "ADM")
}

func TestCorpus_Admonitive_Negation8_Emphatic(t *testing.T) {
	g := glossOne(t, "wäčnüróu")
	assertContains(t, "wäčnüróu", g, "r/8")
	assertContains(t, "wäčnüróu", g, "ADM")
}

func TestCorpus_Imminent_Potentiative_Size7(t *testing.T) {
	g := glossOne(t, "wämžwoxëiwói")
	assertContains(t, "wämžwoxëiwói", g, "x/7")
	assertContains(t, "wämžwoxëiwói", g, "IMM")
	assertContains(t, "wämžwoxëiwói", g, "POT")
}

func TestCorpus_Functive_TemporalPlacement6(t *testing.T) {
	g := glossOne(t, "wafsöţpao")
	assertContains(t, "wafsöţpao", g, "t,p/6")
	assertContains(t, "wafsöţpao", g, "FUN")
}

func TestCorpus_Stem2_Contential(t *testing.T) {
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
		tok := readWord(t, c.word)
		b, ok := tok.(g.Bias)
		if !ok {
			t.Errorf("%q should parse as a BiasWord, got %T", c.word, tok)
			continue
		}
		if b.String() != c.biasAbbr {
			t.Errorf("%q parsed as %s, want %s", c.word, b, c.biasAbbr)
		}
	}
}

// readWord reads one word or fails the test.
func readWord(t *testing.T, word string) g.Word {
	t.Helper()
	w, err := roman.ParseWord(word)
	if err != nil {
		t.Fatalf("ClassifyWord(%q): %v", word, err)
	}
	return w
}
