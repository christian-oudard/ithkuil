package dictionary

import (
	"path/filepath"
	"reflect"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
)

func TestHeadwords(t *testing.T) {
	cases := []struct {
		gloss string
		want  []string
	}{
		{"crisis", []string{"crisis"}},
		{"be/equivalence", []string{"be", "equivalence"}},
		{"to speak", []string{"speak"}},
		{"(to be) a country", []string{"a country"}},
		{"hagfish (genera Rubicundus, Eptatretus)", []string{"hagfish"}},
		{"this (non-epistemological)", []string{"this"}},
		{"[carrier root]", nil},
		{"case scope †", []string{"case scope"}},
		{"trout; salmon", []string{"trout", "salmon"}},
		{"a river, stream, or creek", []string{"a river, stream, or creek"}},
		{"  Mixed   Case  ", []string{"mixed case"}},
	}
	for _, c := range cases {
		got := Headwords(c.gloss)
		if !reflect.DeepEqual(got, c.want) {
			t.Errorf("Headwords(%q) = %q, want %q", c.gloss, got, c.want)
		}
	}
}

// fakeRoots is a two-entry lexicon exercising the plain stems and the
// sparse variant columns.
func fakeRoots() map[string]lexicon.RootEntry {
	return map[string]lexicon.RootEntry{
		"m": {
			Cr:    "m",
			Stem0: "speech",
			Stem1: "to speak; utterance",
			Stem2: "word",
			Stem3: "sentence",
		},
		"kt": {
			Cr:           "kt",
			Stem0:        "river",
			Stem1:        "river",
			Stem2:        "stream",
			Stem3:        "creek",
			Contential:   "flowing water",
			Constitutive: "riverbed",
			Objective:    []string{"the river as object", "", ""},
			Completive:   []string{"", "a finished stream", ""},
			Dynamic:      "to flow",
		},
	}
}

func TestBuildAndLookup(t *testing.T) {
	ix := Build(fakeRoots())

	senses := ix.Lookup("Utterance")
	if len(senses) != 1 {
		t.Fatalf("Lookup(utterance) = %d senses, want 1", len(senses))
	}
	s := senses[0]
	if s.Cr != "m" || s.Stem != g.S1 {
		t.Errorf("utterance = %s/%s, want m/S1", s.Cr, s.Stem)
	}
	if s.Gloss != "to speak; utterance" {
		t.Errorf("Gloss = %q, want the whole source cell", s.Gloss)
	}

	if got := len(ix.Lookup("river")); got != 2 {
		t.Errorf("Lookup(river) = %d senses, want 2 (S0, S1)", got)
	}
	if got := ix.Lookup("nonesuch"); got != nil {
		t.Errorf("Lookup(nonesuch) = %v, want nil", got)
	}
}

func TestSenseCoordinates(t *testing.T) {
	ix := Build(fakeRoots())
	cases := []struct {
		word string
		want Sense
	}{
		{"flowing water", Sense{Cr: "kt", Stem: g.S0, SlotIV: g.SlotIV{Specification: g.CTE}}},
		{"riverbed", Sense{Cr: "kt", Stem: g.S0, SlotIV: g.SlotIV{Specification: g.CSV}}},
		{"the river as object", Sense{Cr: "kt", Stem: g.S1, SlotIV: g.SlotIV{Specification: g.OBJ}}},
		{"a finished stream", Sense{Cr: "kt", Stem: g.S2, Version: g.CPT}},
		{"flow", Sense{Cr: "kt", Stem: g.S0, SlotIV: g.SlotIV{Function: g.DYN}}},
	}
	for _, c := range cases {
		senses := ix.Lookup(c.word)
		if len(senses) != 1 {
			t.Fatalf("Lookup(%q) = %d senses, want 1", c.word, len(senses))
		}
		got := senses[0]
		got.Gloss = ""
		if got != c.want {
			t.Errorf("Lookup(%q) = %+v, want %+v", c.word, got, c.want)
		}
	}
}

func TestSenseFormative(t *testing.T) {
	ix := Build(fakeRoots())
	s := ix.Lookup("stream")[0]
	f := s.Formative()
	root, ok := f.Root.(g.CrRoot)
	if !ok {
		t.Fatalf("Formative().Root is %T, want CrRoot", f.Root)
	}
	if root.Cluster != "kt" || root.Stem != g.S2 {
		t.Errorf("Formative() root = %s/%s, want kt/S2", root.Cluster, root.Stem)
	}
	if f.Final == nil {
		t.Error("Formative().Final is nil; a Formative must be renderable")
	}
}

// The senses of one headword are ordered deterministically, so CLI and
// MCP output does not shuffle between runs.
func TestLookupIsOrdered(t *testing.T) {
	ix := Build(fakeRoots())
	first := ix.Lookup("river")
	for range 20 {
		if !reflect.DeepEqual(ix.Lookup("river"), first) {
			t.Fatal("Lookup order is not stable")
		}
	}
}

func TestRealLexicon(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "data", "data.json"))
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	ix := Build(lex.Roots)
	if len(ix) < 20000 {
		t.Errorf("index has %d headwords, expected 20k+", len(ix))
	}
	for _, w := range []string{"crisis", "trout", "version"} {
		if len(ix.Lookup(w)) == 0 {
			t.Errorf("Lookup(%q) found nothing", w)
		}
	}
	// Bracketed placeholder glosses are not headwords.
	if len(ix.Lookup("carrier root")) != 0 {
		t.Error("bracketed placeholder leaked into the index")
	}
}
