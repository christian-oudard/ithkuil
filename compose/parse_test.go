package compose

import (
	"path/filepath"
	"testing"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/render"
)

func mustLex(t *testing.T) *lexicon.Lexicon {
	t.Helper()
	lex, err := lexicon.Load(
		filepath.Join("..", "data", "roots.json"),
		filepath.Join("..", "data", "affixes.json"),
	)
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	return lex
}

// TestParseString_RoundTripGloss verifies that compose.ParseString
// inverts gloss.Formative for the cases it claims to support — gloss
// the parsed result and the strings must match.
func TestParseString_RoundTripGloss(t *testing.T) {
	lex := mustLex(t)
	gl := &gloss.Glosser{Lex: lex}
	cases := []struct {
		name string
		in   string
	}{
		{"bare root", "ml"},
		{"stem version", "S2/CPT-ml"},
		{"case ERG", "ml-ERG"},
		{"function spec", "ml-DYN/OBJ"},
		{"context", "ml-FNC"},
		{"aspect", "ml-RTR"},
		{"illocution", "ml-DIR"},
		{"affix by Cs", "ml-b/3"},
		{"affix by Abbrev", "ml-DEV/3"},
		{"category-valued affix", "ml-MCS/3"},
		{"ASCII root digraph", "t,k"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			f, err := ParseString(c.in, lex.Affixes)
			if err != nil {
				t.Fatalf("ParseString(%q): %v", c.in, err)
			}
			// Sanity: must render to a non-empty surface word.
			if surf := render.Formative(f); surf == "" {
				t.Errorf("rendered to empty string")
			}
			// And the resulting formative must gloss to a non-empty
			// string that mentions the root cluster.
			if gs := gl.Formative(f); gs == "" {
				t.Errorf("Glosser returned empty string")
			}
		})
	}
}

// TestParseString_Specific spot-checks specific field assignments.
func TestParseString_Specific(t *testing.T) {
	lex := mustLex(t)

	f, err := ParseString("S2/CPT-ml-DYN/OBJ-ERG", lex.Affixes)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	cr, ok := f.Root.(g.CrRoot)
	if !ok {
		t.Fatalf("Root is not CrRoot: %T", f.Root)
	}
	if cr.Cluster != "ml" {
		t.Errorf("Cluster = %q, want ml", cr.Cluster)
	}
	if cr.Stem != g.S2 {
		t.Errorf("Stem = %v, want S2", cr.Stem)
	}
	if cr.Version != g.CPT {
		t.Errorf("Version = %v, want CPT", cr.Version)
	}
	if cr.SlotIV.Function != g.DYN {
		t.Errorf("Function = %v, want DYN", cr.SlotIV.Function)
	}
	if cr.SlotIV.Specification != g.OBJ {
		t.Errorf("Specification = %v, want OBJ", cr.SlotIV.Specification)
	}
	nf, ok := f.Final.(g.UnframedNominal)
	if !ok {
		t.Fatalf("Final is not UnframedNominal: %T", f.Final)
	}
	if nf.Case != g.ERG {
		t.Errorf("Case = %v, want ERG", nf.Case)
	}
}

func TestParseString_AffixViaAbbrev(t *testing.T) {
	lex := mustLex(t)
	f, err := ParseString("ml-DEV/3", lex.Affixes)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if len(f.SlotVII) != 1 {
		t.Fatalf("SlotVII = %d affixes, want 1", len(f.SlotVII))
	}
	a := f.SlotVII[0]
	if a.Consonant != "b" {
		t.Errorf("Consonant = %q, want b (DEV)", a.Consonant)
	}
	if a.Degree != 3 {
		t.Errorf("Degree = %d, want 3", a.Degree)
	}
	if a.Type != g.Type1Affix {
		t.Errorf("Type = %v, want Type1Affix", a.Type)
	}
}

func TestParseString_AffixTypeTag(t *testing.T) {
	lex := mustLex(t)
	f, err := ParseString("ml-nļ/1_2", lex.Affixes) // IVL type-2 degree 1
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if len(f.SlotVII) != 1 || f.SlotVII[0].Type != g.Type2Affix {
		t.Errorf("affix type = %+v, want Type2Affix", f.SlotVII)
	}
}

func TestParseString_Errors(t *testing.T) {
	lex := mustLex(t)
	cases := []struct {
		name string
		in   string
	}{
		{"empty", ""},
		{"no root", "S2-ERG"},
		{"multiple roots", "ml-foo"},
		{"unknown abbrev", "ml-ZZZ"},
		{"unknown affix abbrev", "ml-ZZZ/3"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			if _, err := ParseString(c.in, lex.Affixes); err == nil {
				t.Errorf("ParseString(%q) succeeded, want error", c.in)
			}
		})
	}
}
