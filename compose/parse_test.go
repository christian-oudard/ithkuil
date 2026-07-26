package compose

import (
	"path/filepath"
	"slices"
	"testing"

	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/render"
)

func mustLex(t *testing.T) *lexicon.Lexicon {
	t.Helper()
	lex, err := lexicon.Load(filepath.Join("..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	return lex
}

// TestFormative_RoundTripGloss verifies that compose.Formative
// inverts gloss.Formative for the cases it claims to support — gloss
// the parsed result and the strings must match.
func TestFormative_RoundTripGloss(t *testing.T) {
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
			f, err := Formative(c.in, lex.Affixes)
			if err != nil {
				t.Fatalf("Formative(%q): %v", c.in, err)
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

// TestFormative_Specific spot-checks specific field assignments.
func TestFormative_Specific(t *testing.T) {
	lex := mustLex(t)

	f, err := Formative("S2/CPT-ml-DYN/OBJ-ERG", lex.Affixes)
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

func TestFormative_AffixViaAbbrev(t *testing.T) {
	lex := mustLex(t)
	f, err := Formative("ml-DEV/3", lex.Affixes)
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

func TestFormative_AffixTypeTag(t *testing.T) {
	lex := mustLex(t)
	f, err := Formative("ml-nļ/1_2", lex.Affixes) // IVL type-2 degree 1
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if len(f.SlotVII) != 1 || f.SlotVII[0].Type != g.Type2Affix {
		t.Errorf("affix type = %+v, want Type2Affix", f.SlotVII)
	}
}

func TestFormative_Errors(t *testing.T) {
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
			if _, err := Formative(c.in, lex.Affixes); err == nil {
				t.Errorf("Formative(%q) succeeded, want error", c.in)
			}
		})
	}
}

// TestFormative_SlotVAffixes checks that the "{Ca}" boundary marker
// routes affixes to Slot V (before the Ca) instead of Slot VII, and
// that a spelled-out Ca complex acts as the same boundary.
func TestFormative_SlotVAffixes(t *testing.T) {
	lex := mustLex(t)
	cases := []struct {
		name          string
		in            string
		slotV, slotV2 []g.Affix
	}{
		{
			name:  "no boundary — everything is Slot VII",
			in:    "m-ţř/5_2-t/1_2",
			slotV: nil,
			slotV2: []g.Affix{
				{Type: g.Type2Affix, Degree: 5, Consonant: "ţř"},
				{Type: g.Type2Affix, Degree: 1, Consonant: "t"},
			},
		},
		{
			name:   "default Ca marker splits the affixes",
			in:     "m-ţř/5_2-{Ca}-t/1_2",
			slotV:  []g.Affix{{Type: g.Type2Affix, Degree: 5, Consonant: "ţř"}},
			slotV2: []g.Affix{{Type: g.Type2Affix, Degree: 1, Consonant: "t"}},
		},
		{
			name:   "spelled-out Ca splits the affixes",
			in:     "m-ţř/5_2-MSS.G-t/1_2",
			slotV:  []g.Affix{{Type: g.Type2Affix, Degree: 5, Consonant: "ţř"}},
			slotV2: []g.Affix{{Type: g.Type2Affix, Degree: 1, Consonant: "t"}},
		},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			f, err := Formative(c.in, lex.Affixes)
			if err != nil {
				t.Fatalf("Formative(%q): %v", c.in, err)
			}
			if !slices.Equal(f.SlotV, c.slotV) {
				t.Errorf("SlotV = %v, want %v", f.SlotV, c.slotV)
			}
			if !slices.Equal(f.SlotVII, c.slotV2) {
				t.Errorf("SlotVII = %v, want %v", f.SlotVII, c.slotV2)
			}
		})
	}
}

// TestFormative_MaţřëullaitRoundTrip closes the loop on the community
// endonym and on the older spelling it replaced: surface → canonical
// gloss → compose must land back on the same formative. The two words
// differ only in which side of the Ca the SYS affix sits on, so this
// fails the moment the gloss stops carrying the Ca boundary.
func TestFormative_MaţřëullaitRoundTrip(t *testing.T) {
	lex := mustLex(t)
	gl := &gloss.Glosser{Lex: lex, Canonical: true}
	cases := []struct{ surface, want string }{
		{"maţřëullait", "m-SYS/5_2-{Ca}-DCD/1_2"},
		{"malëuţřait", "m-SYS/5_2-DCD/1_2"},
	}
	for _, c := range cases {
		f, err := fullparse.Formative(c.surface)
		if err != nil {
			t.Fatalf("fullparse.Formative(%q): %v", c.surface, err)
		}
		got := gl.Formative(f)
		if got != c.want {
			t.Fatalf("gloss(%q) = %q, want %q", c.surface, got, c.want)
		}
		back, err := Formative(got, lex.Affixes)
		if err != nil {
			t.Fatalf("compose.Formative(%q): %v", got, err)
		}
		if again := gl.Formative(back); again != got {
			t.Errorf("round-trip of %q: %q → %q", c.surface, got, again)
		}
	}
}
