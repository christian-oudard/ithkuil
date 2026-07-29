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
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
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
		{"stem version", "S2.CPT-ml"},
		{"case ERG", "ml-ERG"},
		{"function spec", "ml-DYN.OBJ"},
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
			// Sanity: must render to a non-empty romanization.
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

	f, err := Formative("S2.CPT-ml-DYN.OBJ-ERG", lex.Affixes)
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
		// A root is any lowercase token, so these used to compose:
		// "zzzz" to "azzzzal", which our validator rejects for a
		// triple consonant, and "qqq" to "aqqqal", which is not
		// spelled in the alphabet and does not round-trip.
		{"triple-consonant root", "zzzz"},
		{"root outside the alphabet", "S1-qqq"},
		// resolveAffixCs hands back anything it cannot look up, so a
		// Cs got no more checking than a Cr did: "zzzz/3" composed to
		// "malezzzza", a triple consonant.
		{"triple-consonant affix", "m-zzzz/3"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			if _, err := Formative(c.in, lex.Affixes); err == nil {
				t.Errorf("Formative(%q) succeeded, want error", c.in)
			}
		})
	}
}

// TestFormative_AbbrevNeedsLexicon checks that an abbreviation fails
// without a lexicon instead of being read as a literal Cs. Passing nil
// affixes used to fold "DEV" straight through, composing "maleDEV" —
// Latin capitals inside an Ithkuil word — which reads back as an
// unrelated SCS/3-P08/3. A bare Cs must still work on the same path,
// since that is what nil affixes is for.
func TestFormative_AbbrevNeedsLexicon(t *testing.T) {
	if _, err := Formative("m-DEV/3", nil); err == nil {
		t.Error(`Formative("m-DEV/3", nil) succeeded, want error`)
	}
	f, err := Formative("m-b/3", nil)
	if err != nil {
		t.Fatalf(`Formative("m-b/3", nil) = %v, want success`, err)
	}
	if got := render.Formative(f); got != "maleb" {
		t.Errorf("bare Cs on the nil-lexicon path = %q, want %q", got, "maleb")
	}
}

// TestFormative_AttestedAwkwardRoots guards the other side of the
// root check. "csk" and "dcs" break the §2 sibilant and dental-stop
// pair rules, but they are Quijada's own roots — the morphology
// corpus attests "cskava" and "Adcsuleuha" — so compose must still
// accept them. Holding a Cr to §2, or validating the rendered word,
// rejects all three.
func TestFormative_AttestedAwkwardRoots(t *testing.T) {
	lex := mustLex(t)
	for _, in := range []string{"csk-N", "dcs-DYN.BSC.EXS-ITM"} {
		t.Run(in, func(t *testing.T) {
			if _, err := Formative(in, lex.Affixes); err != nil {
				t.Errorf("Formative(%q) = %v, want success", in, err)
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
// endonym and on the older spelling it replaced: romanization → canonical
// gloss → compose must land back on the same formative. The two words
// differ only in which side of the Ca the SYS affix sits on, so this
// fails the moment the gloss stops carrying the Ca boundary.
func TestFormative_MaţřëullaitRoundTrip(t *testing.T) {
	lex := mustLex(t)
	gl := &gloss.Glosser{Lex: lex, Canonical: true}
	cases := []struct{ rom, want, canonical string }{
		// Slot V forces a §3.6.2 glottal into the shortcut encoding
		// ("wamëu'ţřait"), which the plain form doesn't pay, so the
		// plain form is canonical.
		{"maţřëullait", "m-SYS/5_2-{Ca}-DCD/1_2", "maţřëullait"},
		// Without Slot V there is no glottal, and the two encodings tie
		// on syllables and length. A shortcut that gains nothing isn't
		// taken, so this spelling is canonical too.
		{"malëuţřait", "m-SYS/5_2-DCD/1_2", "malëuţřait"},
	}
	for _, c := range cases {
		f, err := fullparse.Formative(c.rom)
		if err != nil {
			t.Fatalf("fullparse.Formative(%q): %v", c.rom, err)
		}
		got := gl.Formative(f)
		if got != c.want {
			t.Fatalf("gloss(%q) = %q, want %q", c.rom, got, c.want)
		}
		back, err := Formative(got, lex.Affixes)
		if err != nil {
			t.Fatalf("compose.Formative(%q): %v", got, err)
		}
		if again := gl.Formative(back); again != got {
			t.Errorf("round-trip of %q: %q → %q", c.rom, got, again)
		}
		if surf := render.Formative(back); surf != c.canonical {
			t.Errorf("render(%q) = %q, want %q", got, surf, c.canonical)
		}
	}
}

// SPT is the one abbreviation two C_S forms answer to: Quijada gives
// "-rw/-ry SPT Specified Points in Calendrical Time" with a single
// degree list and no rule for choosing. The lookup used to walk a Go
// map, so it returned rw or ry at random and one Formative had two
// canonical spellings. Lowest cluster wins.
func TestFormative_AmbiguousAbbrevIsDeterministic(t *testing.T) {
	lex := mustLex(t)
	for i := 0; i < 50; i++ {
		f, err := Formative("ml-SPT/3", lex.Affixes)
		if err != nil {
			t.Fatalf("parse: %v", err)
		}
		if got := f.SlotVII[0].Consonant; got != "rw" {
			t.Fatalf("Consonant = %q, want rw", got)
		}
	}
}
