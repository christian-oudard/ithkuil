package gloss_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/store"
)

// The seven category-valued affixes carry a category code in place of
// a degree meaning: MCS/1 is (SUB) Subjunctive, PHS/1 is (PCT)
// Punctual, and so on. Display mode shows the code; canonical mode
// must not, because "MCS:SUB" has the same shape as a §3.9.2
// accessor's "ACC1:INS" — three uppercase letters either side of a
// colon — and only a lexicon lookup tells them apart.
//
// While it did, every one of the seven failed the gloss-to-compose
// trip, which is a round-trip guarantee SPEC.md states outright. Two
// of them, AP1 and AP2, failed even before the accessors existed, so
// the collision was latent in the notation rather than introduced with
// them.
func TestCategoryValuedAffixesRoundTrip(t *testing.T) {
	st, err := store.Open(store.DefaultPath())
	if err != nil {
		t.Skip("no data store; run tools/build_db.py")
	}
	lex, err := lexicon.LoadFromStore(st)
	if err != nil {
		t.Fatal(err)
	}
	canonical := &gloss.Glosser{Lex: lex, Canonical: true}
	display := &gloss.Glosser{Lex: lex}

	for _, abbrev := range []string{"MCS", "PHS", "LVL", "VAL", "IVL", "AP1", "AP2"} {
		cs := csFor(lex, abbrev)
		if cs == "" {
			t.Errorf("no affix in the lexicon with abbreviation %q", abbrev)
			continue
		}
		f := g.MinimalFormative("ml")
		f.SlotVII = []g.Affix{{Type: g.Type1Affix, Degree: 1, Consonant: cs}}

		got := canonical.Formative(f)
		if _, err := gloss.ParseFormative(got, lex.Affixes); err != nil {
			t.Errorf("%s: canonical gloss %q does not compose back: %v", abbrev, got, err)
		}
		// Display mode is where the category code belongs, and it must
		// still be there — this is not a licence to drop it.
		if d := display.Formative(f); d == got {
			t.Errorf("%s: display gloss %q is the same as canonical; the category code was lost",
				abbrev, d)
		}
	}
}

func csFor(lex *lexicon.Lexicon, abbrev string) string {
	for cs, e := range lex.Affixes {
		if e.Abbrev == abbrev {
			return cs
		}
	}
	return ""
}
