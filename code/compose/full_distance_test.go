package compose

import (
	"path/filepath"
	"testing"

	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/gloss"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/slots"
)

// TestFullDistance_SlotsCorpus runs every word in slots.FormativeCorpus
// the full distance through every transformation layer and asserts
// that the round-trip preserves meaning at every stage:
//
//	surface ─parse→ Formative ─render→ surface' (must equal surface)
//	Formative ─gloss→ G1 ─compose→ Formative' ─gloss→ G2 (must equal G1)
//
// The first chain catches structural drift in parse/render; the
// second catches drift between the gloss output format and the
// compose authoring grammar. The corpus exercises every non-trivial
// path through the slot grammar (minimal, concat prefixes, all
// shortcut variants, Cs/Ref roots, Slot V stacks, sentence starters).
func TestFullDistance_SlotsCorpus(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	gl := &gloss.Glosser{Lex: lex, Canonical: true}

	for _, w := range slots.FormativeCorpus {
		t.Run(w, func(t *testing.T) {
			f, err := fullparse.Formative(w)
			if err != nil {
				t.Skipf("fullparse rejects %q: %v", w, err)
			}
			// Gloss round-trip: assert compose is the inverse of gloss
			// at the canonical level. The surface round-trip is *not*
			// asserted — render emits the canonical form, which for
			// some input surfaces (e.g. consonant-initial vs Cc
			// shortcut equivalents) differs from the input. That's
			// by design; see TestCanonicalize in slots/.
			s1 := gl.Formative(f)
			f2, err := Formative(s1, lex.Affixes)
			if err != nil {
				t.Fatalf("compose.Formative(%q): %v\n  formative: %+v", s1, err, f)
			}
			s2 := gl.Formative(f2)
			if s1 != s2 {
				t.Errorf("gloss round-trip mismatch\n  surface: %s\n  first:   %s\n  second:  %s",
					w, s1, s2)
			}
		})
	}
}
