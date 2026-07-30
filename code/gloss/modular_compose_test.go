package gloss_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/roman"
)

// A modular adjunct's gloss does not read back. Every one of the three
// distinct modular glosses in the corpus fails, so the whole class is
// write-only through this arm:
//
//	a           RTR                        no root in "RTR"
//	ä           PRS                        no root in "PRS"
//	wähňainui   PRL.HYP-RSM-IRP-{parent}   root "{parent}": non-Ithkuil characters
//
// Three separate holes in the recogniser, not one:
//
//   - A lone-aspect modular glosses to a bare category abbreviation.
//     looksLikeModular takes that shape only when a scope or reach tail
//     was stripped first, on the reasoning that the bare-uppercase
//     dispatch has already claimed it — but that dispatch tries bias
//     and register, and a Vn category is neither, so it falls through
//     to the formative parser.
//   - A multi-pair modular puts its slots in hyphen-separated fields,
//     and the scope tail is trimmed before the body is split, so
//     "{parent}" is left looking like one more slot.
//   - An all-default modular glosses to "MOD", which composes to a
//     value the renderer then refuses, §4.3 Slot 4 being mandatory.
//     Either the gloss should not emit it or the renderer should
//     supply the default V_N; the two disagree about whether such a
//     word exists.
//
// The fix is not simply to widen looksLikeModular: a bare "RTR" is
// shape-identical to a bias or register abbreviation, and the
// one-job-per-mark rule in SPEC says a token's kind should follow from
// its shape. Deciding it by consulting three inventories in order is
// what the rule exists to avoid, so the gloss for a lone-aspect
// modular probably needs a mark of its own.
func TestModularAdjunct_GlossDoesNotCompose(t *testing.T) {
	t.Skip("a modular adjunct's gloss does not read back; see the comment above and BUGS.md")

	gl := &gloss.Glosser{Canonical: true}
	for _, w := range corpus.Words() {
		word, err := roman.ParseWord(w)
		if err != nil {
			continue
		}
		m, ok := word.(g.ModularAdjunct)
		if !ok {
			continue
		}
		s := gl.Word(m, g.Text{m}, 0)
		if _, err := gloss.ParseWord(s, nil); err != nil {
			t.Errorf("%q glosses to %q, which does not compose: %v", w, s, err)
		}
	}
}
