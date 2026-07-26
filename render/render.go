// Package render turns a grammar.Formative back into its surface text
// representation. Internally it composes Layer D inverse (FromGrammar)
// with Layer C inverse (Render) from the slots package, so all the
// shape-detection and elision logic lives in one place. This file
// keeps the legacy package-level entry points for callers.
package render

import (
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/slots"
)

// Formative renders a formative to its canonical surface string. The
// canonical form is uniquely determined by the grammar: of every legal
// spelling, the shortest wins (see slots.FromGrammar for the ranking),
// default-value elisions apply, and stress lands per §3.10. There is
// no knob — a Formative renders to one surface, full stop.
//
// Panics if f.Root or f.Final is nil — the zero value Formative{} is
// not a valid input. Construct via grammar.MinimalFormative or set
// Root and Final explicitly.
func Formative(f g.Formative) string {
	return slots.Render(slots.FromGrammar(f))
}
