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

// Options control orthographic choices that don't affect the grammar.
// All fields are false by default, giving the canonical long form with
// default elisions applied.
type Options struct {
	// Shortcut requests the Cc-Vv shortcut form when the formative
	// permits it (CrRoot with default SlotIV, encodable SlotVI, no
	// Slot V). When the formative isn't shortcut-encodable, the
	// option is silently ignored and long form is emitted.
	Shortcut bool
}

// Formative renders a formative to its canonical long-form surface
// string with default-value elisions applied.
func Formative(f g.Formative) string {
	return FormativeWithOpts(f, Options{})
}

// FormativeWithOpts renders a formative with the given options.
//
// Panics if f.Root or f.Final is nil — the zero value Formative{} is
// not a valid input. Construct via grammar.MinimalFormative or set
// Root and Final explicitly.
func FormativeWithOpts(f g.Formative, opts Options) string {
	return slots.Render(slots.FromGrammar(f, slots.Options{Shortcut: opts.Shortcut}))
}
