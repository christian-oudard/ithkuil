// Package fullparse turns a surface Ithkuil word into a grammar.Formative
// by composing layout.Parse (Layer C) and layout.ToGrammar (Layer D).
package fullparse

import (
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/layout"
)

// ParseFormative decodes a single surface Ithkuil word into a Formative.
// Returns a descriptive error if the word doesn't match a recognized
// formative shape.
func ParseFormative(word string) (g.Formative, error) {
	l, err := layout.Parse(word)
	if err != nil {
		return g.Formative{}, err
	}
	return layout.ToGrammar(l)
}
