package roman

import (
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/slots"
)

// Formative decodes a single romanized Ithkuil word into a grammar.Formative.
// Returns a descriptive error if the word doesn't match a recognized
// formative shape.
func ParseFormative(word string) (g.Formative, error) {
	l, err := slots.Parse(word)
	if err != nil {
		return g.Formative{}, err
	}
	return slots.ToGrammar(l)
}
