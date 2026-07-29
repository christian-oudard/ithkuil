package tokenize

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/render"
)

// Render turns any classified word back to its romanization.
//
// It closes the loop tokenize opens: every word class now has a
// grammar type and a way back out of it, so a token built by compose
// or read out of a serialized file can be spoken, not just glossed. A
// token's own Text field is not consulted — it records what was typed,
// which for a synthesized token is nothing, and the point here is to
// derive the romanization from the grammar.
//
// The one exception is a foreign word, whose meaning genuinely is its
// letters.
func Render(t WordToken) (string, error) {
	switch v := t.(type) {
	case FormativeWord:
		return render.Formative(v.Formative), nil
	case ConcatenatedFormativeWord:
		parts := make([]string, 0, v.Chain.Length())
		for _, f := range v.Chain.Formatives() {
			parts = append(parts, render.Formative(f))
		}
		return strings.Join(parts, "-"), nil
	case ReferentialWord:
		return render.Referential(v.Referential)
	case CombinationRefWord:
		return render.CombinationReferential(v.Combination)
	case BiasWord:
		return g.BiasForm(v.Bias), nil
	case RegisterStartWord:
		return g.RegisterInitialForm(v.Register), nil
	case RegisterEndWord:
		return g.RegisterFinalForm(v.Register), nil
	case CarrierWord:
		return g.CarrierTypeForm(v.Carrier.Type) + g.CaseToVc(v.Carrier.Case), nil
	case ForeignWord:
		return v.Text, nil
	}
	return "", fmt.Errorf("no renderer for %T", t)
}
