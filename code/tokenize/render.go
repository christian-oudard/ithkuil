package tokenize

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/render"
)

// Render writes a word back out as romanization.
//
// It closes the loop Tokenize opens: every class has a grammar type
// and a way back out of it, so a word built by compose or read from a
// serialized file can be said, not only glossed. Nothing here consults
// how the word was written, because a Word does not carry that; the
// romanization is derived from the grammar every time.
//
// The one word whose letters are not derived is Foreign, whose meaning
// genuinely is its letters.
func Render(w g.Word) (string, error) {
	switch v := w.(type) {
	case g.Formative:
		return render.Formative(v), nil
	case *g.Chain:
		parts := make([]string, 0, v.Length())
		for _, f := range v.Formatives() {
			parts = append(parts, render.Formative(f))
		}
		return strings.Join(parts, "-"), nil
	case g.Referential:
		return render.Referential(v)
	case g.CombinationReferential:
		return render.CombinationReferential(v)
	case g.Bias:
		return parse.BiasForm(v), nil
	case g.RegisterMarker:
		if v.End {
			return parse.RegisterFinalForm(v.Register), nil
		}
		return parse.RegisterInitialForm(v.Register), nil
	case g.CarrierAdjunct:
		return parse.CarrierTypeForm(v.Type) + parse.CaseToVc(v.Case), nil
	case g.ModularAdjunct:
		return render.ModularAdjunct(v)
	case g.SingleAffixAdjunct:
		return render.SingleAffixAdjunct(v)
	case g.MultipleAffixAdjunct:
		return render.MultipleAffixAdjunct(v)
	case g.Foreign:
		return v.Text, nil
	}
	return "", fmt.Errorf("no renderer for %T", w)
}

// RenderText writes a whole span back out, one word after another.
func RenderText(t g.Text) (string, error) {
	parts := make([]string, 0, len(t))
	for _, w := range t {
		s, err := Render(w)
		if err != nil {
			return "", err
		}
		parts = append(parts, s)
	}
	return strings.Join(parts, " "), nil
}
