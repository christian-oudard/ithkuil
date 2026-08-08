package roman

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
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
func Word(w g.Word) (string, error) {
	switch v := w.(type) {
	case g.Formative:
		// A Cc marker says "another formative follows", so it belongs to
		// a chain and never to a formative standing alone. Writing one
		// anyway produced a string no reader accepts: "hafçal" comes
		// back as an affixual adjunct whose scope consonant is missing,
		// because a lone h- is not how any word starts. serialize
		// refuses the same value for the same reason; the two arms
		// should not disagree about what is a word.
		if v.Concat != g.ConcatNone {
			return "", fmt.Errorf("lone formative carries concatenation status %v; write it as a chain", v.Concat)
		}
		return Formative(v), nil
	case *g.Chain:
		parts := make([]string, 0, v.Length())
		for _, f := range v.Formatives() {
			parts = append(parts, Formative(f))
		}
		return strings.Join(parts, "-"), nil
	case g.Referential:
		return Referential(v)
	case g.CombinationReferential:
		return CombinationReferential(v)
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
		return ModularAdjunct(v)
	case g.SingleAffixAdjunct:
		return SingleAffixAdjunct(v)
	case g.MultipleAffixAdjunct:
		return MultipleAffixAdjunct(v)
	case g.Foreign:
		return v.Text, nil
	}
	return "", fmt.Errorf("no renderer for %T", w)
}

// Text writes a whole span back out, one word after another.
func Text(t g.Text) (string, error) {
	parts := make([]string, 0, len(t))
	for _, w := range t {
		s, err := Word(w)
		if err != nil {
			return "", err
		}
		parts = append(parts, s)
	}
	return strings.Join(parts, " "), nil
}
