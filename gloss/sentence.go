package gloss

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/referentials"
	"github.com/christian-oudard/ithkuil/semantics"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// Sentence runs the tokenizer over a sentence and returns one gloss
// string per recognized word.
func (gl *Glosser) Sentence(sentence string) []string {
	tokens := tokenize.Tokenize(sentence)
	out := make([]string, len(tokens))
	for i, t := range tokens {
		out[i] = gl.Token(t)
	}
	return out
}

// Sentence is the no-lexicon convenience wrapper.
func Sentence(sentence string) []string {
	return (&Glosser{}).Sentence(sentence)
}

// Token formats a single tokenize.WordToken. Each variant gets a
// concise, identifying gloss.
func (gl *Glosser) Token(t tokenize.WordToken) string {
	switch v := t.(type) {
	case tokenize.FormativeWord:
		return gl.Formative(v.Formative)
	case tokenize.ConcatenatedFormativeWord:
		parts := make([]string, 0, v.Chain.Length())
		for _, f := range v.Chain.Formatives() {
			parts = append(parts, gl.Formative(f))
		}
		return strings.Join(parts, " >> ")
	case tokenize.BiasWord:
		return biasLabel(v.Bias)
	case tokenize.RegisterStartWord:
		return "REG-" + v.Register.String()
	case tokenize.RegisterEndWord:
		return "REG-" + v.Register.String() + "-END"
	case tokenize.CarrierWord:
		return "CARR-" + v.Carrier.Type.String() + "(" + v.Carrier.Vc + ")"
	case tokenize.ModularWord:
		return modularLabel(v.Modular, v.MarksMood)
	case tokenize.SingleAffixWord:
		return singleAffixLabel(v.Affix)
	case tokenize.MultipleAffixWord:
		return multiAffixLabel(v.Affixes)
	case tokenize.ReferentialWord:
		return refLabel(v)
	case tokenize.CombinationRefWord:
		return combinationRefLabel(v)
	case tokenize.UnknownWord:
		return "?" + v.Text
	case tokenize.ForeignWord:
		return v.Text
	}
	return "?"
}

// singleAffixLabel formats a single-affix adjunct as "<Cs>/<deg><type>{<scope>}".
func singleAffixLabel(a g.SingleAffixAdjunct) string {
	t, d := parse.ClassifyAffixVowel(a.Vx)
	return fmt.Sprintf("AFFIX[%s/%d%s]{%s}", a.Cs, d, affixTypeSubscript(t), a.Scope.String())
}

// multiAffixLabel formats a multiple-affix adjunct as a chain of affixes
// with the first scope ({Cz}) and rest scope ({Vz}) attached.
func multiAffixLabel(a g.MultipleAffixAdjunct) string {
	parts := []string{fmt.Sprintf("%s/%s", a.First.Cs, a.First.Vx)}
	for _, p := range a.Affixes {
		parts = append(parts, fmt.Sprintf("%s/%s", p.Cs, p.Vx))
	}
	return fmt.Sprintf("AFFIXES[%s]{%s→%s}",
		strings.Join(parts, ","),
		a.FirstScope.String(), a.RestScope.String())
}

func affixTypeSubscript(t g.AffixType) string {
	switch t {
	case g.Type1Affix:
		return "₁"
	case g.Type2Affix:
		return "₂"
	case g.Type3Affix:
		return "₃"
	}
	return ""
}

func biasLabel(b g.Bias) string {
	expr := g.BiasExpression(b)
	if expr == "" {
		return b.String()
	}
	return fmt.Sprintf("%s(%s)", b.String(), expr)
}

// modularLabel formats a parsed modular adjunct. When ParseVnCn
// succeeds we show the typed SlotVIII content (e.g. "MOD(PRL.SUB)");
// otherwise we fall back to the raw "MOD(Vn+Cn)" surface form.
//
// marksMood comes from the tokenizer's cross-formative scan: when the
// next formative is verbal it's *true (Cn → Mood); when it's nominal
// or framed-verbal it's *false (Cn → CaseScope); when no neighbor was
// found it's nil and we fall back to the Vn-pattern heuristic.
func modularLabel(m g.ModularAdjunct, marksMood *bool) string {
	s, ok := parse.ParseVnCn(m.Vn, m.Cn)
	if !ok {
		return "MOD(" + m.Vn + "+" + m.Cn + ")"
	}
	inner := slotVIII(s, semantics.ModularIsVerbal(s, marksMood))
	if inner == "" {
		// Both Vn and Cn were defaults; surface just "MOD".
		return "MOD"
	}
	return "MOD(" + inner + ")"
}

// combinationRefLabel formats a combination-referential word as
// "REF[<refs>]-<case>.<spec>(-<affix>...)(-<case2>)".
func combinationRefLabel(c tokenize.CombinationRefWord) string {
	parts := make([]string, len(c.Refs))
	for i, pr := range c.Refs {
		s := pr.Referent.String()
		if pr.Effect != referentials.NEU {
			s += "/" + pr.Effect.String()
		}
		parts[i] = s
	}
	out := "REF[" + strings.Join(parts, "+") + "]-" + c.Case.String() + "." + c.Spec
	for _, a := range c.Affixes {
		out += "-" + a.Cs + ":" + a.Vx
	}
	if c.Case2 != nil {
		out += "-" + c.Case2.String()
	}
	return out
}

func refLabel(r tokenize.ReferentialWord) string {
	formatRefs := func(refs []referentials.PersonalRef) string {
		parts := make([]string, len(refs))
		for i, pr := range refs {
			s := pr.Referent.String()
			if pr.Effect != referentials.NEU {
				s += "/" + pr.Effect.String()
			}
			parts[i] = s
		}
		return strings.Join(parts, "+")
	}
	joined := formatRefs(r.Refs)
	if r.Category != nil {
		joined = r.Category.String() + ":" + joined
	}
	label := "REF[" + joined + "]"
	if r.Case != nil {
		label += "-" + r.Case.String()
	}
	if r.Case2 != nil {
		label += "-" + r.Case2.String()
	}
	if len(r.RefB) > 0 {
		label += "-[" + formatRefs(r.RefB) + "]"
	}
	if r.RpvEssence {
		label += "\\RPV"
	}
	return label
}
