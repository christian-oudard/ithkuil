package gloss

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/roman"
	"github.com/christian-oudard/ithkuil/semantics"
)

// Sentence runs the tokenizer over a sentence and returns one gloss
// string per word. A word that cannot be read glosses as "?" and the
// romanization, which says only that it was not read; callers wanting
// the reason should use roman.Tokenize and report it themselves.
func (gl *Glosser) Sentence(sentence string) []string {
	results := roman.Tokenize(sentence)
	out := make([]string, len(results))
	for i, r := range results {
		if r.Err != nil {
			out[i] = "?" + r.Romanization
			continue
		}
		out[i] = gl.Word(r.Word, roman.Words(results), i)
	}
	return out
}

// Sentence is the no-lexicon convenience wrapper.
func Sentence(sentence string) []string {
	return (&Glosser{}).Sentence(sentence)
}

// Token glosses one word out of context.
//
// Every class but one reads the same alone as in a span. The exception
// is the modular adjunct, whose Cn is Mood or Case-Scope depending on
// whether the formative it applies to is verbal — a fact about the
// neighbours, not the adjunct. Alone it falls back to the Vn-pattern
// heuristic, which is usually right and sometimes a guess. Use Word
// when the span is known.
func (gl *Glosser) Token(t g.Word) string {
	return gl.Word(t, nil, 0)
}

// Word glosses the word at index i of span, using the span for the
// facts that are not in the word. A nil span means there is none.
func (gl *Glosser) Word(t g.Word, span g.Text, i int) string {
	switch v := t.(type) {
	case g.Formative:
		return gl.Formative(v)
	case *g.Chain:
		parts := make([]string, 0, v.Length())
		for _, f := range v.Formatives() {
			parts = append(parts, gl.Formative(f))
		}
		sep := " >> "
		if gl.Canonical {
			sep = " "
		}
		return strings.Join(parts, sep)
	case g.Bias:
		return gl.biasLabel(v)
	case g.RegisterMarker:
		if v.End {
			return gl.registerEndLabel(v.Register)
		}
		return gl.registerStartLabel(v.Register)
	case g.CarrierAdjunct:
		return gl.carrierLabel(v)
	case g.ModularAdjunct:
		var marksMood *bool
		if span != nil {
			if verbal, found := roman.ModularIsVerbal(span, i); found {
				marksMood = &verbal
			}
		}
		return gl.modularLabel(v, marksMood)
	case g.SingleAffixAdjunct:
		return gl.singleAffixLabel(v)
	case g.MultipleAffixAdjunct:
		return gl.multiAffixLabel(v)
	case g.Referential:
		return gl.refLabel(v)
	case g.CombinationReferential:
		return gl.combinationRefLabel(v)
	case g.Foreign:
		// Canonical: wrap in double quotes so the parser can recognize
		// foreign text without confusing it for an Ithkuil word.
		// Display: bare text matches existing display-side expectations.
		if gl.Canonical {
			return `"` + v.Text + `"`
		}
		return v.Text
	}
	return "?"
}

// singleAffixLabel formats a single-affix adjunct. Display mode:
// "AFFIX[Cs/N]{scope}". Canonical mode (zsnout-aligned):
// "Cs/N" plus an optional "-{scope}" tail when scope is non-default.
func (gl *Glosser) singleAffixLabel(a g.SingleAffixAdjunct) string {
	body := gl.affixPart(a.Affix)
	if gl.Canonical {
		if a.Scope == g.ScopeVDom {
			return body
		}
		return body + "-{" + a.Scope.String() + "}"
	}
	return fmt.Sprintf("AFFIX[%s]{%s}", body, a.Scope.String())
}

// multiAffixLabel formats a multi-affix adjunct. Display mode:
// "AFFIXES[a1,a2,...]{first>rest}". Canonical mode (zsnout-aligned):
// "a1[-{first}]-a2-a3-...-aN[-{rest}]" — first scope follows the
// first affix, rest scope trails the last affix, defaults elided.
func (gl *Glosser) multiAffixLabel(a g.MultipleAffixAdjunct) string {
	if gl.Canonical {
		parts := []string{gl.affixPart(a.First)}
		if a.FirstScope != g.ScopeVDom {
			parts = append(parts, "{"+a.FirstScope.String()+"}")
		}
		for _, p := range a.Rest {
			parts = append(parts, gl.affixPart(p))
		}
		if a.RestScope != g.ScopeVDom && a.RestScope != a.FirstScope {
			parts = append(parts, "{"+a.RestScope.String()+"}")
		}
		return strings.Join(parts, "-")
	}
	parts := []string{gl.affixPart(a.First)}
	for _, p := range a.Rest {
		parts = append(parts, gl.affixPart(p))
	}
	return fmt.Sprintf("AFFIXES[%s]{%s→%s}",
		strings.Join(parts, ","),
		a.FirstScope.String(), a.RestScope.String())
}

// affixPart renders one affix as "<abbrev>/<deg><type>" using the
// lexicon abbreviation when available, otherwise the raw Cs cluster.
func (gl *Glosser) affixPart(a g.Affix) string {
	return fmt.Sprintf("%s/%d%s",
		gl.affixLabel(a.Consonant), a.Degree, gl.affixTypeSuffix(a.Type))
}

// affixLabel returns the abbreviation for an affix Cs when the lexicon
// is set and has an entry; otherwise the raw cluster. Canonical mode
// spells that fallback in ASCII digraphs, the same way the root is
// spelled, since the canonical gloss has to stay typable. The lexicon
// is a named subset rather than the list of legal Cs clusters, so the
// fallback is reachable for any well-formed affix outside it.
func (gl *Glosser) affixLabel(cs string) string {
	if gl.Lex != nil {
		if entry, ok := gl.Lex.Affixes[cs]; ok && entry.Abbrev != "" {
			return entry.Abbrev
		}
	}
	if gl.Canonical {
		return phonology.ToASCII(cs)
	}
	return cs
}

// affixTypeSuffix renders an affix's Type. Type 1 (the unmarked
// default) is silent in both modes. Type 2 and Type 3 emit either a
// Unicode subscript (display mode) or the ASCII-clean "_N" suffix
// (Canonical mode, parsed by compose).
func (gl *Glosser) affixTypeSuffix(t g.AffixType) string {
	if gl.Canonical {
		switch t {
		case g.Type2Affix:
			return "_2"
		case g.Type3Affix:
			return "_3"
		}
		return ""
	}
	switch t {
	case g.Type2Affix:
		return "₂"
	case g.Type3Affix:
		return "₃"
	}
	return ""
}

// biasLabel formats a bias adjunct. Display mode adds the English
// expression in parens; canonical mode emits the bare abbreviation
// (suitable for input parsing).
func (gl *Glosser) biasLabel(b g.Bias) string {
	if gl.Canonical {
		return b.String()
	}
	expr := g.BiasExpression(b)
	if expr == "" {
		return b.String()
	}
	return fmt.Sprintf("%s(%s)", b.String(), expr)
}

// registerStartLabel formats a register-start adjunct.
// Display: "REG-DSV". Canonical: bare "DSV" (zsnout style).
func (gl *Glosser) registerStartLabel(r g.Register) string {
	if gl.Canonical {
		return r.String()
	}
	return "REG-" + r.String()
}

// registerEndLabel formats a register-end adjunct.
// Display: "REG-DSV-END". Canonical: "DSV_END" (zsnout style).
func (gl *Glosser) registerEndLabel(r g.Register) string {
	if gl.Canonical {
		return r.String() + "_END"
	}
	return "REG-" + r.String() + "-END"
}

// carrierLabel formats a carrier adjunct.
// Display: "CARR-Quotative(a)". Canonical: "[QUO]-CASE" — the carrier
// type is shown as a 3-letter abbreviation and the Vc vowel is decoded
// to its case name (zsnout-style suppletive form).
func (gl *Glosser) carrierLabel(c g.CarrierAdjunct) string {
	if gl.Canonical {
		head := "[" + carrierTypeAbbrev(c.Type) + "]"
		if c.Case == g.THM {
			return head
		}
		return head + "-" + c.Case.String()
	}
	return "CARR-" + c.Type.String() + "(" + parse.CaseToVc(c.Case) + ")"
}

// carrierTypeAbbrev returns the 3-letter canonical abbreviation for a
// CarrierType, matching the suppletive-adjunct convention used by the
// zsnout toolkit (CAR/QUO/NAM/PHR).
func carrierTypeAbbrev(t g.CarrierType) string {
	switch t {
	case g.Carrier:
		return "CAR"
	case g.Quotative:
		return "QUO"
	case g.Naming:
		return "NAM"
	case g.Phrasal:
		return "PHR"
	}
	return "?"
}

// modularLabel formats a parsed modular adjunct.
//
// Display: "MOD(<vn>.<cn>)" with "MOD" alone when both default.
//
// Canonical: the Vn.Cn content bare ("RTR.SUB"), or "MOD" if both
// are at their defaults.
//
// marksMood comes from the tokenizer's cross-formative scan: when the
// next formative is verbal it's *true (Cn → Mood); when it's nominal
// or framed-verbal it's *false (Cn → CaseScope); when no neighbor was
// found it's nil and we fall back to the Vn-pattern heuristic.
func (gl *Glosser) modularLabel(m g.ModularAdjunct, marksMood *bool) string {
	var inner string
	if len(m.Content) > 0 {
		// Single-pair case (the common shape): emit Vn.Cn content.
		// Multi-pair modulars (§4.3) join their slot-VIII labels with
		// "-" for display, comma for canonical compactness.
		parts := make([]string, len(m.Content))
		for i, s := range m.Content {
			parts[i] = slotVIII(s, semantics.ModularIsVerbal(s, marksMood))
		}
		inner = strings.Join(parts, "-")
	}
	if gl.Canonical {
		if inner == "" {
			return "MOD" + modularScopeSuffix(m.Scope) + modularReachSuffix(m.Reach)
		}
		return inner + modularScopeSuffix(m.Scope) + modularReachSuffix(m.Reach)
	}
	if inner == "" {
		return "MOD"
	}
	return "MOD(" + inner + ")"
}

// modularScopeSuffix returns the canonical "-{parent}" / "-{concat}"
// trailing marker for non-default modular application scopes; empty
// otherwise.
func modularScopeSuffix(s g.ModularScope) string {
	if s == g.ModularScopeDefault {
		return ""
	}
	return "-{" + s.String() + "}"
}

// modularReachSuffix returns the canonical "-{<name>}" trailing marker
// for a non-None reach scope (§4.3 Slot 4 V_H).
func modularReachSuffix(r g.ModularReach) string {
	if r == g.ModularReachNone {
		return ""
	}
	return "-{" + r.String() + "}"
}

// combinationRefLabel formats a §4.6.2 combination referential.
//
// Display: "REF[<refs>]-<case>.<spec>(-<affix>...)(-<case2>)".
// Canonical: "<refs>-<case>-<spec>(-<affix>...)(-<case2>)(-RPV)" — the
// Specification is always emitted, which is what tells a combination
// referential apart from a plain one.
func (gl *Glosser) combinationRefLabel(c g.CombinationReferential) string {
	comb := c
	head := gl.refHead(comb.Head)
	sep := "-"
	if !gl.Canonical {
		sep = "."
	}
	out := head + "-" + comb.Case.String() + sep + comb.Spec.String()
	for _, a := range comb.Affixes {
		out += "-" + gl.affixPart(a)
	}
	if comb.Case2 != nil {
		out += "-" + comb.Case2.String()
	}
	if comb.RpvEssence {
		if gl.Canonical {
			out += "-RPV"
		} else {
			out += "\\RPV"
		}
	}
	return out
}

// refLabel formats a §4.6.1 referential.
//
// Display: "REF[<refs>]-<case>(-[<refB>]/<case2>)(\RPV)".
// Canonical: the same with a bare head and "-RPV".
//
// The second referent binds its own case, so it reads "[2m]/IND" per
// the gloss rule that a case attached to a head is written HEAD/CASE.
// A second case with no referent of its own stacks onto the head
// instead, and stays a plain slot.
func (gl *Glosser) refLabel(r g.Referential) string {
	ref := r
	label := gl.refHead(ref.Head) + "-" + ref.Case.String()
	if s := ref.Second; s != nil {
		if len(s.Refs) > 0 {
			label += "-" + formatRefList(s.Refs, true) + "/" + s.Case.String()
		} else {
			label += "-" + s.Case.String()
		}
	}
	if ref.RpvEssence {
		if gl.Canonical {
			label += "-RPV"
		} else {
			label += "\\RPV"
		}
	}
	return label
}

// refHead renders a referential head: a suppletive cluster as "[QUO]"
// (canonical) or "CARR[Quotative]" (display), or a referent chain with
// its optional category tag, as in "NOM:1m".
func (gl *Glosser) refHead(head g.RefHead) string {
	switch h := head.(type) {
	case g.SuppletiveHead:
		if gl.Canonical {
			return "[" + carrierTypeAbbrev(h.Type) + "]"
		}
		return "CARR[" + h.Type.String() + "]"
	case g.PersonalHead:
		listed := formatRefList(h.Refs, len(h.Refs) > 1)
		if h.Category != nil {
			listed = h.Category.String() + ":" + listed
		}
		if gl.Canonical {
			return listed
		}
		return "REF[" + strings.TrimPrefix(strings.TrimSuffix(listed, "]"), "[") + "]"
	}
	return "?"
}

// formatRefList renders a list of PersonalRefs as "a+b+c", optionally
// wrapped in square brackets (used when the list has multiple refs or
// must be visually distinct from a single referent).
func formatRefList(refs []g.PersonalRef, brackets bool) string {
	parts := make([]string, len(refs))
	for i, pr := range refs {
		s := pr.Referent.String()
		if pr.Effect != g.NEU {
			s += "/" + pr.Effect.String()
		}
		parts[i] = s
	}
	joined := strings.Join(parts, "+")
	if brackets {
		return "[" + joined + "]"
	}
	return joined
}
