package gloss

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/referentials"
	"github.com/christian-oudard/ithkuil/semantics"
	"github.com/christian-oudard/ithkuil/surface"
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
		sep := " >> "
		if gl.Canonical {
			sep = " "
		}
		return strings.Join(parts, sep)
	case tokenize.BiasWord:
		return gl.biasLabel(v.Bias)
	case tokenize.RegisterStartWord:
		return gl.registerStartLabel(v.Register)
	case tokenize.RegisterEndWord:
		return gl.registerEndLabel(v.Register)
	case tokenize.CarrierWord:
		return gl.carrierLabel(v.Carrier)
	case tokenize.ModularWord:
		return gl.modularLabel(v.Modular, v.MarksMood)
	case tokenize.SingleAffixWord:
		return gl.singleAffixLabel(v.Affix)
	case tokenize.MultipleAffixWord:
		return gl.multiAffixLabel(v.Affixes)
	case tokenize.ReferentialWord:
		return gl.refLabel(v)
	case tokenize.CombinationRefWord:
		return gl.combinationRefLabel(v)
	case tokenize.ParsingAdjunctWord:
		return gl.parsingAdjunctLabel(v.Adjunct)
	case tokenize.UnknownWord:
		return "?" + v.Text
	case tokenize.ForeignWord:
		// Canonical: wrap in double quotes so the parser can recognize
		// foreign text without confusing it for an Ithkuil token.
		// Display: bare text matches existing display-side expectations.
		if gl.Canonical {
			return `"` + v.Text + `"`
		}
		return v.Text
	}
	return "?"
}

// parsingAdjunctLabel formats a §4.8 parsing adjunct.
// Canonical: "mono:" / "ulti:" / "penu:" / "ante:" (zsnout style).
// Display: same — there's no useful display elaboration for these.
func (gl *Glosser) parsingAdjunctLabel(p g.ParsingAdjunct) string {
	switch p.Stress {
	case surface.Monosyllabic:
		return "mono:"
	case surface.Ultimate:
		return "ulti:"
	case surface.Penultimate:
		return "penu:"
	case surface.Antepenultimate:
		return "ante:"
	}
	return "?:"
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
		return surface.ToASCII(cs)
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
	return "CARR-" + c.Type.String() + "(" + g.CaseToVc(c.Case) + ")"
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

// modularScopeSuffix returns the canonical "-{parent}" / "-{concat.}"
// trailing marker for non-default modular application scopes; empty
// otherwise.
func modularScopeSuffix(s g.ModularScope) string {
	switch s {
	case g.ModularScopeParent:
		return "-{parent}"
	case g.ModularScopeConcat:
		return "-{concat.}"
	}
	return ""
}

// modularReachSuffix returns the canonical "-{<name>}" trailing marker
// for a non-None reach scope (§4.3 Slot 4 V_H).
func modularReachSuffix(r g.ModularReach) string {
	switch r {
	case g.ModularReachCaseMood:
		return "-{case/mood}"
	case g.ModularReachCaseMoodIll:
		return "-{case/mood/ill}"
	case g.ModularReachFormative:
		return "-{form.}"
	case g.ModularReachAdjacent:
		return "-{adj.}"
	}
	return ""
}

// combinationRefLabel formats a combination-referential word.
//
// Display: "REF[<refs>]-<case>.<spec>(-<affix>...)(-<case2>)" with
// "CARR[<type>]" when the C1 was a suppletive.
//
// Canonical (zsnout-aligned): "<refs>-<case>-<spec>(-<affix>...)(-<case2>)"
// — head is a bare referent list (or "[a+b]" when more than one); the
// Specification is shown as another hyphen-separated slot (always
// emitted to disambiguate from a plain referential).
func (gl *Glosser) combinationRefLabel(c tokenize.CombinationRefWord) string {
	head := gl.refHead(c.Carrier, c.Refs, nil)
	if gl.Canonical {
		out := head + "-" + c.Case.String() + "-" + c.Spec.String()
		for _, a := range c.Affixes {
			out += "-" + gl.affixPart(a)
		}
		if c.Case2 != nil {
			out += "-" + c.Case2.String()
		}
		return out
	}
	out := head + "-" + c.Case.String() + "." + c.Spec.String()
	for _, a := range c.Affixes {
		out += "-" + gl.affixPart(a)
	}
	if c.Case2 != nil {
		out += "-" + c.Case2.String()
	}
	return out
}

// refLabel formats a referential word.
//
// Display: "REF[<refs>]-<case>(-<case2>)(-[<refB>])(\RPV)" with
// "CARR[<type>]" for suppletive C1.
//
// Canonical (zsnout-aligned): bare "<refs>" or "[a+b]" head; case
// suffixed with "-"; RpvEssence trails as "-RPV".
//
// Carrier-headed referentials emit a leading "*" in canonical mode
// (§4.6.3 epenthesis disambiguator): "*[CAR]-CASE" reads as "carrier
// used as a referential head" — distinct from the bare CarrierWord
// gloss "[CAR]-CASE".
func (gl *Glosser) refLabel(r tokenize.ReferentialWord) string {
	head := gl.refHead(r.Carrier, r.Refs, r.Category)
	var label string
	if gl.Canonical {
		if r.Carrier != nil {
			head = "*" + head
		}
		label = head
		if r.Case != nil {
			label += "-" + r.Case.String()
		}
		if r.Case2 != nil {
			label += "-" + r.Case2.String()
		}
		if len(r.RefB) > 0 {
			label += "-" + formatRefList(r.RefB, true)
		}
		if r.RpvEssence {
			label += "-RPV"
		}
		return label
	}
	label = head
	if r.Case != nil {
		label += "-" + r.Case.String()
	}
	if r.Case2 != nil {
		label += "-" + r.Case2.String()
	}
	if len(r.RefB) > 0 {
		label += "-" + formatRefList(r.RefB, true)
	}
	if r.RpvEssence {
		label += "\\RPV"
	}
	return label
}

// refHead renders the head of a referential or combination-referential:
// either a carrier suppletive "[QUO]" (canonical) / "CARR[Quotative]"
// (display), or a referent list. The optional Category prefixes the
// list with "X:" for non-personal categories (only used by
// ReferentialWord; CombinationRefWord doesn't carry Category).
func (gl *Glosser) refHead(carrier *g.CarrierType, refs []referentials.PersonalRef, category *referentials.Category) string {
	if carrier != nil {
		if gl.Canonical {
			return "[" + carrierTypeAbbrev(*carrier) + "]"
		}
		return "CARR[" + carrier.String() + "]"
	}
	listed := formatRefList(refs, len(refs) > 1)
	if category != nil {
		listed = category.String() + ":" + listed
	}
	if gl.Canonical {
		return listed
	}
	return "REF[" + strings.TrimPrefix(strings.TrimSuffix(listed, "]"), "[") + "]"
}

// formatRefList renders a list of PersonalRefs as "a+b+c", optionally
// wrapped in square brackets (used when the list has multiple refs or
// must be visually distinct from a single referent).
func formatRefList(refs []referentials.PersonalRef, brackets bool) string {
	parts := make([]string, len(refs))
	for i, pr := range refs {
		s := pr.Referent.String()
		if pr.Effect != referentials.NEU {
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
