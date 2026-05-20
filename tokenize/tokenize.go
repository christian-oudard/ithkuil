// Package tokenize classifies words in an Ithkuil sentence into their
// grammatical roles. Each word becomes one of several WordToken
// variants — a formative, a bias adjunct, a register marker, etc. —
// using the parsers already built in the parse, fullparse, and
// referentials packages.
//
// The classifier tries parsers in priority order tightest-first:
//
//  1. Pure-consonant single conjunct → Bias.
//  2. Recognized register opener/closer surface → Register.
//  3. Recognized carrier consonant + vowel → Carrier.
//  4. Vowel + valid Cn consonant → Modular.
//  5. Anything else that parses as a formative → Formative.
//  6. Anything else that decomposes as referential C1 → Referential.
//  7. Fallback → UnknownWord.
package tokenize

import (
	"strings"

	"github.com/christian-oudard/ithkuil/concatenation"
	"github.com/christian-oudard/ithkuil/fullparse"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/surface"
	"github.com/christian-oudard/ithkuil/referentials"
)

// WordToken is the sealed sum type for classified words. Each variant
// carries the original surface text plus the parsed data appropriate
// to its kind.
type WordToken interface {
	Surface() string
	word()
}

// FormativeWord wraps a successfully parsed formative.
type FormativeWord struct {
	Text      string
	Formative g.Formative
}

func (f FormativeWord) Surface() string { return f.Text }
func (FormativeWord) word()             {}

// ConcatenatedFormativeWord wraps a hyphen-joined chain of two or more
// formatives. The first part is the head; subsequent parts must each
// have a Slot I concatenation marker on their parsed Formative.
type ConcatenatedFormativeWord struct {
	Text  string
	Chain *concatenation.Chain
}

func (c ConcatenatedFormativeWord) Surface() string { return c.Text }
func (ConcatenatedFormativeWord) word()             {}

// BiasWord is a stand-alone bias adjunct.
type BiasWord struct {
	Text string
	Bias g.Bias
}

func (b BiasWord) Surface() string { return b.Text }
func (BiasWord) word()             {}

// RegisterStartWord opens a non-narrative register.
type RegisterStartWord struct {
	Text     string
	Register g.Register
}

func (r RegisterStartWord) Surface() string { return r.Text }
func (RegisterStartWord) word()             {}

// RegisterEndWord closes a register.
type RegisterEndWord struct {
	Text     string
	Register g.Register
}

func (r RegisterEndWord) Surface() string { return r.Text }
func (RegisterEndWord) word()             {}

// ModularWord carries a Vn+Cn modular adjunct.
//
// MarksMood reflects the next formative's verbal/nominal status, used
// to disambiguate the Cn surface form: true = the adjacent formative is
// verbal (Cn → Mood); false = nominal or framed-verbal (Cn → Case-
// Scope); nil = no adjacent formative was found in the token stream.
type ModularWord struct {
	Text      string
	Modular   g.ModularAdjunct
	MarksMood *bool
}

func (m ModularWord) Surface() string { return m.Text }
func (ModularWord) word()             {}

// SingleAffixWord is one V_x C_s affix on its own as an adjunct
// (§4.1.1). Shape: V-C[-V].
type SingleAffixWord struct {
	Text  string
	Affix g.SingleAffixAdjunct
}

func (s SingleAffixWord) Surface() string { return s.Text }
func (SingleAffixWord) word()             {}

// MultipleAffixWord is two-or-more affixes chained into one adjunct
// (§4.1.2). Shape: [ë] C V Cz V C ... [V].
type MultipleAffixWord struct {
	Text    string
	Affixes g.MultipleAffixAdjunct
}

func (m MultipleAffixWord) Surface() string { return m.Text }
func (MultipleAffixWord) word()             {}

// CarrierWord wraps a carrier adjunct (carrier/quotative/naming/phrasal).
type CarrierWord struct {
	Text    string
	Carrier g.CarrierAdjunct
}

func (c CarrierWord) Surface() string { return c.Text }
func (CarrierWord) word()             {}

// ReferentialWord wraps one or more personal references parsed from
// a single referential cluster, optionally followed by a Vc case
// vowel that scopes the entire reference.
type ReferentialWord struct {
	Text     string
	Category *referentials.Category // nil if no category modifier
	Refs     []referentials.PersonalRef
	Case     *g.Case // nil when no trailing Vc was present
}

// CombinationRefWord is the richer referential shape that pairs a
// referent chain with a case, a Specification marker, optional
// VxCs affixes, and an optional second case:
//
//	[ë] C1 Vc Spec [VxCs...] [Vc2]
//
// The Spec field is the raw consonant cluster (one of x/xt/xp/xx);
// the rest are decoded values.
type CombinationRefWord struct {
	Text    string
	Refs    []referentials.PersonalRef
	Case    g.Case
	Spec    string
	Affixes []g.AffixPair
	Case2   *g.Case // optional second case
}

func (c CombinationRefWord) Surface() string { return c.Text }
func (CombinationRefWord) word()             {}

func (r ReferentialWord) Surface() string { return r.Text }
func (ReferentialWord) word()             {}

// UnknownWord is the fallback when no parser claims the word.
type UnknownWord struct {
	Text string
}

func (u UnknownWord) Surface() string { return u.Text }
func (UnknownWord) word()             {}

// ForeignWord is a token consumed in carrier context: the word
// immediately following a CarrierWord is treated as foreign text
// (a name, quotation, or other passthrough) and not parsed.
type ForeignWord struct {
	Text string
}

func (f ForeignWord) Surface() string { return f.Text }
func (ForeignWord) word()             {}

// ClassifyWord decides which WordToken variant a single surface word
// belongs to. The order of attempts is the docstring of the package.
func ClassifyWord(word string) WordToken {
	if word == "" {
		return UnknownWord{Text: word}
	}
	// Hyphenated input: try as a concatenation chain. Every part must
	// parse as a formative; non-final parts must carry Slot I.
	if strings.Contains(word, "-") {
		if c, ok := tryConcatenation(word); ok {
			return ConcatenatedFormativeWord{Text: word, Chain: c}
		}
		// Fall through and try other classifications anyway.
	}
	conjs := surface.SplitConjuncts(word)

	// 1. Single consonant cluster → Bias.
	if len(conjs) == 1 && surface.IsConsonantConjunct(conjs[0]) {
		if b, ok := parse.ParseBias(conjs[0]); ok {
			return BiasWord{Text: word, Bias: b}
		}
	}

	// 2. Register opener or closer (whole word, no splitting).
	if r, ok := parse.ParseRegister(word); ok {
		return RegisterStartWord{Text: word, Register: r}
	}
	if r, ok := parse.ParseRegisterFinal(word); ok {
		return RegisterEndWord{Text: word, Register: r}
	}

	// 3. Carrier adjunct: word starting with a carrier consonant
	// (hl/hm/hn/hň) followed by trailing content. Tried before
	// formative parsing so that "hnas" is a Naming carrier rather
	// than a formative with Cr=hn.
	if len(conjs) >= 2 && surface.IsConsonantConjunct(conjs[0]) {
		if c, err := parse.ParseCarrier(word); err == nil {
			return CarrierWord{Text: word, Carrier: c}
		}
	}

	// 4. Modular adjunct: 1-7 conjuncts of the shape
	//    [w/y] (Vn Cn){0-3} V(final). Single-vowel words are valid as
	//    "lone aspect" modulars (§4.3). Larger forms can have up to
	//    three VnCn pairs plus a final vowel, plus an optional w/y
	//    scope prefix.
	if m, err := parse.ParseModular(word); err == nil {
		return ModularWord{Text: word, Modular: m}
	}

	// 4a. Single-affix adjunct (§4.1.1): V-C[-V], starting with a
	//     vowel other than "ë". Tried before referential/formative so
	//     a leading vowel doesn't get re-read as Vv of an under-sized
	//     formative.
	if len(conjs) >= 2 && len(conjs) <= 3 &&
		surface.IsVowelConjunct(conjs[0]) && conjs[0] != "ë" {
		if a, err := parse.ParseSingleAffix(word); err == nil {
			return SingleAffixWord{Text: word, Affix: a}
		}
	}

	// 4b. Multi-affix adjunct (§4.1.2): [ë] C V Cz V C ... [V]. The Cz
	//     consonant ('h, 'hl, 'hr, hw, 'hw or h) at the third post-ë
	//     position is what distinguishes this shape from a same-length
	//     consonant-initial formative.
	if a, err := parse.ParseMultipleAffix(word); err == nil {
		return MultipleAffixWord{Text: word, Affixes: a}
	}

	// 5. Two-conjunct referential: C1-cluster + Vc-vowel.
	if len(conjs) == 2 &&
		surface.IsConsonantConjunct(conjs[0]) &&
		surface.IsVowelConjunct(conjs[1]) {
		if cat, refs, ok := referentials.DecomposeRefWithCategory(conjs[0]); ok {
			if c, cok := parse.ParseCase(conjs[1]); cok {
				return ReferentialWord{
					Text:     word,
					Category: cat,
					Refs:     refs,
					Case:     &c,
				}
			}
		}
	}

	// 5b. Combination referential: [ë] C1 Vc Spec [VxCs...] [Vc2].
	if c, ok := tryCombinationRef(word, conjs); ok {
		return c
	}

	// 6. Formative.
	if f, err := fullparse.ParseFormative(word); err == nil {
		return FormativeWord{Text: word, Formative: f}
	}

	// 7. Referential without case: single consonant cluster that decomposes.
	if len(conjs) == 1 && surface.IsConsonantConjunct(conjs[0]) {
		if cat, refs, ok := referentials.DecomposeRefWithCategory(conjs[0]); ok {
			return ReferentialWord{Text: word, Category: cat, Refs: refs}
		}
	}

	return UnknownWord{Text: word}
}

// Tokenize splits a sentence on whitespace and classifies each word.
// Context-aware: a CarrierWord causes the immediately-following word
// to be re-tagged as a ForeignWord (carrier scopes one trailing word
// of foreign text — a name, quotation, or similar).
func Tokenize(sentence string) []WordToken {
	fields := strings.Fields(sentence)
	out := make([]WordToken, len(fields))
	for i, w := range fields {
		out[i] = ClassifyWord(w)
	}
	for i := 0; i+1 < len(out); i++ {
		if isCarrierToken(out[i]) {
			out[i+1] = ForeignWord{Text: out[i+1].Surface()}
		}
	}
	for i, t := range out {
		if mw, ok := t.(ModularWord); ok {
			if verbal, found := nextFormativeIsVerbal(out, i); found {
				mw.MarksMood = &verbal
				out[i] = mw
			}
		}
	}
	return out
}

// nextFormativeIsVerbal scans forward from i+1 for the next formative-
// bearing token and returns whether its (parent, for chains) Final is
// verbal (ultimate stress). Returns found=false if no formative-bearing
// token is encountered.
func nextFormativeIsVerbal(toks []WordToken, i int) (verbal, found bool) {
	for j := i + 1; j < len(toks); j++ {
		switch w := toks[j].(type) {
		case FormativeWord:
			return g.IsVerbal(w.Formative.Final), true
		case ConcatenatedFormativeWord:
			fs := w.Chain.Formatives()
			if len(fs) == 0 {
				return false, false
			}
			return g.IsVerbal(fs[len(fs)-1].Final), true
		}
	}
	return false, false
}

// isCarrierToken reports whether tok semantically scopes a foreign word
// that follows it. Plain CarrierWords always do; FormativeWords whose
// root is the carrier root "s" also do (matching Haskell's
// isCarrierParsed which is true for either shape).
func isCarrierToken(tok WordToken) bool {
	switch v := tok.(type) {
	case CarrierWord:
		return true
	case FormativeWord:
		if cr, ok := v.Formative.Root.(g.CrRoot); ok {
			return cr.Cluster == "s"
		}
		return false
	}
	return false
}

// isCombinationSpec reports whether c is one of the Specification
// consonant markers used in combination referentials.
func isCombinationSpec(c string) bool {
	switch c {
	case "x", "xt", "xp", "xx":
		return true
	}
	return false
}

// hasDoubledLetter reports whether s contains two consecutive identical
// runes. Used to detect geminated Ca clusters — a signal that an
// ambiguous word is a formative (with §3.6.1 gemination marking Slot V
// boundary) rather than a combination referential (whose post-spec
// affix Cs values never contain doubled letters).
func hasDoubledLetter(s string) bool {
	var prev rune
	for i, r := range s {
		if i > 0 && r == prev {
			return true
		}
		prev = r
	}
	return false
}

// tryCombinationRef matches the combination-referential shape
// [ë] C1 Vc Spec [VxCs...] [Vc2]. Returns ok=false if any constraint
// fails. The Vc2 special form "üa" maps to THM and "a" alone means
// "no second case".
func tryCombinationRef(text string, conjs []string) (CombinationRefWord, bool) {
	// Strip optional leading "ë" prefix.
	if len(conjs) > 0 && conjs[0] == "ë" {
		conjs = conjs[1:]
	}
	if len(conjs) < 3 {
		return CombinationRefWord{}, false
	}
	c1, vc, spec := conjs[0], conjs[1], conjs[2]
	if !surface.IsConsonantConjunct(c1) || !surface.IsVowelConjunct(vc) || !isCombinationSpec(spec) {
		return CombinationRefWord{}, false
	}
	refs, refsOk := referentials.DecomposeRefCluster(c1)
	if !refsOk || len(refs) == 0 {
		return CombinationRefWord{}, false
	}
	caseVal, caseOk := parse.ParseCase(vc)
	if !caseOk {
		return CombinationRefWord{}, false
	}
	// Pair up the rest as VxCs with optional trailing Vc2.
	rest := conjs[3:]
	// A geminated consonant anywhere in the tail (e.g. "kk" in ţnaxekka)
	// signals this is actually a formative with a Slot V boundary,
	// not a combination referential.
	for _, c := range rest {
		if surface.IsConsonantConjunct(c) && hasDoubledLetter(c) {
			return CombinationRefWord{}, false
		}
	}
	var affixes []g.AffixPair
	var case2 *g.Case
	for i := 0; i < len(rest); {
		if i+1 < len(rest) &&
			surface.IsVowelConjunct(rest[i]) &&
			surface.IsConsonantConjunct(rest[i+1]) {
			affixes = append(affixes, g.AffixPair{Vx: rest[i], Cs: rest[i+1]})
			i += 2
			continue
		}
		if i == len(rest)-1 && surface.IsVowelConjunct(rest[i]) {
			// Final Vc2: special-case "a" (no case) and "üa" → THM.
			switch rest[i] {
			case "a":
				// no second case
			case "üa":
				thm := g.THM
				case2 = &thm
			default:
				if c, ok := parse.ParseCase(rest[i]); ok {
					case2 = &c
				}
			}
			i++
			continue
		}
		return CombinationRefWord{}, false
	}
	return CombinationRefWord{
		Text:    text,
		Refs:    refs,
		Case:    caseVal,
		Spec:    spec,
		Affixes: affixes,
		Case2:   case2,
	}, true
}

// tryConcatenation attempts to read word as a hyphen-joined formative
// chain. All parts must parse as formatives; every non-final part
// must carry a Slot I concatenation marker. Returns ok=false if any
// constraint fails.
func tryConcatenation(word string) (*concatenation.Chain, bool) {
	parts := strings.Split(word, "-")
	if len(parts) < 2 {
		return nil, false
	}
	formatives := make([]g.Formative, 0, len(parts))
	for _, p := range parts {
		f, err := fullparse.ParseFormative(p)
		if err != nil {
			return nil, false
		}
		formatives = append(formatives, f)
	}
	// Head must be plain (no Concat).
	if formatives[0].Concat != nil {
		return nil, false
	}
	// Every dependent must carry Concat.
	for i := 1; i < len(formatives); i++ {
		if formatives[i].Concat == nil {
			return nil, false
		}
	}
	chain := concatenation.New(formatives[0])
	for i := 1; i < len(formatives); i++ {
		switch *formatives[i].Concat {
		case g.Type1:
			chain.Tail = append(chain.Tail, concatenation.Link{
				Type:      concatenation.Type1Concat,
				Formative: formatives[i],
			})
		case g.Type2:
			chain.Tail = append(chain.Tail, concatenation.Link{
				Type:      concatenation.Type2Concat,
				Formative: formatives[i],
			})
		}
	}
	return chain, true
}
