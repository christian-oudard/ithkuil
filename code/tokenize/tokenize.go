// Package tokenize classifies words in an Ithkuil sentence into their
// grammatical roles. Each word becomes one of several WordToken
// variants — a formative, a bias adjunct, a register marker, etc. —
// each of which is a thin wrapper around the grammar type for its word
// class. The decoding is done by parse and fullparse; what this
// package adds is deciding which of them to believe.
//
// The classifier tries parsers in priority order tightest-first:
//
//  1. Pure-consonant single conjunct → Bias.
//  2. Recognized register opener/closer romanization → Register.
//  3. Recognized carrier consonant + vowel → Carrier.
//  4. Vowel + valid Cn consonant → Modular.
//  5. A referential, then a combination referential.
//  6. Anything else that parses as a formative → Formative.
//  7. Fallback → UnknownWord.
package tokenize

import (
	"strings"

	"github.com/christian-oudard/ithkuil/fullparse"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
)

// WordToken is the sealed sum type for classified words. Each variant
// carries the original romanization plus the parsed data appropriate
// to its kind.
type WordToken interface {
	Romanization() string
	word()
}

// FormativeWord wraps a successfully parsed formative.
type FormativeWord struct {
	Text      string
	Formative g.Formative
}

func (f FormativeWord) Romanization() string { return f.Text }
func (FormativeWord) word()                  {}

// ConcatenatedFormativeWord wraps a hyphen-joined chain of two or more
// formatives. The first part is the head; subsequent parts must each
// have a Slot I concatenation marker on their parsed Formative.
type ConcatenatedFormativeWord struct {
	Text  string
	Chain *g.Chain
}

func (c ConcatenatedFormativeWord) Romanization() string { return c.Text }
func (ConcatenatedFormativeWord) word()                  {}

// BiasWord is a stand-alone bias adjunct.
type BiasWord struct {
	Text string
	Bias g.Bias
}

func (b BiasWord) Romanization() string { return b.Text }
func (BiasWord) word()                  {}

// RegisterStartWord opens a non-narrative register.
type RegisterStartWord struct {
	Text     string
	Register g.Register
}

func (r RegisterStartWord) Romanization() string { return r.Text }
func (RegisterStartWord) word()                  {}

// RegisterEndWord closes a register.
type RegisterEndWord struct {
	Text     string
	Register g.Register
}

func (r RegisterEndWord) Romanization() string { return r.Text }
func (RegisterEndWord) word()                  {}

// ModularWord carries a Vn+Cn modular adjunct.
//
// MarksMood reflects the next formative's verbal/nominal status, used
// to disambiguate the Cn romanization: true = the adjacent formative is
// verbal (Cn → Mood); false = nominal or framed-verbal (Cn → Case-
// Scope); nil = no adjacent formative was found in the token stream.
type ModularWord struct {
	Text      string
	Modular   g.ModularAdjunct
	MarksMood *bool
}

func (m ModularWord) Romanization() string { return m.Text }
func (ModularWord) word()                  {}

// SingleAffixWord is one V_x C_s affix on its own as an adjunct
// (§4.1.1). Shape: V-C[-V].
type SingleAffixWord struct {
	Text  string
	Affix g.SingleAffixAdjunct
}

func (s SingleAffixWord) Romanization() string { return s.Text }
func (SingleAffixWord) word()                  {}

// MultipleAffixWord is two-or-more affixes chained into one adjunct
// (§4.1.2). Shape: [ë] C V Cz V C ... [V].
type MultipleAffixWord struct {
	Text    string
	Affixes g.MultipleAffixAdjunct
}

func (m MultipleAffixWord) Romanization() string { return m.Text }
func (MultipleAffixWord) word()                  {}

// CarrierWord wraps a carrier adjunct (carrier/quotative/naming/phrasal).
type CarrierWord struct {
	Text    string
	Carrier g.CarrierAdjunct
}

func (c CarrierWord) Romanization() string { return c.Text }
func (CarrierWord) word()                  {}

// ReferentialWord wraps a §4.6.1 single- or dual-referential.
type ReferentialWord struct {
	Text        string
	Referential g.Referential
}

// CombinationRefWord wraps a §4.6.2 combination referential.
type CombinationRefWord struct {
	Text        string
	Combination g.CombinationReferential
}

func (c CombinationRefWord) Romanization() string { return c.Text }
func (CombinationRefWord) word()                  {}

func (r ReferentialWord) Romanization() string { return r.Text }
func (ReferentialWord) word()                  {}

// ParsingAdjunctWord wraps a §4.8 parsing adjunct ('V'). The adjunct
// itself has no grammatical content; it signals the stress of the
// immediately-following word as a written cue when prosody can't be
// relied on.
type ParsingAdjunctWord struct {
	Text    string
	Adjunct parse.ParsingAdjunct
}

func (p ParsingAdjunctWord) Romanization() string { return p.Text }
func (ParsingAdjunctWord) word()                  {}

// UnknownWord is the fallback when no parser claims the word.
type UnknownWord struct {
	Text string
}

func (u UnknownWord) Romanization() string { return u.Text }
func (UnknownWord) word()                  {}

// ForeignWord is a token consumed in carrier context: the word
// immediately following a CarrierWord is treated as foreign text
// (a name, quotation, or other passthrough) and not parsed.
type ForeignWord struct {
	Text string
}

func (f ForeignWord) Romanization() string { return f.Text }
func (ForeignWord) word()                  {}

// referentialToken wraps a parsed referential, folding away the one
// shape that has a simpler equivalent. §4.6.3 gives a suppletive
// cluster the referential's machinery so it can carry a Specification,
// affixes and a stacked case; a word using none of that says no more
// than the carrier adjunct of §4.5 does, and the epenthetic vowel in
// front of it is there to keep the parse unambiguous, not to mean
// anything. So it reads back as the plainer word.
func referentialToken(word string, r g.Referential) WordToken {
	if h, ok := r.Head.(g.SuppletiveHead); ok && r.Second == nil && !r.RpvEssence {
		return CarrierWord{Text: word, Carrier: g.CarrierAdjunct{Type: h.Type, Case: r.Case}}
	}
	return ReferentialWord{Text: word, Referential: r}
}

// ClassifyWord decides which WordToken variant a single romanization
// belongs to. The order of attempts is the docstring of the package.
//
// Non-Ithkuil characters (chars not in the V4 alphabet) reject the
// word up front. Stress-mark and per-slot phonotactic violations are
// only enforced on words that match the formative recognizer; biases,
// modulars and other adjunct shapes have their own phonotactic rules
// that diverge from the formative-shaped ones (e.g. modulars permit a
// word-final w, biases use the otherwise-prohibited geminates çç/ļļ).
func ClassifyWord(word string) WordToken {
	if word == "" {
		return UnknownWord{Text: word}
	}
	// Compose and lowercase before any classifier reads the letters;
	// see phonology.Normalize. Words we fail to classify keep their
	// original text: a carrier adjunct scopes over a following foreign
	// name ("hna John"), where capitalization is meaningful.
	orig := word
	word = phonology.Normalize(word)
	if _, err := phonology.ParseChain(word); err != nil {
		return UnknownWord{Text: orig}
	}
	// §4.8 parsing adjunct: 'V' is a fixed three-character word; check
	// before anything else so a leading glottal doesn't get reinterpreted
	// downstream.
	if pa, err := parse.ParseParsingAdjunct(word); err == nil {
		return ParsingAdjunctWord{Text: word, Adjunct: pa}
	}
	// Hyphenated input: try as a concatenation chain. A hyphen is only
	// meaningful as a concat-pair separator, so if the chain doesn't
	// parse, don't let other classifiers (whose input model has no
	// hyphen) snatch the word with a stretchy match.
	if strings.Contains(word, "-") {
		if c, ok := tryConcatenation(word); ok {
			return ConcatenatedFormativeWord{Text: word, Chain: c}
		}
		return UnknownWord{Text: orig}
	}
	conjs := phonology.SplitConjuncts(word)

	// 1. Single consonant cluster → Bias.
	if len(conjs) == 1 && phonology.IsConsonantConjunct(conjs[0]) {
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
	// formative parsing so that "hna" is a Naming carrier rather
	// than a formative with Cr=hn.
	if len(conjs) >= 2 && phonology.IsConsonantConjunct(conjs[0]) {
		if c, err := parse.ParseCarrier(word); err == nil {
			return CarrierWord{Text: word, Carrier: c}
		}
	}

	// 4. §4.6.3 Cp-in-referential epenthesis: "üo" + a Cp cluster
	//    (hl/hm/hn/hň) is a referential, not a modular adjunct — the
	//    "üo-" prefix exists precisely to disambiguate. Run referential
	//    first so the modular pattern doesn't snatch it.
	if len(conjs) >= 2 && conjs[0] == "üo" {
		if _, isCp := parse.ParseCarrierType(conjs[1]); isCp {
			if r, err := fullparse.Referential(word); err == nil {
				return referentialToken(word, r)
			}
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
	//     formative. A special-Vv marker (ae/ea/ëi/eë/ëu/oë) at conjs[0]
	//     means this is a §4.6.4 / §4.2 specialised root formative,
	//     not an affix — skip and let the formative recogniser handle it.
	if len(conjs) >= 2 && len(conjs) <= 3 &&
		phonology.IsVowelConjunct(conjs[0]) && conjs[0] != "ë" &&
		!parse.IsSpecialVv(conjs[0]) {
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

	// 5. Single/dual referential per §4.6.1:
	//    [ë]C1 Vc1 [w/y Vc2 [C2 [ë]]], with ultimate stress signalling
	//    the RPV essence override.
	if r, err := fullparse.Referential(word); err == nil {
		return referentialToken(word, r)
	}

	// 5b. Combination referential: [ë] C1 Vc Spec [VxCs...] [Vc2].
	if c, err := fullparse.CombinationReferential(word); err == nil {
		return CombinationRefWord{Text: word, Combination: c}
	}

	// 6. Formative. A Slot I C_C marker means another formative
	// follows, and §3.1.8 joins the two with a hyphen — which the
	// hyphenated branch above already claimed. So a C_C on a word with
	// no hyphen is not a chain, and the leading h-cluster is something
	// else: a foreign name approximated in Ithkuil letters, or a
	// dependent someone quoted without its parent.
	if f, err := fullparse.Formative(word); err == nil {
		if f.Concat == g.ConcatNone {
			return FormativeWord{Text: word, Formative: f}
		}
	}

	// A bare consonant cluster that decomposes as referents used to be
	// accepted here as a caseless referential. §4.6.1 leaves V_C1
	// unparenthesized in its slot table and gives "(ë)C(C)-V" as the
	// tell-tale shape, so a referential always carries a case, and a
	// word with no vowel in it at all is unpronounceable besides.
	return UnknownWord{Text: orig}
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
			// Foreign text is passthrough: take the raw field rather
			// than the discarded classification's rom, which has
			// been case-normalized as Ithkuil.
			out[i+1] = ForeignWord{Text: fields[i+1]}
		}
	}
	ResolveModularMood(out)
	return out
}

// ResolveModularMood fills in each ModularWord's MarksMood from the
// tokens around it: the flag says whether the next formative-bearing
// token is verbal, which is what disambiguates the adjunct's Cn
// between Mood and Case-Scope.
//
// It is derived rather than intrinsic, so nothing needs to store it.
// Any consumer that rebuilds a token stream from something other than
// romanization calls this to restore it.
func ResolveModularMood(toks []WordToken) {
	for i, t := range toks {
		mw, ok := t.(ModularWord)
		if !ok {
			continue
		}
		if verbal, found := nextFormativeIsVerbal(toks, i); found {
			mw.MarksMood = &verbal
			toks[i] = mw
		}
	}
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
// root is the carrier root "s" also do.
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

// tryConcatenation attempts to read word as a hyphen-joined formative
// chain (§3.1.7). Written order: every leading formative is a
// "concatenated" dependent carrying a Slot I Cc marker, and the LAST
// formative is the "parent" with no Cc. Returns ok=false if any
// constraint fails.
func tryConcatenation(word string) (*g.Chain, bool) {
	parts := strings.Split(word, "-")
	if len(parts) < 2 {
		return nil, false
	}
	formatives := make([]g.Formative, 0, len(parts))
	for _, p := range parts {
		f, err := fullparse.Formative(p)
		if err != nil {
			return nil, false
		}
		formatives = append(formatives, f)
	}
	last := len(formatives) - 1
	// Parent (last) must be plain.
	if formatives[last].Concat != g.ConcatNone {
		return nil, false
	}
	// Every leading dependent must carry a Cc.
	for i := 0; i < last; i++ {
		if formatives[i].Concat == g.ConcatNone {
			return nil, false
		}
	}
	chain := g.NewChain(formatives[last])
	for i := 0; i < last; i++ {
		switch formatives[i].Concat {
		case g.Type1:
			chain.AddType1(formatives[i])
		case g.Type2:
			chain.AddType2(formatives[i])
		}
	}
	return chain, true
}
