package roman

import (
	"errors"
	"fmt"
	"strings"

	"github.com/christian-oudard/ithkuil/fault"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
)

// Result is what reading one written word produced: the grammar, or
// the reason there is none.
//
// The romanization is kept beside the Word rather than inside it. A
// Word is grammar and holds no text, so anything that wants to show
// the user what they typed — an error message, a table header — takes
// it from here.
type Result struct {
	// Romanization is the word as read, normalized.
	Romanization string
	// Word is the grammar it carries, nil when Err is set.
	Word g.Word
	// Err says why no word could be read.
	Err error
}

// referentialToken wraps a parsed referential, folding away the one
// shape that has a simpler equivalent. §4.6.3 gives a suppletive
// cluster the referential's machinery so it can carry a Specification,
// affixes and a stacked case; a word using none of that says no more
// than the carrier adjunct of §4.5 does, and the epenthetic vowel in
// front of it is there to keep the parse unambiguous, not to mean
// anything. So it reads back as the plainer word.
func referentialWord(r g.Referential) g.Word {
	if h, ok := r.Head.(g.SuppletiveHead); ok && r.Second == nil && !r.RpvEssence {
		return g.CarrierAdjunct{Type: h.Type, Case: r.Case}
	}
	return r
}

// ParseWord decides which grammar.Word variant a single romanization
// belongs to. It tries the recognizers tightest-first, so the first one
// that fits wins:
//
//  1. Pure-consonant single conjunct → Bias.
//  2. Recognized register opener/closer → RegisterMarker.
//  3. Recognized carrier consonant + vowel → CarrierAdjunct.
//  4. Vowel + valid Cn consonant → ModularAdjunct.
//  5. A referential, then a combination referential.
//  6. Anything else that parses as a formative → Formative.
//
// Nothing matching is an error, not a variant; see the Word doc in
// package grammar for why an unreadable stretch is not a kind of word.
//
// Non-Ithkuil characters (chars not in the V4 alphabet) reject the
// word up front. Stress-mark and per-slot phonotactic violations are
// only enforced on words that match the formative recognizer; biases,
// modulars and other adjunct shapes have their own phonotactic rules
// that diverge from the formative-shaped ones (e.g. modulars permit a
// word-final w, biases use the otherwise-prohibited geminates çç/ļļ).
func ParseWord(word string) (g.Word, error) {
	if word == "" {
		return nil, fault.One(word, fault.Fault{
			Stage: fault.Shape, Code: "shape", Fix: "a word needs at least one conjunct",
		})
	}
	// Compose and lowercase before any classifier reads the letters;
	// see phonology.Normalize. Words we fail to classify keep their
	// original text: a carrier adjunct scopes over a following foreign
	// name ("hna John"), where capitalization is meaningful.
	word = phonology.Normalize(word)
	if _, err := phonology.ParseChain(word); err != nil {
		return nil, err
	}
	// Hyphenated input: try as a concatenation chain. A hyphen is only
	// meaningful as a concat-pair separator, so if the chain doesn't
	// parse, don't let other classifiers (whose input model has no
	// hyphen) snatch the word with a stretchy match.
	if strings.Contains(word, "-") {
		// A hyphen is only meaningful as a concat-pair separator, so a
		// word carrying one is a chain or nothing. No other class is
		// offered it, and the chain reader's complaint is the answer
		// rather than one candidate among several.
		return tryConcatenation(word)
	}
	conjs := phonology.SplitConjuncts(word)

	// best keeps the complaint from whichever class read furthest.
	// Every attempt below that fails has something to say, and nine of
	// the ten are noise — "this is not a bias adjunct" describes a word
	// nobody was writing. The stage ordering picks the one that is not
	// noise: a class that got as far as reading slot values had a shape
	// it recognized, and a class that failed on the shape never did.
	var best fault.Faults
	consider := func(err error) {
		var fs fault.Faults
		if errors.As(err, &fs) && (best.List == nil || fs.Stage() >= best.Stage()) {
			best = fs
		}
	}

	// 1. Single consonant cluster → Bias.
	if len(conjs) == 1 && phonology.IsConsonantConjunct(conjs[0]) {
		if b, ok := parse.ParseBias(conjs[0]); ok {
			return b, nil
		}
	}

	// 2. Register opener or closer (whole word, no splitting).
	if r, ok := parse.ParseRegister(word); ok {
		return g.RegisterMarker{Register: r}, nil
	}
	if r, ok := parse.ParseRegisterFinal(word); ok {
		return g.RegisterMarker{Register: r, End: true}, nil
	}

	// 3. Carrier adjunct: word starting with a carrier consonant
	// (hl/hm/hn/hň) followed by trailing content. Tried before
	// formative parsing so that "hna" is a Naming carrier rather
	// than a formative with Cr=hn.
	if len(conjs) >= 2 && phonology.IsConsonantConjunct(conjs[0]) {
		c, err := parse.ParseCarrier(word)
		if err == nil {
			return c, nil
		}
		consider(err)
	}

	// 4. §4.6.3 Cp-in-referential epenthesis: "üo" + a Cp cluster
	//    (hl/hm/hn/hň) is a referential, not a modular adjunct — the
	//    "üo-" prefix exists precisely to disambiguate. Run referential
	//    first so the modular pattern doesn't snatch it.
	if len(conjs) >= 2 && conjs[0] == "üo" {
		if _, isCp := parse.ParseCarrierType(conjs[1]); isCp {
			r, err := ParseReferential(word)
			if err == nil {
				return referentialWord(r), nil
			}
			consider(err)
		}
	}

	// 4. Modular adjunct: 1-7 conjuncts of the shape
	//    [w/y] (Vn Cn){0-3} V(final). Single-vowel words are valid as
	//    "lone aspect" modulars (§4.3). Larger forms can have up to
	//    three VnCn pairs plus a final vowel, plus an optional w/y
	//    scope prefix.
	if m, err := parse.ParseModular(word); err == nil {
		return m, nil
	} else {
		consider(err)
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
		a, err := parse.ParseSingleAffix(word)
		if err == nil {
			return a, nil
		}
		consider(err)
	}

	// 4b. Multi-affix adjunct (§4.1.2): [ë] C V Cz V C ... [V]. The Cz
	//     consonant ('h, 'hl, 'hr, hw, 'hw or h) at the third post-ë
	//     position is what distinguishes this shape from a same-length
	//     consonant-initial formative.
	if a, err := parse.ParseMultipleAffix(word); err == nil {
		return a, nil
	} else {
		consider(err)
	}

	// 5. Single/dual referential per §4.6.1:
	//    [ë]C1 Vc1 [w/y Vc2 [C2 [ë]]], with ultimate stress signalling
	//    the RPV essence override.
	if r, err := ParseReferential(word); err == nil {
		return referentialWord(r), nil
	} else {
		consider(err)
	}

	// 5b. Combination referential: [ë] C1 Vc Spec [VxCs...] [Vc2].
	if c, err := ParseCombinationReferential(word); err == nil {
		return c, nil
	} else {
		consider(err)
	}

	// 6. Formative. A Slot I C_C marker means another formative
	// follows, and §3.1.8 joins the two with a hyphen — which the
	// hyphenated branch above already claimed. So a C_C on a word with
	// no hyphen is not a chain, and the leading h-cluster is something
	// else: a foreign name approximated in Ithkuil letters, or a
	// dependent someone quoted without its parent.
	if f, err := ParseFormative(word); err == nil {
		if f.Concat == g.ConcatNone {
			return f, nil
		}
	} else {
		consider(err)
	}

	// A bare consonant cluster that decomposes as referents used to be
	// accepted here as a caseless referential. §4.6.1 leaves V_C1
	// unparenthesized in its slot table and gives "(ë)C(C)-V" as the
	// tell-tale shape, so a referential always carries a case, and a
	// word with no vowel in it at all is unpronounceable besides.
	// A word that reached the value stage under some class failed for
	// a reason worth printing: its shape was recognized and a slot in
	// it was not. Anything less is a word no class recognized at all,
	// and saying so is the whole of what we know.
	if best.List != nil {
		return nil, best
	}
	return nil, fault.One(word, fault.Fault{
		Stage: fault.Shape,
		Code:  "shape",
		Fix:   "no word class has this shape: not a formative, referential, adjunct, register marker or bias",
	})
}

// Tokenize reads a span of romanization into one Result per word.
//
// Two things here are decided across words rather than within one. A
// carrier adjunct scopes the word after it as foreign text, and a §4.8
// parsing adjunct declares the stress of the word after it — which is
// phonology, not grammar, so the adjunct is consumed rather than read
// as a word and never appears in the result.
//
// A word that cannot be read leaves its Result carrying the reason
// instead of a Word. Reading is per-word, so one unreadable word does
// not cost the rest of the span.
func Tokenize(sentence string) []Result {
	fields := consumeParsingAdjuncts(strings.Fields(sentence))
	out := make([]Result, len(fields))
	for i, w := range fields {
		word, err := ParseWord(w)
		out[i] = Result{Romanization: phonology.Normalize(w), Word: word, Err: err}
	}
	for i := 0; i+1 < len(out); i++ {
		if isCarrier(out[i].Word) {
			// Foreign text is passthrough: take the raw field, not the
			// discarded reading's romanization, which was normalized as
			// Ithkuil.
			out[i+1] = Result{
				Romanization: fields[i+1],
				Word:         g.Foreign{Text: fields[i+1]},
			}
		}
	}
	return out
}

// Text reads a span of romanization into grammar, and fails if any
// word in it cannot be read. Callers that want to report per-word and
// carry on — the analyzer, mostly — use Tokenize instead.
func ParseText(sentence string) (g.Text, error) {
	results := Tokenize(sentence)
	out := make(g.Text, 0, len(results))
	for _, r := range results {
		if r.Err != nil {
			return nil, fmt.Errorf("%s: %w", r.Romanization, r.Err)
		}
		out = append(out, r.Word)
	}
	return out, nil
}

// Words drops the provenance, for callers that only want the grammar
// of a span they already know reads cleanly.
func Words(results []Result) g.Text {
	out := make(g.Text, 0, len(results))
	for _, r := range results {
		if r.Word != nil {
			out = append(out, r.Word)
		}
	}
	return out
}

// consumeParsingAdjuncts applies each §4.8 adjunct's stress
// declaration to the word it precedes and drops the adjunct. A
// declaration the following word contradicts is left alone: the
// classifier will fail on the word itself and report that, which
// names the real problem better than a rewritten word would.
func consumeParsingAdjuncts(fields []string) []string {
	out := make([]string, 0, len(fields))
	for i := 0; i < len(fields); i++ {
		declared, ok := phonology.ParsingAdjunct(fields[i])
		if !ok || i+1 >= len(fields) {
			out = append(out, fields[i])
			continue
		}
		i++
		if marked, err := phonology.DeclareStress(fields[i], declared); err == nil {
			out = append(out, marked)
		} else {
			out = append(out, fields[i])
		}
	}
	return out
}

// ModularIsVerbal reports whether the formative a modular adjunct at
// index i applies to is verbal, which is what decides whether its Cn
// reads as Mood or as Case-Scope.
//
// It is asked of the span rather than stored on the adjunct. The fact
// belongs to the arrangement of words, not to any one of them, and
// storing it meant every caller that built a span from something other
// than text had to remember to recompute it.
func ModularIsVerbal(t g.Text, i int) (verbal, found bool) {
	for j := i + 1; j < len(t); j++ {
		switch w := t[j].(type) {
		case g.Formative:
			return g.IsVerbal(w.Final), true
		case *g.Chain:
			fs := w.Formatives()
			if len(fs) == 0 {
				return false, false
			}
			return g.IsVerbal(fs[len(fs)-1].Final), true
		}
	}
	return false, false
}

// isCarrier reports whether a word scopes the foreign text after it.
// A carrier adjunct always does; so does a formative on the carrier
// root "s".
func isCarrier(w g.Word) bool {
	switch v := w.(type) {
	case g.CarrierAdjunct:
		return true
	case g.Formative:
		cr, ok := v.Root.(g.CrRoot)
		return ok && cr.Cluster == "s"
	}
	return false
}

// tryConcatenation attempts to read word as a hyphen-joined formative
// chain (§3.1.7). Written order: every leading formative is a
// "concatenated" dependent carrying a Slot I Cc marker, and the LAST
// formative is the "parent" with no Cc. Returns ok=false if any
// constraint fails.
func tryConcatenation(word string) (*g.Chain, error) {
	parts := strings.Split(word, "-")
	if len(parts) < 2 {
		return nil, fault.One(word, fault.Fault{
			Stage: fault.Shape, Code: "chain", Found: word,
			Fix: "a hyphen joins two or more formatives, and this has one",
		})
	}
	formatives := make([]g.Formative, 0, len(parts))
	for _, p := range parts {
		f, err := ParseFormative(p)
		if err != nil {
			// The link's own complaint, relabelled to the link. A
			// chain fails because one of its formatives does, and
			// saying only that the whole word is not a chain hands
			// the reader back what they typed and leaves them to
			// bisect it.
			var fs fault.Faults
			if errors.As(err, &fs) {
				fs.Word = p
				return nil, fs
			}
			return nil, err
		}
		formatives = append(formatives, f)
	}
	last := len(formatives) - 1
	// §3.1.7 puts the parent last and gives it no marker, and every
	// link in front of it one. Both halves are named against the link
	// that broke them, because which link is wrong is the whole of
	// what a reader cannot see for themselves.
	if formatives[last].Concat != g.ConcatNone {
		return nil, fault.One(parts[last], fault.Fault{
			Stage: fault.Value, Code: "Cc", Found: parts[last],
			Fix: "§3.1.7 puts the parent last and gives it no concatenation marker, and this one carries " +
				formatives[last].Concat.String(),
		})
	}
	for i := 0; i < last; i++ {
		if formatives[i].Concat == g.ConcatNone {
			return nil, fault.One(parts[i], fault.Fault{
				Stage: fault.Value, Code: "Cc", Found: parts[i],
				Fix: "§3.1.7 gives every dependent a concatenation marker, and this one before the parent has none",
			})
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
	return chain, nil
}
