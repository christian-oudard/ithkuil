package phonology

import (
	"errors"
	"fmt"
	"strings"
	"unicode/utf8"

	"github.com/christian-oudard/ithkuil/fault"
)

// ithkuilRunes is the set of characters that may appear in well-formed
// Ithkuil V4 romanization — the 31 consonants, the 9 base vowels, the
// 9 stressed forms (acute and circumflex; "i" has no umlaut so no î),
// the glottal stop, and the concatenation hyphen.
var ithkuilRunes = func() map[rune]bool {
	m := make(map[rune]bool)
	for _, r := range "pbtdkgfvţḑszšžçxhļcẓčjmnňrlwyř" {
		m[r] = true
	}
	for _, r := range "aäeëioöuü" {
		m[r] = true
	}
	for _, r := range "áéíóú" {
		m[r] = true
	}
	for _, r := range "âêôû" {
		m[r] = true
	}
	m['\''] = true
	m['-'] = true
	return m
}()

// Word is text that has been read as phonology: normalized, checked
// against the phonotactics, and split into the pieces every later layer
// needs. Only ParseWord builds one, so holding a Word is the evidence
// that its text is pronounceable Ithkuil, and nothing downstream has to
// check again or re-derive the split.
//
// The zero Word is not valid input to anything.
type Word struct {
	text   string   // normalized, stress mark intact
	bare   string   // the same text with the stress mark removed
	stress Stress   // where the mark put the stress
	conjs  []string // bare, split into vowel and consonant runs
}

// String returns the normalized text, stress mark and all.
func (w Word) String() string { return w.text }

// Bare returns the text with the stress mark removed. The mark is
// suprasegmental: it rides on a vowel without being one of the nine, so
// every table lookup keys on this rather than on String.
func (w Word) Bare() string { return w.bare }

// Stress returns the stress position the mark put on the word.
func (w Word) Stress() Stress { return w.stress }

// Conjuncts returns the bare text split into vowel and consonant runs,
// with glottal-stop vowel forms merged.
func (w Word) Conjuncts() []string { return append([]string(nil), w.conjs...) }

// sound builds a Sound-stage fault. Every §2 rule, every stress mark
// and every vowel sequence in this package fails at that one stage, so
// naming it at each site would be noise; the exception is CheckChars,
// which is a stage earlier and says so itself.
func sound(rule, cluster, reason string) fault.Fault {
	return fault.Fault{Stage: fault.Sound, Code: rule, Found: cluster, Fix: reason}
}

// CheckChars reports any character in text that isn't part of the V4
// alphabet (consonants, vowels with diacritic variants, glottal,
// hyphen). Capital letters are folded to lowercase first — case is
// orthographic in V4, not phonemic. The fault names each offending
// rune with its codepoint.
func CheckChars(word string) []fault.Fault {
	var bad []rune
	for _, r := range strings.ToLower(word) {
		if !ithkuilRunes[r] {
			bad = append(bad, r)
		}
	}
	if len(bad) == 0 {
		return nil
	}
	parts := make([]string, 0, len(bad))
	for _, r := range bad {
		parts = append(parts, fmt.Sprintf("%q (U+%04X)", r, r))
	}
	// Found stays empty. This rule is about the whole word, not a
	// conjunct in it, and filling the field made reports read
	// "non-Ithkuil characters: 'q' (U+0071) (cluster akxq)" — calling
	// the entire word a cluster. The offending runes are named in the
	// fix already.
	return []fault.Fault{{
		Stage: fault.Chars,
		Code:  "chars",
		Fix:   "remove the non-Ithkuil characters: " + strings.Join(parts, ", "),
	}}
}

// ParseWord reads one word as phonology: it normalizes the text,
// rejects anything outside the alphabet, reads the stress mark, and
// splits the rest into conjuncts. It is the only constructor of Word,
// so every later layer can take the reading as done.
//
// It does not judge the §2 cluster rules. Reading a word and holding an
// opinion about whether it is well-formed are different things: the Ca
// tables generate a few clusters our own reading of §2 rejects, so a
// parser that refused them could not round-trip its own output. Ask a
// Word for its Violations where that judgment is wanted.
//
// A §3.1.7 concatenation chain is not one word. Each link carries its
// own stress and its own word-initial and word-final positions, so use
// ParseChain for text that may hold a hyphen.
func ParseWord(text string) (Word, error) {
	if text == "" {
		return Word{}, fault.One(text, sound("empty", "", "a word needs at least one conjunct"))
	}
	if strings.Contains(text, "-") {
		return Word{}, fault.One(text, sound("chain", text,
			"a hyphen joins a concatenation chain; read each link on its own"))
	}
	// Compose and lowercase before anything reads the letters, so every
	// rule sees one spelling; see Normalize.
	word := Normalize(text)

	// Non-Ithkuil characters mean no other check can be trusted, so
	// report them alone rather than a pile of downstream cluster and
	// stress complaints derived from garbage.
	if v := CheckChars(word); v != nil {
		return Word{}, fault.Faults{Word: word, List: v}
	}

	bare, stress := Strip(word)
	if stress == InvalidStress {
		return Word{}, fault.One(word, sound("stress", word, DoubleMarkedStress.Error()))
	}
	return Word{
		text:   word,
		bare:   bare,
		stress: stress,
		conjs:  MergeGlottalVowels(SplitConjuncts(bare)),
	}, nil
}

// ParseChain reads text that may be a §3.1.7 concatenation chain,
// returning one Word per link. A word with no hyphen is a chain of one.
func ParseChain(text string) ([]Word, error) {
	parts := strings.Split(text, "-")
	words := make([]Word, 0, len(parts))
	var vs []fault.Fault
	for _, part := range parts {
		w, err := ParseWord(part)
		if err != nil {
			var ill fault.Faults
			if errors.As(err, &ill) {
				vs = append(vs, ill.List...)
				continue
			}
			return nil, err
		}
		words = append(words, w)
	}
	if len(vs) > 0 {
		return nil, fault.Faults{Word: text, List: vs}
	}
	return words, nil
}

// Violations lists the phonotactic rules the word breaks: the stress
// mark's placement, each vowel sequence, and each consonant cluster
// under the rules for the position it sits in. An empty result is a
// well-formed word.
//
// This is a judgment about a word already read, not part of reading it.
// See ParseWord for why the two are separate.
func (w Word) Violations() []fault.Fault {
	var vs []fault.Fault
	if _, err := ValidateStress(w.text); err != nil {
		if se, ok := err.(StressError); ok {
			vs = append(vs, sound("stress", w.text, se.Error()))
		}
	}
	return append(vs, clusterViolations(w.bare)...)
}

// Legal reports whether text is a well-formed Ithkuil word or chain:
// it reads as phonology and breaks no phonotactic rule. Generators
// building candidate romanizations use it, where there is no reading to
// keep, only a yes or a no.
func Legal(text string) bool {
	words, err := ParseChain(text)
	if err != nil {
		return false
	}
	for _, w := range words {
		if len(w.Violations()) > 0 {
			return false
		}
	}
	return true
}

// CheckText reads text and collects every phonotactic rule it breaks,
// chain links included. A nil error means the text is well-formed.
func CheckText(text string) error {
	words, err := ParseChain(text)
	if err != nil {
		return err
	}
	var vs []fault.Fault
	for _, w := range words {
		vs = append(vs, w.Violations()...)
	}
	if len(vs) > 0 {
		return fault.Faults{Word: text, List: vs}
	}
	return nil
}

// clusterViolations walks a bare word's conjuncts and collects what
// each one breaks: vowel sequences by their own rules, consonant
// clusters by the rules for the position they sit in.
func clusterViolations(bare string) []fault.Fault {
	conjs := SplitConjuncts(bare)

	// Single-consonant-conjunct words are stand-alone Bias adjuncts;
	// their cluster table is authoritative and may legitimately contain
	// shapes the §2 root/affix rules disallow (e.g. "pļļ" CMD, "kçç"
	// EXA). Skip cluster validation in that case.
	//
	// The exemption is not for a handful of awkward forms. A bias
	// adjunct is a bare consonant conjunct standing alone as a word, a
	// shape §1.4 and §§3-4 never contemplate — every rule there is
	// written about a conjunct with a vowel-form beside it, §4.1 opening
	// "A single word-final consonant following a vowel-form". Taking the
	// word-initial inventory at its word, 34 of the 61 forms are
	// unlicensed: §3.2.9 grants word-initial l- and r- only -w or -y,
	// which fails ACC lf, ANP lst and nine more; §3.2.8 grants nasals a
	// liquid or approximant, which fails ATE ňj, RSG msf and nine more;
	// ř- is granted no word-initial pair at all, failing APB řs, DOL řřx
	// and IVD řřn; and §3.2.1, §3.2.2, §3.3 and §3.3.4 take one apiece.
	//
	// None of them is hypothetical: every one is attested standing alone
	// in the community corpus, pļļ 291 times, msf 127, kçç 48, cč 33.
	// So the table is taken as authoritative and §3's word-initial rules
	// as scoped to words that have a vowel in them.
	//
	// One form stays unsettled and is simply admitted with the rest.
	// ARB xtļ is neither permitted nor prohibited: §3.2.3 licenses xt
	// word-initially but §3.3 grants no triple beginning with x-.
	if len(conjs) == 1 {
		firstRune, _ := utf8.DecodeRuneInString(conjs[0])
		if !IsVowel(firstRune) {
			return nil
		}
	}

	var vs []fault.Fault
	for i, c := range conjs {
		if c == "" {
			continue
		}
		firstRune, _ := utf8.DecodeRuneInString(c)
		if IsVowel(firstRune) {
			vs = append(vs, VowelSequenceViolations(c)...)
			continue
		}
		pos := Medial
		switch {
		case i == 0:
			pos = Initial
		case i == len(conjs)-1:
			pos = Final
		}
		vs = append(vs, ClusterViolationsAt(pos, c)...)
	}
	return vs
}
