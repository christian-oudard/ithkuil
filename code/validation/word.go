package validation

import (
	"fmt"
	"strings"
	"unicode/utf8"

	"github.com/christian-oudard/ithkuil/surface"
)

// ithkuilRunes is the set of characters that may appear in well-formed
// Ithkuil V4 surface text — the 31 consonants, the 9 base vowels, the
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

// ValidateChars reports any character in word that isn't part of the
// V4 alphabet (consonants, vowels with diacritic variants, glottal,
// hyphen). Capital letters are folded to lowercase first — case is
// orthographic in V4, not phonemic. The error names each offending
// rune with its codepoint.
func ValidateChars(word string) Result {
	var bad []rune
	for _, r := range strings.ToLower(word) {
		if !ithkuilRunes[r] {
			bad = append(bad, r)
		}
	}
	if len(bad) == 0 {
		return Result{Valid: true}
	}
	parts := make([]string, 0, len(bad))
	for _, r := range bad {
		parts = append(parts, fmt.Sprintf("%q (U+%04X)", r, r))
	}
	// Cluster stays empty. This rule is about the whole word, not a
	// conjunct in it, and filling the field made reports read
	// "non-Ithkuil characters: 'q' (U+0071) (cluster akxq)" — calling
	// the entire word a cluster. The offending runes are named in the
	// reason already.
	return Result{
		Valid: false,
		Errors: []Error{{
			Rule:   "chars",
			Reason: "non-Ithkuil characters: " + strings.Join(parts, ", "),
		}},
	}
}

// ValidateWord runs the full battery of phonotactic checks on a single
// word: stress placement, each consonant cluster (initial/medial/final
// per its position), and each vowel sequence. Returns a Result whose
// Errors slice collects every violation.
func ValidateWord(word string) Result {
	if word == "" {
		return Result{Valid: true}
	}

	// §3.1.7 concatenation chains are written with a hyphen, and each
	// link carries its own stress and its own word-initial and
	// word-final positions. Checking the joined string instead reads
	// two stress marks as one over-marked word and mistakes both
	// interior edges for medial clusters.
	if strings.Contains(word, "-") {
		var errs []Error
		for _, part := range strings.Split(word, "-") {
			if r := ValidateWord(part); !r.Valid {
				errs = append(errs, r.Errors...)
			}
		}
		if len(errs) == 0 {
			return Result{Valid: true}
		}
		return Result{Valid: false, Errors: errs}
	}

	// Non-Ithkuil characters mean no other check can be trusted —
	// short-circuit so callers see the chars error alone, not a pile of
	// downstream cluster/stress complaints derived from garbage input.
	if cr := ValidateChars(word); !cr.Valid {
		return cr
	}

	// Stress/cluster lookups all assume lowercase; fold here so that
	// orthographic capitals (sentence-initial, proper nouns) don't
	// trip downstream tables.
	word = strings.ToLower(word)

	var errs []Error

	// Stress validation.
	if _, err := ValidateStress(word); err != nil {
		if se, ok := err.(StressError); ok {
			errs = append(errs, Error{
				Rule:    "stress",
				Cluster: word,
				Reason:  se.Error(),
			})
		}
	}

	// The stress mark is suprasegmental: it rides on a vowel but is not
	// one of the nine. ValidateStress above has read it, so take it off
	// before any cluster or vowel-sequence lookup, which all key on the
	// bare letters.
	word, _ = surface.Strip(word)

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
	// Two forms stay unsettled and are simply admitted with the rest.
	// SAT's spelling is itself in dispute — the grammar prints lţ,
	// data.json has ļţ, the corpus uses both — and neither is licensed.
	// ARB xtļ is neither permitted nor prohibited: §3.2.3 licenses xt
	// word-initially but §3.3 grants no triple beginning with x-.
	conjs := surface.SplitConjuncts(word)
	if len(conjs) == 1 {
		firstRune, _ := utf8.DecodeRuneInString(conjs[0])
		if !surface.IsVowel(firstRune) {
			if len(errs) == 0 {
				return Result{Valid: true}
			}
			return Result{Valid: false, Errors: errs}
		}
	}

	// Walk conjuncts, classifying each as a consonant cluster or vowel
	// sequence and tracking position.
	for i, c := range conjs {
		if c == "" {
			continue
		}
		firstRune, _ := utf8.DecodeRuneInString(c)
		if surface.IsVowel(firstRune) {
			res := ValidateVowelSequence(c)
			if !res.Valid {
				errs = append(errs, res.Errors...)
			}
			continue
		}
		pos := Medial
		if i == 0 {
			pos = Initial
		} else if i == len(conjs)-1 {
			pos = Final
		}
		res := ValidateClusterAt(pos, c)
		if !res.Valid {
			errs = append(errs, res.Errors...)
		}
	}

	if len(errs) == 0 {
		return Result{Valid: true}
	}
	return Result{Valid: false, Errors: errs}
}
