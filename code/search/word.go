package search

import (
	"strings"
	"unicode"
	"unicode/utf8"
)

// matchesTerms reports whether every word of the query matches a word
// of the text, comparing Porter stems, so "cats" finds a cat and
// "speaks" finds speech.
//
// This is for prose: a name, a description, a lexicon gloss. It is what
// the store's index does, and doing the same here is what keeps a
// browser's answers and a terminal's the same. Prefix matching is not
// used on prose because a three-letter query lands inside too much:
// "cat" prefix-matches 209 roots, most of them catfish, Catopuma and
// catastrophe, against 19 that are actually about cats.
func matchesTerms(text, query string) bool {
	if query == "" {
		return false
	}
	words := splitWords(text)
	if len(words) == 0 {
		return false
	}
	stems := make(map[string]bool, len(words))
	for _, w := range words {
		stems[Stem(w)] = true
	}
	for _, q := range splitWords(query) {
		if !stems[Stem(q)] {
			return false
		}
	}
	return true
}

// splitWords cuts text where a word ends: at anything that is not a
// letter or a digit.
func splitWords(text string) []string {
	return strings.FieldsFunc(strings.ToLower(text), func(r rune) bool {
		return !isWordRune(r)
	})
}

// matchesWord reports whether q begins a word somewhere in text, both
// compared case-insensitively.
//
// This is for identifiers rather than prose: an abbreviation, a written
// form, a category path. "BEN" has to find BEN1 and "trans" has to find
// Case/Transrelative, and neither is English for a stemmer to reduce.
//
// A plain substring test is wrong here and was wrong for a long time:
// searching the grammar for "ERG" answered with the Absolutive, whose
// description reads "PATIENT undergoing the act". Nothing about
// "undergoing" is about the Ergative, and a three-letter abbreviation
// is exactly the kind of query that lands inside unrelated English
// words. The lexicon had the same fault, so "cat" matched "indicate"
// and "communicate".
//
// The boundary is the start of a word rather than the whole of one, so
// "water" still finds "waterfall" and "SYS" still finds "systems".
// Trailing partial words are what a person typing a prefix expects to
// match; leading ones are not.
func matchesWord(text, q string) bool {
	if q == "" {
		return false
	}
	t := strings.ToLower(text)
	for i := 0; i <= len(t)-len(q); {
		j := strings.Index(t[i:], q)
		if j < 0 {
			return false
		}
		at := i + j
		if at == 0 {
			return true
		}
		if r, _ := utf8.DecodeLastRuneInString(t[:at]); !isWordRune(r) {
			return true
		}
		i = at + 1
	}
	return false
}

// isWordRune is what counts as being inside a word. Letters and digits
// only: a hyphen, a slash or a bracket ends one, so "Case/Transrelative"
// answers to "trans" and an affix degree written "(ASR) Assertive"
// answers to "assertive".
func isWordRune(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsDigit(r)
}
