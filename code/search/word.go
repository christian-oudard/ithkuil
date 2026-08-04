package search

import (
	"strings"
	"unicode"
	"unicode/utf8"
)

// matchesWord reports whether q begins a word somewhere in text, both
// compared case-insensitively.
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
