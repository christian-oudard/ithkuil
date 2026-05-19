package allomorph

import (
	"strings"
	"unicode/utf8"

	g "github.com/coudard/ithkuil/go/grammar"
)

// rule is a (pattern, replacement) pair for a single substitution.
type rule struct{ from, to string }

// simpleSubstitutions apply unconditionally to all matches anywhere in
// the cluster. Order matters: each rule transforms the output of the
// previous one.
var simpleSubstitutions = []rule{
	{"pp", "mp"}, {"tt", "nt"}, {"kk", "nk"},
	{"pb", "mb"}, {"kg", "ng"},
	{"ll", "pļ"}, {"rr", "ns"},
	{"çy", "nd"},
	{"řř", "ňš"}, {"rř", "nš"}, {"řr", "ňs"},
}

// contextSubstitutions only fire when the match is preceded by at least
// one character — i.e., not at the very start of the cluster. The
// grammar writes these as "[C]X → [C]Y".
var contextSubstitutions = []rule{
	{"gm", "x"}, {"gn", "ň"},
	{"bm", "v"}, {"bn", "ḑ"},
	{"çx", "xw"},
}

// secondPassSubstitutions clean up intermediate forms produced by the
// context pass. They apply unconditionally like simpleSubstitutions.
var secondPassSubstitutions = []rule{
	{"fv", "vw"}, {"tḑ", "ḑy"},
}

// replaceNonInitial replaces every occurrence of from with to in s,
// except when the match begins at byte 0. The check is implemented by
// peeling off the first rune and running ReplaceAll on the remainder.
func replaceNonInitial(s, from, to string) string {
	if len(s) < len(from) || s == "" {
		return s
	}
	r, size := utf8.DecodeRuneInString(s)
	return string(r) + strings.ReplaceAll(s[size:], from, to)
}

// ApplySubstitutions applies the §3.6 Ca substitution rules in order:
// simple → context → second-pass.
func ApplySubstitutions(s string) string {
	for _, r := range simpleSubstitutions {
		s = strings.ReplaceAll(s, r.from, r.to)
	}
	for _, r := range contextSubstitutions {
		s = replaceNonInitial(s, r.from, r.to)
	}
	for _, r := range secondPassSubstitutions {
		s = strings.ReplaceAll(s, r.from, r.to)
	}
	return s
}

// ConstructCa builds the surface Ca consonant cluster for a SlotVI by
// composing the raw form and applying allomorphic substitutions.
func ConstructCa(s g.SlotVI) string {
	return ApplySubstitutions(ConstructCaRaw(s))
}
