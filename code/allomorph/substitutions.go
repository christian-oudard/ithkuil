package allomorph

import (
	"strings"
	"unicode/utf8"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// rule is a (pattern, replacement) pair for a single substitution.
type rule struct{ from, to string }

// simpleSubstitutions apply unconditionally to all matches anywhere in
// the cluster. Order matters: each rule transforms the output of the
// previous one.
var simpleSubstitutions = []rule{
	{"pp", "mp"}, {"tt", "nt"}, {"kk", "nk"},
	{"pb", "mb"}, {"kg", "ng"},
	// "ngn" is the spec's named exception to the [C]gn → [C]ň rule
	// below. It has to fire here, after kg → ng has created the "ngn"
	// and before the general rule turns it into the §8-prohibited
	// "nň". It is not a [C] rule: MSC/CSL/A/GRA/RPV composes to a bare
	// "kgn", so the match sits at the head of the cluster.
	{"ngn", "ňn"},
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
//
// Two of these are Quijada's, written "fbm → (fv) → vw" and
// "ţbn → (tḑ) → ḑy", the parenthesis being the intermediate the
// context pass leaves behind. The third is ours; see ERRATA.md §3.6.
//
// The printed "(tḑ)" is a typo for "(ţḑ)". [C]bn → [C]ḑ rewrites only
// the "bn", so ţbn mechanically yields ţḑ and never tḑ, and the rule
// beside it is the exact parallel: both escape a §2.5 homologous
// voicing mismatch by voicing the fricative and opening the nasal into
// an approximant, f→v with m→w and ţ→ḑ with n→y. So the rule is about
// MDS, which composes ţbn.
//
// That leaves MSS, which composes tbn and lands on tḑ — barred by
// §2.2, a different rule, which is why it was never in scope for the
// v1.3 revision that added these two. Quijada supplies nothing for it.
// tḑ → ḑw is our amendment, keeping the interdental the composition
// was already heading for and varying only the approximant, so that ḑy
// and ḑw are a minimal pair the way vw sits beside them.
var secondPassSubstitutions = []rule{
	{"fv", "vw"},
	{"ţḑ", "ḑy"}, // Quijada's, with its intermediate corrected
	{"tḑ", "ḑw"}, // ours: the §2.2 case he left out
}

// UnresolvedCa reports whether s carries a Ca cluster we cannot
// compose into a sayable form. Nothing does any more: with the §3.6
// substitution family completed (see ERRATA.md §3.6) all 3840 Ca
// values compose to a legal cluster, and TestCaFormsAreLegal checks it.
//
// Kept because the round-trip and fuzz tests use it to excuse a Ca that
// fails the phonotactics, and that excuse should disappear by the
// predicate going quiet rather than by the call sites being edited out.
// If a future change to the tables reopens a hole, those tests start
// reporting it instead of silently tolerating it.
func UnresolvedCa(string) bool { return false }

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

// ConstructCa builds the written Ca consonant cluster for a SlotVI by
// composing the raw form and applying allomorphic substitutions.
func ConstructCa(s g.SlotVI) string {
	return ApplySubstitutions(ConstructCaRaw(s))
}
