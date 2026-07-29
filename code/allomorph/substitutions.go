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
	// and before the general rule turns it into the §2.23-prohibited
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
// The grammar writes these as "fbm → (fv) → vw" and "ţbn → (tḑ) →
// ḑy", the parenthesis being the intermediate the context pass leaves
// behind. For ţbn that intermediate is really "ţḑ": [C]bn → [C]ḑ
// rewrites only the "bn", so the ţ stays put. Taking the spec's "tḑ"
// at face value left the rule unreachable, and MDS/A/DPL/RPV stopped
// at "ţḑ", which §2.5 prohibits as a homologous voicing mismatch.
var secondPassSubstitutions = []rule{
	{"fv", "vw"}, {"tḑ", "ḑy"},
}

// UnresolvedCa reports whether s carries the one Ca cluster we do not
// compose into a sayable form: MDS/A/DPL/RPV, which lands on "ţḑ" and
// is barred by §2.5 as a homologous voicing mismatch. Matching on the
// substring covers the liquid-prefixed and geminated forms too.
//
// What is known, so the next person does not redo it:
//
// The §3.6 substitution "ţbn → (tḑ) → ḑy" cannot cover both cases that
// need it. MDS composes "ţbn" and MSS composes "tbn"; the rule names
// the first on its input side and the second in its intermediate, and
// routing both through it collapses two Ca values onto one romanization
// form that ParseCa cannot separate. Whichever we take, the other
// composes an unsayable cluster: "ţḑ" under §2.5 or "tḑ" under §2.2.
// We take the intermediate, which is the older reading here.
//
// Structure argues for the other one. This rule sits beside "fbm →
// (fv) → vw", and both escape the same thing: the general [C]bm/[C]bn
// rule producing a §2.5 homologous voicing mismatch. f/v and ţ/ḑ are
// exactly the two non-sibilant fricative voicing pairs, and applying
// [C]bn → [C]ḑ to "ţbn" mechanically yields "ţḑ", so the parenthesis
// looks like the typo rather than the input. That argument does not
// reduce the breakage, only move it, so it did not seem worth the
// churn on its own.
//
// Two hypotheses have been tested and refuted. Making both inputs map
// to "ḑy" collides, as above. Reading §3.6's "use the alternate
// Extension form when preceded by [C]t-, [C]k- or [C]p-" as "take the
// voiced alternate after a t/k/p Configuration" is much worse: it adds
// 128 §2.13 violations, 56 §2.4 and 8 §2.2. That rule means something
// else, and ConstructCaRaw does not implement it at all.
//
// The corpus cannot settle it. DPL+A+RPV appears zero times in
// Quijada's 384 official examples and zero times in the 3893-word
// Discord corpus, in any Configuration. This is an unvisited corner of
// the grammar, so there is no usage to appeal to and no cost to
// leaving it until someone can ask.
func UnresolvedCa(s string) bool { return strings.Contains(s, "ţḑ") }

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
