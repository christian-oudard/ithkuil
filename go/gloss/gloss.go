// Package gloss produces a human-readable morphological gloss of a
// grammar.Formative. The output format is hyphen-separated slot
// abbreviations with grammatical defaults suppressed, e.g.
//
//	S2/PRC-ml-DYN/OBJ-MSS.G-ERG
//
// When a lexicon is supplied (via Glosser.Lex), roots show their
// stem-selected meaning and affixes show their ABBREV/degree form;
// otherwise both fall back to raw consonant clusters.
package gloss

import (
	"fmt"
	"strings"

	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/lexicon"
	"github.com/coudard/ithkuil/go/parse"
)

// Glosser carries the optional lexicon used for enriching root and
// affix glosses. A zero-value Glosser produces the same output as the
// package-level Formative function.
type Glosser struct {
	Lex *lexicon.Lexicon
}

// Formative renders a one-line gloss of f. Components at their default
// values are suppressed. Stress is shown explicitly as a trailing tag
// (MONO/ULT/ANT); penultimate stress is the unmarked default and stays
// off the gloss. A leading "§" marks a sentence-starter formative (one
// that carried a ç prefix in the surface form). Cs-root formatives
// render their root as "(Cs)/degree" to distinguish them from regular
// roots.
func (gl *Glosser) Formative(f g.Formative) string {
	parts := []string{
		slotI(f.SlotI),
		slotII(f.SlotII),
		gl.rootOrCsRoot(f),
		slotIV(f.SlotIV),
		gl.affixes(f.SlotV),
		slotVI(f.SlotVI),
		gl.affixes(f.SlotVII),
		slotVIII(f.SlotVIII),
		slotIX(f.SlotIX),
		stressTag(f.Stress),
	}
	body := strings.Join(nonEmpty(parts), "-")
	if f.SentenceStarter {
		return "§ " + body
	}
	return body
}

// rootOrCsRoot routes to either the regular root gloss or the Cs-root
// variant. Cs-roots show the Cs identifier (looked up in the affix
// lexicon if available) followed by the degree, e.g. "(NEG)/6".
func (gl *Glosser) rootOrCsRoot(f g.Formative) string {
	if f.CsRootDegree == nil {
		return gl.root(f.SlotII.Stem, f.SlotIII)
	}
	cs := string(f.SlotIII)
	abbr := cs
	if gl.Lex != nil {
		if entry, ok := gl.Lex.Affixes[cs]; ok {
			abbr = entry.Abbrev
		}
	}
	return fmt.Sprintf("(%s)/%d", abbr, *f.CsRootDegree)
}

// stressTag emits a tag for each non-default stress value. Penultimate
// is the unmarked default (omitted). The tag is independent of Slot IX
// shape so a Formative's stress can be read directly off the gloss.
func stressTag(s g.Stress) string {
	switch s {
	case g.Monosyllabic:
		return "MONO"
	case g.Ultimate:
		return "ULT"
	case g.Antepenultimate:
		return "ANT"
	}
	return ""
}

// Formative is the no-lexicon convenience form of Glosser.Formative.
func Formative(f g.Formative) string {
	return (&Glosser{}).Formative(f)
}

func nonEmpty(parts []string) []string {
	out := parts[:0]
	for _, p := range parts {
		if p != "" {
			out = append(out, p)
		}
	}
	return out
}

func slotI(c *g.ConcatenationStatus) string {
	if c == nil {
		return ""
	}
	switch *c {
	case g.Type1:
		return "T1"
	case g.Type2:
		return "T2"
	}
	return ""
}

func slotII(s g.SlotII) string {
	if s == g.DefaultSlotII {
		return ""
	}
	return fmt.Sprintf("%s/%s", s.Stem, s.Version)
}

// root renders the root slot. With a lexicon, the cluster is followed
// by the stem-selected meaning in single quotes. Without, just the
// surface cluster between dashes ("-ml-").
func (gl *Glosser) root(stem g.Stem, r g.Root) string {
	cr := string(r)
	if cr == "" {
		return ""
	}
	if gl.Lex != nil {
		if entry, ok := gl.Lex.Roots[cr]; ok {
			meaning := entry.Stem(stemIndex(stem))
			if meaning != "" {
				return "-" + cr + "- '" + meaning + "'"
			}
		}
	}
	return "-" + cr + "-"
}

// stemIndex converts the grammar.Stem enum to the 0-3 index expected
// by RootEntry.Stem. The Haskell convention is S0→0, S1→1, S2→2, S3→3.
func stemIndex(s g.Stem) int {
	switch s {
	case g.S1:
		return 1
	case g.S2:
		return 2
	case g.S3:
		return 3
	default:
		return 0
	}
}

func slotIV(s g.SlotIV) string {
	if s == g.DefaultSlotIV {
		return ""
	}
	return fmt.Sprintf("%s/%s/%s", s.Function, s.Specification, s.Context)
}

func slotVI(s g.SlotVI) string {
	if s == g.DefaultSlotVI {
		return ""
	}
	parts := []string{}
	if s.Configuration != g.UNI {
		parts = append(parts, s.Configuration.String())
	}
	if s.Affiliation != g.CSL {
		parts = append(parts, s.Affiliation.String())
	}
	if s.Perspective != g.M_ {
		parts = append(parts, s.Perspective.String())
	}
	if s.Extension != g.DEL {
		parts = append(parts, s.Extension.String())
	}
	if s.Essence != g.NRM {
		parts = append(parts, s.Essence.String())
	}
	return strings.Join(parts, ".")
}

// affixes renders a list of affixes, hyphenated. With a lexicon each
// affix shows its ABBREV/degree; without one, the surface "Cs:Vx" form.
func (gl *Glosser) affixes(as []g.Affix) string {
	if len(as) == 0 {
		return ""
	}
	parts := make([]string, len(as))
	for i, a := range as {
		parts[i] = gl.affix(a)
	}
	return strings.Join(parts, "-")
}

func (gl *Glosser) affix(a g.Affix) string {
	_, deg := parse.ClassifyAffixVowel(a.Vowel)
	if gl.Lex != nil {
		if entry, ok := gl.Lex.Affixes[a.Consonant]; ok {
			return fmt.Sprintf("%s/%d", entry.Abbrev, deg)
		}
	}
	// Without a lexicon entry, show "Cs/degree" — same shape the
	// Haskell glosser uses at Short precision when no entry exists.
	return fmt.Sprintf("%s/%d", a.Consonant, deg)
}

func slotVIII(s g.SlotVIII) string {
	if s == nil {
		return ""
	}
	switch v := s.(type) {
	case g.VnCnValence:
		return joinDot(valenceLabel(v.Valence), moodOrScope(v.MS))
	case g.VnCnPhase:
		return joinDot(v.Phase.String(), moodOrScope(v.MS))
	case g.VnCnEffect:
		return joinDot(v.Effect.String(), moodOrScope(v.MS))
	case g.VnCnLevel:
		return joinDot(v.Level.String(), moodOrScope(v.MS))
	case g.VnCnAspect:
		return joinDot(v.Aspect.String(), moodOrScope(v.MS))
	}
	return ""
}

// valenceLabel suppresses MNO (the unmarked default valence) to match
// the Haskell convention.
func valenceLabel(v g.Valence) string {
	if v == g.MNO {
		return ""
	}
	return v.String()
}

// moodOrScope renders the MoodOrScope part of a SlotVIII. FAC mood
// (the default) is suppressed; CCN case-scope is also suppressed since
// it's the nominal counterpart of FAC.
func moodOrScope(ms g.MoodOrScope) string {
	switch v := ms.(type) {
	case g.MoodVal:
		if v.Mood == g.FAC {
			return ""
		}
		return v.Mood.String()
	case g.CaseScopeVal:
		if v.CaseScope == g.CCN {
			return ""
		}
		return v.CaseScope.String()
	}
	return ""
}

func joinDot(parts ...string) string {
	return strings.Join(nonEmpty(parts), ".")
}

func slotIX(s g.SlotIX) string {
	switch v := s.(type) {
	case g.CaseSlot:
		if v.Case == g.THM {
			return ""
		}
		return v.Case.String()
	case g.IllocValSlot:
		// Validation suppressed when OBS (the default for non-ASR
		// illocutions and the most common ASR pairing).
		if v.Validation == g.OBS {
			return v.Illocution.String()
		}
		return v.Illocution.String() + "/" + v.Validation.String()
	}
	return ""
}
