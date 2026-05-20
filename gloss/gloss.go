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

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/semantics"
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
	if f.Root == nil {
		panic("gloss: Formative.Root is nil")
	}
	if f.Final == nil {
		panic("gloss: Formative.Final is nil")
	}
	parts := []string{
		slotI(f.Concat),
		gl.rootPrefix(f.Root),
		gl.rootBody(f.Root),
		gl.rootSuffix(f.Root),
		gl.affixes(f.SlotV),
		slotVI(f.SlotVI),
		gl.affixes(f.SlotVII),
		slotVIII(f.SlotVIII, isVerbalFinal(f.Final)),
		finalSlotIX(f.Final),
		finalTag(f.Final),
	}
	body := strings.Join(nonEmpty(parts), "-")
	if f.SentenceStarter {
		return "§ " + body
	}
	return body
}

// rootPrefix returns the slot-II / version gloss for the root variant.
// CrRoot shows non-default (Stem, Version); CsRoot and RefRoot show
// version only since their Stem is implicit S1.
func (gl *Glosser) rootPrefix(r g.Root) string {
	switch x := r.(type) {
	case g.CrRoot:
		if (g.SlotII{Stem: x.Stem, Version: x.Version}) == g.DefaultSlotII {
			return ""
		}
		return fmt.Sprintf("%s/%s", x.Stem, x.Version)
	case g.CsRoot:
		if x.Version == g.PRC {
			return ""
		}
		return x.Version.String()
	case g.RefRoot:
		if x.Version == g.PRC {
			return ""
		}
		return x.Version.String()
	}
	return ""
}

// rootBody returns the root identifier itself — the lexical cluster
// for CrRoot, "(Cs)/degree" for CsRoot, "(C1)" for RefRoot.
func (gl *Glosser) rootBody(r g.Root) string {
	switch x := r.(type) {
	case g.CrRoot:
		return gl.crRootLabel(x)
	case g.CsRoot:
		return gl.csRootLabel(x)
	case g.RefRoot:
		return "-(" + x.C1 + ")-"
	}
	return ""
}

// rootSuffix returns the slot-IV gloss for the root variant. CrRoot
// and RefRoot suppress the default SlotIV; CsRoot suppresses default
// Context (Function is shown only when DYN since STA is the default).
func (gl *Glosser) rootSuffix(r g.Root) string {
	switch x := r.(type) {
	case g.CrRoot:
		return slotIV(x.SlotIV)
	case g.RefRoot:
		return slotIV(x.SlotIV)
	case g.CsRoot:
		parts := []string{}
		if x.Function != g.STA {
			parts = append(parts, x.Function.String())
		}
		if x.Context != g.EXS {
			parts = append(parts, x.Context.String())
		}
		return strings.Join(parts, "/")
	}
	return ""
}

func (gl *Glosser) crRootLabel(x g.CrRoot) string {
	if x.Cluster == "" {
		return ""
	}
	if gl.Lex != nil {
		if entry, ok := gl.Lex.Roots[x.Cluster]; ok {
			meaning := entry.Stem(stemIndex(x.Stem))
			if meaning != "" {
				return "-" + x.Cluster + "- '" + meaning + "'"
			}
		}
	}
	return "-" + x.Cluster + "-"
}

func (gl *Glosser) csRootLabel(x g.CsRoot) string {
	abbr := x.Cs
	if gl.Lex != nil {
		if entry, ok := gl.Lex.Affixes[x.Cs]; ok {
			abbr = entry.Abbrev
		}
	}
	return fmt.Sprintf("(%s)/%d", abbr, x.Degree)
}

// isVerbalFinal reports whether the formative's Slot VIII C_N should be
// glossed as Mood. Per spec §3.8.1, only UNFRAMED verbal formatives
// (ultimate stress) take Mood; nominal and FRAMED-verbal formatives
// take Case-Scope.
func isVerbalFinal(f g.Final) bool { return g.IsVerbal(f) }

// finalTag emits the grammatical-category tag for non-default Final
// variants. UnframedNominal (penultimate) is the unmarked default and
// stays off the gloss. UnframedVerbal → "ULT"; FramedVerbal → "ANT".
func finalTag(f g.Final) string {
	if f == nil {
		return ""
	}
	return f.Tag()
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
	if gl.Lex != nil {
		if entry, ok := gl.Lex.Affixes[a.Consonant]; ok {
			return fmt.Sprintf("%s/%d", entry.Abbrev, a.Degree)
		}
	}
	// Without a lexicon entry, show "Cs/degree" — same shape the
	// Haskell glosser uses at Short precision when no entry exists.
	return fmt.Sprintf("%s/%d", a.Consonant, a.Degree)
}

func slotVIII(s g.SlotVIII, isVerbal bool) string {
	if s == nil {
		return ""
	}
	vn := g.SlotVIIIVnLabel(s)
	// MNO is the unmarked default valence and is suppressed in gloss
	// output, matching the Haskell convention.
	if v, ok := s.(g.VnCnValence); ok && v.Valence == g.MNO {
		vn = ""
	}
	mood := g.SlotVIIIMoodScope(s)
	scope := ""
	if mood != g.FAC {
		scope = semantics.MoodOrCaseScope(mood, isVerbal)
	}
	return joinDot(vn, scope)
}

func joinDot(parts ...string) string {
	return strings.Join(nonEmpty(parts), ".")
}

// finalSlotIX glosses the Slot IX content of the Formative's Final.
// THM Case is the unmarked default and is suppressed. Assertive
// Vk renders as "ASR" (or "ASR/<val>" for non-OBS Validations);
// the other Vk variants render as their illocution tag.
func finalSlotIX(f g.Final) string {
	if f == nil {
		return ""
	}
	switch v := f.(type) {
	case g.UnframedNominal:
		if v.Case == g.THM {
			return ""
		}
		return v.Case.String()
	case g.FramedVerbal:
		if v.Case == g.THM {
			return ""
		}
		return v.Case.String()
	case g.UnframedVerbal:
		return vkTag(v.Vk)
	}
	return ""
}

// vkTag returns the gloss label for a Vk variant. Assertive shows ASR
// plus Validation when non-default; the other eight illocutions just
// use their Tag().
func vkTag(v g.Vk) string {
	if as, ok := v.(g.Assertive); ok {
		if as.Validation == g.OBS {
			return "ASR"
		}
		return "ASR/" + as.Validation.String()
	}
	return v.Tag()
}
