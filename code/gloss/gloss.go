// Package gloss produces a human-readable morphological gloss of a
// grammar.Formative. The output format is hyphen-separated slot
// abbreviations with grammatical defaults suppressed, e.g.
//
//	S2.PRC-ml-DYN.OBJ-MSS.G-ERG
//
// Each punctuation mark has exactly one job, so a token's kind can be
// told from its shape without consulting the lexicon:
//
//	"-"  separates slots
//	"."  joins category values within one slot
//	"/"  binds an argument, a degree or a case, to a head
//	"_"  trails a modifier, currently the affix Type
//	":"  tags a structured body ("Ca:MSS.G", "NOM:1m")
//	"()" wraps a head built from referents or a Cs
//	"+"  joins referents
//	"{}" marks something structural rather than a morpheme
//
// When a lexicon is supplied (via Glosser.Lex), roots show their
// stem-selected meaning and affixes show their ABBREV/degree form;
// otherwise both fall back to raw consonant clusters.
package gloss

import (
	"fmt"
	"strings"

	"github.com/christian-oudard/ithkuil/allomorph"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/numbers"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/semantics"
	"github.com/christian-oudard/ithkuil/surface"
)

// Glosser carries the optional lexicon used for enriching root and
// affix glosses. A zero-value Glosser produces the same output as the
// package-level Formative function.
type Glosser struct {
	Lex *lexicon.Lexicon
	// Canonical suppresses display-only annotations that aren't part of
	// the compose authoring syntax: the quoted lexicon meaning after the
	// root ("-ml- 'gold (color)'"), and the category code that replaces
	// the degree on affixes like MCS ("MCS:SUB" for "MCS/1"). Both are
	// derivable from what remains, so dropping them loses nothing and
	// keeps one spelling per Formative. The result is exactly what
	// compose.Formative accepts, so set Canonical=true for round-tripping.
	Canonical bool
}

// Formative renders a one-line gloss of f. Components at their default
// values are suppressed. Stress is shown explicitly as a trailing tag,
// ULT for an unframed verbal and ANT for a framed one; penultimate
// stress is the unmarked default and stays off the gloss. Cs-root
// formatives render their root as "(Cs)/degree" to distinguish them
// from regular roots.
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
		gl.rootBody(f),
		gl.rootSuffix(f.Root),
		gl.affixes(f.SlotV),
		slotVI(f.SlotVI, len(f.SlotV) > 0),
		gl.affixes(f.SlotVII),
		slotVIII(f.SlotVIII, isVerbalFinal(f.Final)),
		finalSlotIX(f.Final),
		finalTag(f.Final),
	}
	return strings.Join(nonEmpty(parts), "-")
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
		return fmt.Sprintf("%s.%s", x.Stem, x.Version)
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
// for CrRoot, "(Cs)/degree" for CsRoot, "(refs)" for RefRoot. The
// RefRoot label decomposes its C1 into the underlying personal-
// reference chain (§4.6.4); when decomposition fails we fall back to
// the raw cluster so the gloss still names what it parsed.
//
// The full Formative is taken so number-root CrRoots can also read
// Slot VII to fold the TNX tens-affix into the gloss value.
func (gl *Glosser) rootBody(f g.Formative) string {
	switch x := f.Root.(type) {
	case g.CrRoot:
		return gl.crRootLabel(x, f)
	case g.CsRoot:
		return gl.csRootLabel(x)
	case g.RefRoot:
		open, close := "-(", ")-"
		if gl.Canonical {
			open, close = "(", ")"
		}
		if refs, ok := parse.DecomposeRefCluster(x.C1); ok && len(refs) > 0 {
			parts := make([]string, len(refs))
			for i, pr := range refs {
				s := pr.Referent.String()
				if pr.Effect != g.NEU {
					s += "/" + pr.Effect.String()
				}
				parts[i] = s
			}
			return open + strings.Join(parts, "+") + close
		}
		return open + x.C1 + close
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
		return strings.Join(parts, ".")
	}
	return ""
}

func (gl *Glosser) crRootLabel(x g.CrRoot, f g.Formative) string {
	if x.Cluster == "" {
		return ""
	}
	cluster := x.Cluster
	if gl.Canonical {
		// Canonical mode emits the bare cluster: the parser identifies
		// it by shape (no slashes, no parens) and the surrounding "-"
		// from the slot join is enough. Display mode wraps with "-X-"
		// to make the root visually prominent in human-read output.
		return surface.ToASCII(cluster)
	}
	// Number roots are interpreted from the centesimal system table
	// (§8.0), not from the lexicon. Decode reads Slot VII as well, so
	// 11-99 fold the TNX tens-affix into the displayed value.
	if numbers.IsNumberRoot(x.Cluster) {
		if n, ok := numbers.Decode(f); ok {
			// SPT affix in Slot VII upgrades the bare integer gloss to a
			// date/time label per §6: "8th hour", "15th of month", etc.
			if d, hasSPT := numbers.SPTDegree(f); hasSPT {
				if lbl := numbers.SPTDegreeLabel(d); lbl != "" {
					return fmt.Sprintf("-%s-'%dth %s'", cluster, n.Value, lbl)
				}
			}
			return fmt.Sprintf("-%s-'%d'", cluster, n.Value)
		}
	}
	if gl.Lex != nil {
		if entry, ok := gl.Lex.Roots[x.Cluster]; ok {
			if meaning := rootMeaning(entry, x); meaning != "" {
				return "-" + cluster + "-'" + meaning + "'"
			}
		}
	}
	return "-" + cluster + "-"
}

// rootMeaning picks the best gloss string for a CrRoot, consulting
// the entry's specialization variants in order of specificity:
//
//	spec=OBJ → Objective[stem-1]    (per-stem alternate)
//	spec=CTE → Contential           (stem-independent)
//	spec=CSV → Constitutive         (stem-independent)
//	fn=DYN   → Dynamic              (stem-independent)
//	ver=CPT  → Completive[stem-1]   (per-stem alternate)
//	default  → Stem(stem)
//
// Spec variants override completion-based ones because the spec
// reshapes what the root denotes; completion just marks completeness.
// Each branch falls through to the next when the relevant variant is
// empty, so unannotated roots still get a sensible default.
func rootMeaning(entry lexicon.RootEntry, x g.CrRoot) string {
	stem := stemIndex(x.Stem)
	switch x.SlotIV.Specification {
	case g.OBJ:
		if v := stemTriple(entry.Objective, stem); v != "" {
			return v
		}
	case g.CTE:
		if entry.Contential != "" {
			return entry.Contential
		}
	case g.CSV:
		if entry.Constitutive != "" {
			return entry.Constitutive
		}
	}
	if x.SlotIV.Function == g.DYN && entry.Dynamic != "" {
		return entry.Dynamic
	}
	if x.Version == g.CPT {
		if v := stemTriple(entry.Completive, stem); v != "" {
			return v
		}
	}
	return entry.Stem(stem)
}

func stemTriple(ss []string, stem int) string {
	if stem < 1 || stem > 3 || len(ss) < stem {
		return ""
	}
	return ss[stem-1]
}

func (gl *Glosser) csRootLabel(x g.CsRoot) string {
	return fmt.Sprintf("(%s)/%d", gl.affixLabel(x.Cs), x.Degree)
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

func slotI(c g.ConcatenationStatus) string {
	switch c {
	case g.Type1:
		return "T1"
	case g.Type2:
		return "T2"
	}
	return ""
}

// stemIndex converts the grammar.Stem enum to the 0-3 index expected
// by RootEntry.Stem: S0→0, S1→1, S2→2, S3→3.
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
	return fmt.Sprintf("%s.%s.%s", s.Function, s.Specification, s.Context)
}

// caStackPrefix tags a §3.5/§3.7 Ca-stacking affix in the gloss. The
// body after it is the same component list slotVI writes, so a stacked
// Ca and the Slot VI Ca read alike and only the tag distinguishes
// them. ":" does one job in the canonical gloss, introducing a tagged
// body, and this is its only use inside a formative; the referent
// category tag ("NOM:1m") is the same sense at word level.
const caStackPrefix = "Ca:"

// stackedCaBody renders a stacked Ca cluster as its components. An
// all-default stack writes "{Ca}", the same marker Slot VI uses,
// because the notation already says exactly that and the two compose
// unambiguously — the tag is Ca, the body is {Ca}.
//
// Whether an all-default stacked Ca means anything is a question about
// the language that §3.7 does not answer. It is spelled rather than
// elided so that the gloss stays lossless either way; if it turns out
// to be vacuous, canonicalization can drop it later at no cost.
func stackedCaBody(cluster string) string {
	s, ok := allomorph.ParseCa(cluster)
	if !ok {
		return cluster
	}
	return slotVI(s, true)
}

// slotVI renders the Ca complex, suppressing components at their
// default value. slotVFilled forces a "{Ca}" placeholder for an
// all-default Ca: Slot V affixes apply to the stem without scope over
// Ca, Slot VII affixes have scope over it, and position in the gloss
// is what tells them apart — so the Ca must stay visible as the
// boundary between them. The surface does the same thing, geminating
// the Ca whenever Slot V is filled.
func slotVI(s g.SlotVI, slotVFilled bool) string {
	if s == g.DefaultSlotVI {
		if slotVFilled {
			return "{Ca}"
		}
		return ""
	}
	parts := []string{}
	if s.Configuration != g.UPX {
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
//
// §4.6.5 special case: a lone Type-3 affix whose Cs is a referential
// consonant reads as a personal-reference shortcut rather than a
// regular affix. We render that as "(refs/degree)" to make the
// referential reading visible.
// affixes glosses one affix slot. Called once for Slot V and once for
// Slot VII, which is what settles §4.6.5's ambiguity: its trigger is "a
// lone Type-3 V_X C_S affix without any adjacent Type-1 or Type-2 affix
// for it to apply to", and §3.5 says a Type-3 affix applies "to
// previous C_S V_X / V_X C_S affix only (or the following affix if it
// is the first in the slot)". If Slot V holds affixes and Slot VII
// holds a single Type-3, that affix does have something to apply to by
// §3.5, and by the letter of §4.6.5's trigger the referential reading
// is blocked — but nothing says whether a slot boundary interrupts
// adjacency.
//
// We take it that it does: "lone" is read per slot. §3.5's own
// parenthetical scopes adjacency to the slot ("if it is the first in
// the slot"), and the alternative would make the same Slot VII
// spelling mean different things depending on Slot V, which no other
// rule in §3.5 does.
//
// The stakes are that all 33 referential consonant forms are also C_S
// forms of ordinary affixes — not most, all: l is CTR, r is NEG, s is
// CMF, n is TPF, t is DCD, ň is COO, č is SWR, and so on through the
// list. Most of that overlap costs nothing and appears to be the
// design, since a Type-3 affix with nothing adjacent is already
// meaningless and its slot is free to be repurposed.
func (gl *Glosser) affixes(as []g.Affix) string {
	if len(as) == 0 {
		return ""
	}
	if len(as) == 1 && as[0].Type == g.Type3Affix {
		if refs := refClusterLabel(as[0].Consonant); refs != "" {
			return fmt.Sprintf("(%s)/%d", refs, as[0].Degree)
		}
	}
	parts := make([]string, len(as))
	for i, a := range as {
		parts[i] = gl.affix(a)
	}
	return strings.Join(parts, "-")
}

// refClusterLabel decomposes a referential consonant cluster into its
// "1m+2p/BEN" form, or "" when the cluster is not one. Shared by the
// two shortcuts that put a referential in an affix slot: the §4.6.5
// Type-3 degree form and the §4.6.5 Column-4 case form.
func refClusterLabel(cluster string) string {
	refs, ok := parse.DecomposeRefCluster(cluster)
	if !ok || len(refs) == 0 {
		return ""
	}
	parts := make([]string, len(refs))
	for i, pr := range refs {
		s := pr.Referent.String()
		if pr.Effect != g.NEU {
			s += "/" + pr.Effect.String()
		}
		parts[i] = s
	}
	return strings.Join(parts, "+")
}

func (gl *Glosser) affix(a g.Affix) string {
	if a.IsCaStack() {
		return caStackPrefix + stackedCaBody(a.Consonant)
	}
	// §3.9.2 case-accessor, inverse case-accessor or case-stacking
	// affix. The Cs increment names the family and which half of the 68
	// cases it reaches; the Vx series and degree name the case within
	// that half. Written "ACC/INS", "IAC/PRP_3", "CST/ERG" — the same
	// head-slash-argument shape the Column-4 shortcut uses, since both
	// bind a case to a head. The Type suffix is the affix Type suffix,
	// because §3.9.2's three accessor Types are affix Types.
	//
	// The Vx series is not written: it is derived from the case, so
	// spelling it would be redundant, and the stored Affix.Type holds it
	// rather than the accessor Type for exactly that reason.
	if kind, high, ok := g.ParseAccessorCs(a.Consonant); ok {
		if series, sok := g.VxSeries(a.Type); sok {
			if c, cok := g.AccessorCase(series, a.Degree, high); cok {
				return fmt.Sprintf("%s/%s%s",
					kind.Family(), c, gl.affixTypeSuffix(kind.Type()))
			}
		}
	}
	// §4.6.5 Column-4 shortcut: a referential in a Transrelative case.
	// Written "(refs)/CASE" against the Type-3 form's "(refs)/degree";
	// a case is three uppercase letters and a degree is one digit, so
	// the two never collide.
	if a.Type == g.Column4Affix {
		c, ok := g.TransrelativeCase(a.Degree)
		if !ok {
			return fmt.Sprintf("(%s)/?%d", a.Consonant, a.Degree)
		}
		refs := refClusterLabel(a.Consonant)
		if refs == "" {
			refs = gl.affixLabel(a.Consonant)
		}
		return fmt.Sprintf("(%s)/%s", refs, c)
	}
	if gl.Lex != nil {
		if entry, ok := gl.Lex.Affixes[a.Consonant]; ok {
			// Category-valued affixes (MCS, PHS, AP1-4, IVL, LVL, VAL)
			// carry a category code in place of a degree meaning, and
			// display mode shows that code: "MCS:SUB" rather than
			// "MCS/1".
			//
			// Canonical mode keeps the degree. The code is derived from
			// (Cs, degree, type), so writing it is redundant, and a
			// canonical form that admits two spellings of one Formative
			// is not canonical. It also keeps ":" doing the single job
			// it has here, tagging a "Ca:" body; "ABBREV:CAT" would be a
			// second sense of the same mark, and the last time two
			// constructs shared a shape the round trip broke on it.
			if cat := entry.CategoryValue(a.Degree, int(a.Type)+1); cat != "" && !gl.Canonical {
				return fmt.Sprintf("%s:%s", entry.Abbrev, cat)
			}
			return fmt.Sprintf("%s/%d%s", entry.Abbrev, a.Degree, gl.affixTypeSuffix(a.Type))
		}
	}
	return fmt.Sprintf("%s/%d%s", gl.affixLabel(a.Consonant), a.Degree, gl.affixTypeSuffix(a.Type))
}

func slotVIII(s g.SlotVIII, isVerbal bool) string {
	if s == nil {
		return ""
	}
	vn := g.SlotVIIIVnLabel(s)
	// MNO is the unmarked default valence and is suppressed in gloss output.
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
// Vk renders as "ASR" (or "ASR.<val>" for non-OBS Validations);
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
		return "ASR." + as.Validation.String()
	}
	return v.Tag()
}
