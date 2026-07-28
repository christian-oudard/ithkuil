// Package semantics is Layer E of the parse pipeline: it turns
// Layer-D grammar values into context-dependent labels.
//
// The decisions here cannot be made by looking at a single slot in
// isolation — they depend on either the formative's grammatical
// category (verbal vs nominal) or the neighboring tokens in the
// sentence (a modular adjunct's Cn pattern is driven by the adjacent
// formative's verbality).
//
// What lives here:
//
//   - MoodOrCaseScope:   verbal Mood vs nominal CaseScope twin label
//   - SlotVIIICnLabel:   formative Slot VIII Cn applied with §3.8.1
//   - IsVH / IsVN:       modular adjunct slot 4 reading (§4.3)
//   - ModularIsVerbal:   modular Cn pattern driven by MarksMood
//   - VnCategory:        which Vn series a modular Vn belongs to
//   - CnLabel:           a modular Cn rendered as Mood or CaseScope
//   - VhCode/VhMeaning:  V_H scope-vowel encoding and prose
//   - PrefixCode/PrefixMeaning: w/y scope-prefix encoding and prose
package semantics

import (
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
)

// MoodOrCaseScope returns the verbal Mood label (FAC/SUB/…) when
// isVerbal is true, otherwise the nominal CaseScope twin (CCN/CCA/…).
// Per §3.8.1 only UNFRAMED verbal formatives (ultimate stress) take
// the Mood label; nominal and FRAMED-verbal formatives take CaseScope.
func MoodOrCaseScope(mood g.Mood, isVerbal bool) string {
	if isVerbal {
		return mood.String()
	}
	return g.MoodToCaseScope(mood).String()
}

// SlotVIIICnLabel applies MoodOrCaseScope to the MoodScope field of a
// formative's SlotVIII, picking the verbal/nominal labelling from the
// formative's Final. Returns "" when SlotVIII is absent.
func SlotVIIICnLabel(s g.SlotVIII, fin g.Final) string {
	if s == nil {
		return ""
	}
	return MoodOrCaseScope(g.SlotVIIIMoodScope(s), g.IsVerbal(fin))
}

// IsVH reports whether slot 4 of a modular adjunct is V_H (a scope
// marker) rather than V_N (another aspect/valence/etc. position). Per
// §4.3, V_H requires ultimate stress and at least one (Vn, Cn) pair
// to scope over.
func IsVH(stress phonology.Stress, pairCount int) bool {
	return stress == phonology.Ultimate && pairCount > 0
}

// ModularIsVerbal picks the verbal/nominal labelling for a modular
// adjunct's Cn. When marksMood is set by the tokenizer (it found a
// neighboring formative), it wins. Otherwise we fall back to the Vn
// pattern: Pattern-1 Vn (Valence/Phase/Effect/Level) → Mood,
// Pattern-2 Vn (Aspect) → CaseScope. The fallback matches the modular
// Cn pattern's own series.
func ModularIsVerbal(s g.SlotVIII, marksMood *bool) bool {
	if marksMood != nil {
		return *marksMood
	}
	_, isAspect := s.(g.VnCnAspect)
	return !isAspect
}

// IsAspectCn reports whether a Cn cluster is from the Pattern-2 set
// (used with Aspect Vn).
func IsAspectCn(cn string) bool {
	switch cn {
	case "w", "y", "hw", "hrw", "hmw", "hnw", "hňw":
		return true
	}
	return false
}

// VnCategory identifies the grammatical category a modular Vn encodes,
// returning its abbreviation (PRG/PRL/PCT/BEN1/MIN/…). Spec rules:
//
//   - Cn empty or Pattern-2 or Cm "n" → Vn is an Aspect.
//   - Otherwise Vn is one of Valence/Phase/Effect/Level (determined
//     by the vowel series).
func VnCategory(vn, cn string) string {
	if cn == "" || IsAspectCn(cn) || cn == "n" {
		if a, ok := parse.ParseVnAspect(vn); ok {
			return a.String()
		}
		return "Aspect?"
	}
	if v, ok := parse.ParseVnValence(vn); ok {
		return v.String()
	}
	if p, ok := parse.ParseVnPhase(vn); ok {
		return p.String()
	}
	if e, ok := parse.ParseVnEffect(vn); ok {
		return e.String()
	}
	if l, ok := parse.ParseVnLevel(vn); ok {
		return l.String()
	}
	return "Vn?"
}

// CnLabel renders a modular Cn cluster as either a Mood label or a
// CaseScope label depending on asMood. The "n"/"ň" Cm markers used in
// 3-slot modular adjuncts have no Mood/CaseScope reading; they return
// the marker tags "CmAspect"/"CmOther" instead.
func CnLabel(cn string, asMood bool) string {
	if asMood {
		if IsAspectCn(cn) {
			if m, ok := parse.ParseCnMoodP2(cn); ok {
				return m.String()
			}
		}
		if m, ok := parse.ParseCnMood(cn); ok {
			return m.String()
		}
	} else if cs, ok := parse.ParseCnCaseScope(cn); ok {
		return cs.String()
	}
	switch cn {
	case "n":
		return "CmAspect"
	case "ň":
		return "CmOther"
	}
	return "Cn?"
}

// VhCode returns a short tag for a V_H scope vowel indicating the
// scope's reach. The vowel may carry an acute (ultimate stress mark);
// it's normalized before the lookup.
func VhCode(v string) string {
	switch parse.NormalizeAccents(v) {
	case "a":
		return "→Case/Mood/Val/Illoc"
	case "e":
		return "→Case/Mood"
	case "i", "u":
		return "→formative"
	case "o":
		return "→formative+adjuncts"
	}
	return "→" + v
}

// VhMeaning returns the prose meaning of a V_H scope vowel.
func VhMeaning(v string) string {
	switch parse.NormalizeAccents(v) {
	case "a":
		return "scope over Case/Mood + Validation+Illocution"
	case "e":
		return "scope over Case/Mood"
	case "i", "u":
		return "scope over formative only"
	case "o":
		return "scope over formative + adjacent affixual adjuncts"
	}
	return "V_H " + v
}

// PrefixCode returns a short tag for a w/y modular scope prefix.
func PrefixCode(p string) string {
	switch p {
	case "w":
		return "→parent"
	case "y":
		return "→concat"
	}
	return p
}

// PrefixMeaning returns the prose meaning of a w/y modular scope prefix.
func PrefixMeaning(p string) string {
	switch p {
	case "w":
		return "applies to parent formative only"
	case "y":
		return "applies to concatenated formative only"
	}
	return ""
}

// CmName returns a label name for the "n"/"ň" Cm markers (3-slot
// modular adjuncts).
func CmName(code string) string {
	switch code {
	case "CmAspect":
		return "Cm (n)"
	case "CmOther":
		return "Cm (ň)"
	}
	return ""
}

// CmMeaning returns the prose meaning of a Cm marker code.
func CmMeaning(code string) string {
	switch code {
	case "CmAspect":
		return "marks the previous Vn as an aspect"
	case "CmOther":
		return "marks the previous Vn as valence/phase/effect/level"
	}
	return ""
}
