// Package inspect extracts compact, per-slot string representations of
// parsed word tokens. Used by the ithkuil CLI's --short view and by the
// MCP server's per-token type tag.
package inspect

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/semantics"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// Dot is the placeholder rendered when a slot is at its grammatical
// default — a single visible character so per-slot strings line up if
// callers ever choose to align them.
const Dot = "·"

// Type returns a short tag identifying the token kind: Form, Concat,
// Ref, CombRef, Bias, Reg, Mod, Affix, Affixes, Carrier, (fgn), or "?"
// for unknown.
func Type(t tokenize.WordToken) string {
	switch t.(type) {
	case tokenize.FormativeWord:
		return "Form"
	case tokenize.ConcatenatedFormativeWord:
		return "Concat"
	case tokenize.ReferentialWord:
		return "Ref"
	case tokenize.CombinationRefWord:
		return "CombRef"
	case tokenize.BiasWord:
		return "Bias"
	case tokenize.RegisterStartWord, tokenize.RegisterEndWord:
		return "Reg"
	case tokenize.ModularWord:
		return "Mod"
	case tokenize.SingleAffixWord:
		return "Affix"
	case tokenize.MultipleAffixWord:
		return "Affixes"
	case tokenize.CarrierWord:
		return "Carrier"
	case tokenize.ForeignWord:
		return "(fgn)"
	case tokenize.UnknownWord:
		return "?"
	}
	return "?"
}

// SlotI returns "T1"/"T2" for concatenated formatives, Dot otherwise.
func SlotI(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok || f.Formative.Concat == nil {
		return Dot
	}
	switch *f.Formative.Concat {
	case g.Type1:
		return "T1"
	case g.Type2:
		return "T2"
	}
	return Dot
}

// SlotII returns "Stem/Version" for formatives, Dot otherwise. Stem
// is implicit (S1) for CsRoot and RefRoot; we still show it.
func SlotII(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return Dot
	}
	switch r := f.Formative.Root.(type) {
	case g.CrRoot:
		return fmt.Sprintf("%s/%s", r.Stem, r.Version)
	case g.CsRoot:
		return fmt.Sprintf("S1/%s", r.Version)
	case g.RefRoot:
		return fmt.Sprintf("S1/%s", r.Version)
	}
	return Dot
}

// SlotIII returns the root identifier (Cr cluster, Cs identifier, or
// referential C1) for formatives, or the joined referent list for
// referentials.
func SlotIII(t tokenize.WordToken) string {
	switch v := t.(type) {
	case tokenize.FormativeWord:
		switch r := v.Formative.Root.(type) {
		case g.CrRoot:
			if r.Cluster == "" {
				return Dot
			}
			return r.Cluster
		case g.CsRoot:
			return r.Cs
		case g.RefRoot:
			return r.C1
		}
	case tokenize.ReferentialWord:
		parts := make([]string, len(v.Refs))
		for i, r := range v.Refs {
			parts[i] = r.Referent.String()
		}
		return strings.Join(parts, "+")
	}
	return Dot
}

// SlotIV returns the non-default Function/Specification/Context
// values joined by "/", or Dot if everything is at the default.
func SlotIV(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return Dot
	}
	var fn g.Function
	var sp g.Specification
	var ctx g.Context
	switch r := f.Formative.Root.(type) {
	case g.CrRoot:
		fn, sp, ctx = r.SlotIV.Function, r.SlotIV.Specification, r.SlotIV.Context
	case g.CsRoot:
		fn, sp, ctx = r.Function, g.BSC, r.Context
	case g.RefRoot:
		fn, sp, ctx = r.SlotIV.Function, r.SlotIV.Specification, r.SlotIV.Context
	default:
		return Dot
	}
	var parts []string
	if fn != g.STA {
		parts = append(parts, fn.String())
	}
	if sp != g.BSC {
		parts = append(parts, sp.String())
	}
	if ctx != g.EXS {
		parts = append(parts, ctx.String())
	}
	if len(parts) == 0 {
		return Dot
	}
	return strings.Join(parts, "/")
}

// SlotV returns "N+M" — counts of Slot V and Slot VII affixes — or
// Dot if there are no affixes.
func SlotV(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return Dot
	}
	total := len(f.Formative.SlotV) + len(f.Formative.SlotVII)
	if total == 0 {
		return Dot
	}
	return fmt.Sprintf("%d+%d", len(f.Formative.SlotV), len(f.Formative.SlotVII))
}

// SlotVI returns the non-default Ca features joined by "/", or Dot.
func SlotVI(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return Dot
	}
	s := f.Formative.SlotVI
	if s == g.DefaultSlotVI {
		return Dot
	}
	var parts []string
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
	return strings.Join(parts, "/")
}

// SlotVIII returns the VnCn content rendered as "Vn.MoodScope", or
// Dot when the slot is empty. The MoodScope label is the verbal
// Mood (FAC/SUB/…) when the formative is verbal, the nominal
// CaseScope (CCN/CCA/…) otherwise.
func SlotVIII(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok || f.Formative.SlotVIII == nil {
		return Dot
	}
	label := g.SlotVIIIVnLabel(f.Formative.SlotVIII)
	if label == "" {
		return Dot
	}
	return label + "." + semantics.SlotVIIICnLabel(f.Formative.SlotVIII, f.Formative.Final)
}

// SlotIX returns the case (Vc) or illocution/validation (Vk) encoded
// by the Final, with THM and bare ASR/OBS suppressed as defaults.
func SlotIX(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return Dot
	}
	switch v := f.Formative.Final.(type) {
	case g.UnframedNominal:
		if v.Case == g.THM {
			return Dot
		}
		return v.Case.String()
	case g.FramedVerbal:
		if v.Case == g.THM {
			return Dot
		}
		return v.Case.String()
	case g.UnframedVerbal:
		return vkLabel(v.Vk)
	}
	return Dot
}

// vkLabel formats a Vk variant as "ILL" or "ILL/VAL" — non-default
// Validation is shown only on Assertive (the only Vk that carries
// one).
func vkLabel(vk g.Vk) string {
	if asr, ok := vk.(g.Assertive); ok {
		if asr.Validation == g.OBS {
			return "ASR"
		}
		return "ASR/" + asr.Validation.String()
	}
	return vk.Tag()
}

// Stress returns the Final tag (ANT for framed-verbal, ULT for
// verbal, Dot for the nominal/penultimate default).
func Stress(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return Dot
	}
	if tag := f.Formative.Final.Tag(); tag != "" {
		return tag
	}
	return Dot
}
