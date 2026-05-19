// Package render turns a grammar.Formative back into its surface text
// representation. Per-slot encoders mirror the parsers in package parse.
package render

import (
	"strings"

	"github.com/coudard/ithkuil/go/allomorph"
	g "github.com/coudard/ithkuil/go/grammar"
)

// Formative renders a complete formative as a single string, concatenating
// the ten slots in order.
func Formative(f g.Formative) string {
	var b strings.Builder
	b.WriteString(SlotI(f.SlotI))
	b.WriteString(g.SlotIIToVv(f.SlotII))
	b.WriteString(string(f.SlotIII))
	b.WriteString(g.SlotIVToVr(f.SlotIV))
	b.WriteString(SlotV(f.SlotV))
	b.WriteString(allomorph.ConstructCa(f.SlotVI))
	b.WriteString(SlotVII(f.SlotVII))
	b.WriteString(SlotVIII(f.SlotVIII))
	b.WriteString(SlotIX(f.SlotIX))
	return b.String()
}

// SlotI renders the optional concatenation status as the Cc consonant.
// Shortcut-only Cc forms ("w", "y") are not emitted here; they belong
// to the Cc-with-shortcut paths handled by a future builder.
func SlotI(c *g.ConcatenationStatus) string {
	if c == nil {
		return ""
	}
	switch *c {
	case g.Type1:
		return "h"
	case g.Type2:
		return "hw"
	}
	return ""
}

// SlotV renders stem affixes in their Cs+Vx (reversed) surface order.
func SlotV(affixes []g.Affix) string {
	var b strings.Builder
	for _, a := range affixes {
		b.WriteString(a.Consonant)
		b.WriteString(a.Vowel)
	}
	return b.String()
}

// SlotVII renders Ca-scoped affixes in their Vx+Cs surface order.
func SlotVII(affixes []g.Affix) string {
	var b strings.Builder
	for _, a := range affixes {
		b.WriteString(a.Vowel)
		b.WriteString(a.Consonant)
	}
	return b.String()
}

// SlotVIII renders the VnCn slot. Empty when SlotVIII is nil.
// Pattern-1 variants (Valence/Phase/Effect/Level) use Pattern-1 Cn;
// Pattern-2 (Aspect) uses Pattern-2 Cn.
func SlotVIII(s g.SlotVIII) string {
	if s == nil {
		return ""
	}
	switch v := s.(type) {
	case g.VnCnValence:
		return Valence(v.Valence) + MoodOrScopeP1(v.MS)
	case g.VnCnPhase:
		return Phase(v.Phase) + MoodOrScopeP1(v.MS)
	case g.VnCnEffect:
		return Effect(v.Effect) + MoodOrScopeP1(v.MS)
	case g.VnCnLevel:
		return Level(v.Level) + MoodOrScopeP1(v.MS)
	case g.VnCnAspect:
		return Aspect(v.Aspect) + MoodOrScopeP2(v.MS)
	}
	return ""
}

// SlotIX renders the final slot (Vc or Vk) based on the variant.
func SlotIX(s g.SlotIX) string {
	switch v := s.(type) {
	case g.CaseSlot:
		return g.CaseToVc(v.Case)
	case g.IllocValSlot:
		return Vk(v.Illocution, v.Validation)
	}
	return ""
}

// Vk renders an Illocution+Validation pair as the surface Vk vowel.
// ASR is encoded by the Validation vowel only (Series 1); other
// illocutions have a dedicated Series 2 form and ignore Validation.
func Vk(ill g.Illocution, val g.Validation) string {
	if ill == g.ASR {
		return Validation(val)
	}
	switch ill {
	case g.DIR:
		return "ai"
	case g.DEC:
		return "au"
	case g.IRG:
		return "ei"
	case g.VER:
		return "eu"
	case g.ADM:
		return "ou"
	case g.POT:
		return "oi"
	case g.HOR:
		return "iu"
	case g.CNJ:
		return "ui"
	}
	return ""
}

// Validation renders the Series-1 vowel that encodes a Validation
// alongside ASR illocution.
func Validation(v g.Validation) string {
	return [...]string{"a", "ä", "e", "i", "ëi", "ö", "o", "ü", "u"}[v]
}

// Valence renders the Series-1 Vn vowel.
func Valence(v g.Valence) string {
	return [...]string{"a", "ä", "e", "i", "ëi", "ö", "o", "ü", "u"}[v]
}

// Phase renders the Series-2 Vn vowel.
func Phase(p g.Phase) string {
	return [...]string{"ai", "au", "ei", "eu", "ëu", "ou", "oi", "iu", "ui"}[p]
}

// Effect renders the Series-3 Vn vowel (canonical, not the y-/w- alternates).
func Effect(e g.Effect) string {
	return [...]string{"ia", "ie", "io", "iö", "eë", "uö", "uo", "ue", "ua"}[e]
}

// Level renders the Series-4 Vn vowel.
func Level(l g.Level) string {
	return [...]string{"ao", "aö", "eo", "eö", "oë", "öe", "oe", "öa", "oa"}[l]
}

// Aspect renders the Vn vowel for any of the 36 aspects (one canonical
// form per aspect — series-3 alternates are not regenerated).
var aspectForms = [...]string{
	// Column 1 (Series 1)
	"a", "ä", "e", "i", "ëi", "ö", "o", "ü", "u",
	// Column 2 (Series 2)
	"ai", "au", "ei", "eu", "ëu", "ou", "oi", "iu", "ui",
	// Column 3 (Series 3)
	"ia", "ie", "io", "iö", "eë", "uö", "uo", "ue", "ua",
	// Column 4 (Series 4)
	"ao", "aö", "eo", "eö", "oë", "öe", "oe", "öa", "oa",
}

func Aspect(a g.Aspect) string {
	return aspectForms[a]
}

// MoodOrScopeP1 renders a MoodOrScope as a Pattern-1 Cn consonant.
// Pattern 1 collapses the parallel Mood/CaseScope values to the same
// consonant ("h"/"hl"/"hr"/"hm"/"hn"/"hň").
func MoodOrScopeP1(ms g.MoodOrScope) string {
	switch v := ms.(type) {
	case g.MoodVal:
		return moodCnP1[v.Mood]
	case g.CaseScopeVal:
		return moodCnP1[g.CaseScopeToMood(v.CaseScope)]
	}
	return ""
}

// MoodOrScopeP2 renders a MoodOrScope as a Pattern-2 Cn consonant
// (used with Aspect Vn). "w" is used for FAC mood / CCN case-scope;
// other values get an "h…w" wrapping.
func MoodOrScopeP2(ms g.MoodOrScope) string {
	switch v := ms.(type) {
	case g.MoodVal:
		return moodCnP2[v.Mood]
	case g.CaseScopeVal:
		return moodCnP2[g.CaseScopeToMood(v.CaseScope)]
	}
	return ""
}

var moodCnP1 = [...]string{"h", "hl", "hr", "hm", "hn", "hň"}
var moodCnP2 = [...]string{"w", "hw", "hrw", "hmw", "hnw", "hňw"}
