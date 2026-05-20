// Package render turns a grammar.Formative back into its surface text
// representation. Per-slot encoders mirror the parsers in package parse.
package render

import (
	"strings"
	"unicode/utf8"

	"github.com/coudard/ithkuil/go/allomorph"
	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/parse"
)

// Formative renders a complete formative as a single string. Stress
// is applied as a post-pass driven by f.Stress (independent of Slot IX
// shape). Two default elisions are applied greedily, preferring both
// over either: the leading default Slot II Vv "a" per §3.2 and the
// trailing THM Vc "a" per §3.1.3. Both are skipped when they would
// shorten the body past the syllable count the stress requires, and
// the Vv elision is skipped when Slot V carries the §3.5.1 glottal
// signal. Formatives with a Slot I shortcut go through renderShortcut
// instead.
func Formative(f g.Formative) string {
	if f.SlotIShortcut != nil {
		return renderShortcut(f)
	}
	var b strings.Builder
	b.WriteString(SlotI(f.SlotI))
	vv := g.SlotIIToVv(f.SlotII)
	if len(f.SlotV) >= 2 {
		vv = insertGlottalVv(vv)
	}
	b.WriteString(vv)
	b.WriteString(string(f.SlotIII))
	b.WriteString(g.SlotIVToVr(f.SlotIV))
	b.WriteString(SlotV(f.SlotV))
	ca := allomorph.ConstructCa(f.SlotVI)
	if len(f.SlotV) > 0 {
		ca = allomorph.GeminateCa(ca)
	}
	b.WriteString(ca)
	b.WriteString(SlotVII(f.SlotVII))
	b.WriteString(SlotVIII(f.SlotVIII))
	b.WriteString(SlotIX(f.SlotIX))
	body := padForANT(f, b.String())
	body = applyDefaultElisions(f, body)
	return applyStress(body, f.Stress)
}

// insertGlottalVv inserts a glottal stop into a Slot II Vv per §1.7.
// Vv is followed by Cr (a consonant) in the surface form, so a bare
// glottal stop at the end of Vv would form an impermissible glottal+C
// cluster; the rules of §1.7 require an epenthetic vowel.
//   - Single vowel: "a" → "a'a" (reduplicated).
//   - Diphthong: "ai" → "a'i" (glottal between the two vowels).
func insertGlottalVv(v string) string {
	rs := []rune(v)
	if len(rs) == 1 {
		return string(rs[0]) + "'" + string(rs[0])
	}
	if len(rs) == 2 {
		return string(rs[0]) + "'" + string(rs[1])
	}
	return v + "'"
}

// padForANT implements §5.8.8: if Stress is Antepenultimate and the
// rendered body has fewer than three syllables, append the default Slot
// IX Vc "a" (THM) so the diacritic has somewhere to land. Only triggers
// when Slot IX is genuinely absent — a constructed Formative with a
// non-nil Slot IX will already render its Vc/Vk vowel.
func padForANT(f g.Formative, body string) string {
	if f.Stress != g.Antepenultimate {
		return body
	}
	if countVowelConjuncts(body) >= 3 {
		return body
	}
	if f.SlotIX == nil {
		body += "a"
	}
	return body
}

// applyDefaultElisions drops the leading Vv "a" and/or trailing THM Vc
// "a" when the slot configuration allows and the result still has
// enough syllables for f.Stress. Both elide together when possible; if
// only one fits, the leading Vv is preferred (consonant-initial is the
// idiomatic surface form).
func applyDefaultElisions(f g.Formative, body string) string {
	canVv := canElideLeadingVv(f, body)
	canVc := canElideTrailingTHMVc(f, body)
	slack := countVowelConjuncts(body) - requiredSyllables(f.Stress)
	if slack < 0 {
		slack = 0
	}
	switch {
	case canVv && canVc && slack >= 2:
		return body[1 : len(body)-1]
	case canVv && slack >= 1:
		return body[1:]
	case canVc && slack >= 1:
		return body[:len(body)-1]
	}
	return body
}

// canElideLeadingVv reports whether the default S1/PRC Vv "a" is
// present and the slot configuration permits its elision per §3.2.
// Slot V affixes block Vv elision: with two or more affixes Vv
// carries the §3.5.1 glottal-stop signal; with one affix the Vv must
// stay visible so the parser can distinguish Slot V Cs from a plain Ca.
func canElideLeadingVv(f g.Formative, body string) bool {
	if f.SlotI != nil || f.SlotIShortcut != nil {
		return false
	}
	if f.SlotII != g.DefaultSlotII {
		return false
	}
	if f.CsRootDegree != nil {
		return false
	}
	if len(f.SlotV) > 0 {
		return false
	}
	return strings.HasPrefix(body, "a")
}

// canElideTrailingTHMVc reports whether Slot IX is THM with an "a"
// marker that can be dropped per §3.1.3. Ultimate-stressed formatives
// carry Vk, not Vc, so the rule does not apply there.
func canElideTrailingTHMVc(f g.Formative, body string) bool {
	if f.Stress == g.Ultimate {
		return false
	}
	cs, ok := f.SlotIX.(g.CaseSlot)
	if !ok || cs.Case != g.THM {
		return false
	}
	return strings.HasSuffix(body, "a")
}

// requiredSyllables returns the minimum vowel-conjunct count that
// preserves the formative's stress through a round-trip parse. ANT
// needs three syllables to place the diacritic; PEN needs two (going
// to one flips the reading to Monosyllabic/verbal); ULT and MONO need
// one. §5.8.8 says short formatives that can't reach the ANT minimum
// should be padded with default Slot II/VIII/IX syllables; we don't
// auto-pad yet — instead we just decline to elide.
func requiredSyllables(s g.Stress) int {
	switch s {
	case g.Antepenultimate:
		return 3
	case g.Penultimate:
		return 2
	case g.Ultimate, g.Monosyllabic:
		return 1
	}
	return 0
}

func countVowelConjuncts(s string) int {
	n := 0
	for _, c := range parse.SplitConjuncts(s) {
		if c == "" {
			continue
		}
		r, _ := utf8.DecodeRuneInString(c)
		if parse.IsVowelChar(r) {
			n++
		}
	}
	return n
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
// The Vx vowel is derived from the affix's (Type, Degree).
func SlotV(affixes []g.Affix) string {
	var b strings.Builder
	for _, a := range affixes {
		b.WriteString(a.Consonant)
		b.WriteString(parse.AffixVowel(a.Type, a.Degree))
	}
	return b.String()
}

// SlotVII renders Ca-scoped affixes in their Vx+Cs surface order.
// The Vx vowel is derived from the affix's (Type, Degree).
func SlotVII(affixes []g.Affix) string {
	var b strings.Builder
	for _, a := range affixes {
		b.WriteString(parse.AffixVowel(a.Type, a.Degree))
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
// Assertive yields its Validation as a Series-1 vowel; the eight non-
// ASR illocutions each yield their dedicated Series-2 diphthong.
func SlotIX(s g.SlotIX) string {
	switch v := s.(type) {
	case g.CaseSlot:
		return g.CaseToVc(v.Case)
	case g.Assertive:
		return Validation(v.Validation)
	case g.Directive:
		return "ai"
	case g.Declarative:
		return "au"
	case g.Interrogative:
		return "ei"
	case g.Verificative:
		return "eu"
	case g.Admonitive:
		return "ou"
	case g.Potentiative:
		return "oi"
	case g.Hortative:
		return "iu"
	case g.Conjectural:
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
