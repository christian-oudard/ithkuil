// Package render turns a grammar.Formative back into its surface text
// representation. The grammar layer is pure meaning; this package owns
// all phonetic and orthographic decisions — Cc and Vv consonant
// choices, special Vv markers for Cs- and reference-roots, shortcut
// form, default-value elisions, §3.5.1 / §3.6.1 Slot V signals, and
// §1.3.1 stress diacritic placement.
package render

import (
	"strings"
	"unicode/utf8"

	"github.com/coudard/ithkuil/go/allomorph"
	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/parse"
	"github.com/coudard/ithkuil/go/phonology"
)

// Options control orthographic choices that don't affect the grammar.
// All fields are false by default, giving the canonical long form with
// default elisions applied.
type Options struct {
	// Shortcut requests the Cc-Vv shortcut form when the formative
	// permits it (CrRoot with default SlotIV, encodable SlotVI, no
	// Slot V). When the formative isn't shortcut-encodable, the
	// option is silently ignored and long form is emitted.
	Shortcut bool
}

// Formative renders a formative to its canonical long-form surface
// string with default-value elisions applied.
func Formative(f g.Formative) string {
	return FormativeWithOpts(f, Options{})
}

// FormativeWithOpts renders a formative with the given options. See
// Options for what each toggle controls.
//
// Panics if f.Root or f.Final is nil — the zero value Formative{} is
// not a valid input. Construct via grammar.MinimalFormative or set
// Root and Final explicitly.
func FormativeWithOpts(f g.Formative, opts Options) string {
	if f.Root == nil {
		panic("render: Formative.Root is nil")
	}
	if f.Final == nil {
		panic("render: Formative.Final is nil")
	}
	if opts.Shortcut && canUseShortcut(f) {
		return renderShortcut(f)
	}
	return renderPlain(f)
}

// renderPlain emits the long surface form: Cc + Vv + Cr + Vr + Slot V
// + Ca + Slot VII + Slot VIII + Slot IX, followed by elisions and the
// stress diacritic.
func renderPlain(f g.Formative) string {
	var b strings.Builder
	b.WriteString(plainCc(f.Concat))
	b.WriteString(rootVv(f))
	b.WriteString(rootCr(f.Root))
	b.WriteString(rootVr(f.Root))
	b.WriteString(SlotV(f.SlotV))
	ca := allomorph.ConstructCa(f.SlotVI)
	if len(f.SlotV) > 0 {
		ca = allomorph.GeminateCa(ca)
	}
	b.WriteString(ca)
	b.WriteString(SlotVII(f.SlotVII))
	b.WriteString(SlotVIII(f.SlotVIII))
	b.WriteString(SlotIX(f.Final))
	body := padForANT(f, b.String())
	body = applyDefaultElisions(f, body)
	body = applyFinalStress(body, f.Final)
	if f.SentenceStarter {
		return sentencePrefix(body)
	}
	return body
}

// plainCc returns the Cc consonant for a non-shortcut formative —
// either nothing, "h", or "hw".
func plainCc(c *g.ConcatenationStatus) string {
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

// rootVv returns the Vv vowel for a formative's Root variant. CrRoot
// uses the standard Series-1 Vv encoding (Stem, Version). CsRoot uses
// one of four special Vv markers (ëi/eë/ëu/oë). RefRoot uses ae or ea.
// Slot V with ≥2 affixes inserts a §3.5.1 glottal-stop into Vv.
func rootVv(f g.Formative) string {
	v := bareRootVv(f.Root)
	if len(f.SlotV) >= 2 {
		v = insertGlottalVv(v)
	}
	return v
}

func bareRootVv(r g.Root) string {
	switch x := r.(type) {
	case g.CrRoot:
		return g.SlotIIToVv(g.SlotII{Stem: x.Stem, Version: x.Version})
	case g.CsRoot:
		return csRootVv(x.Function, x.Version)
	case g.RefRoot:
		return refRootVv(x.Version)
	}
	return ""
}

func csRootVv(fn g.Function, ver g.Version) string {
	switch {
	case fn == g.STA && ver == g.PRC:
		return "ëi"
	case fn == g.DYN && ver == g.PRC:
		return "eë"
	case fn == g.STA && ver == g.CPT:
		return "ëu"
	case fn == g.DYN && ver == g.CPT:
		return "oë"
	}
	return ""
}

func refRootVv(ver g.Version) string {
	switch ver {
	case g.PRC:
		return "ae"
	case g.CPT:
		return "ea"
	}
	return ""
}

func rootCr(r g.Root) string {
	switch x := r.(type) {
	case g.CrRoot:
		return x.Cluster
	case g.CsRoot:
		return x.Cs
	case g.RefRoot:
		return x.C1
	}
	return ""
}

// rootVr returns the Vr vowel. For CrRoot and RefRoot it's the
// standard Function/Specification/Context encoding; for CsRoot it's
// the affix Vr table (Degree, Context).
func rootVr(r g.Root) string {
	switch x := r.(type) {
	case g.CrRoot:
		return g.SlotIVToVr(x.SlotIV)
	case g.CsRoot:
		return csRootVr(x.Degree, x.Context)
	case g.RefRoot:
		return g.SlotIVToVr(x.SlotIV)
	}
	return ""
}

// csRootVr encodes (Degree, Context) for a Cs-root. Series is given
// by Context (EXS=1, FNC=2, RPS=3, AMG=4); form is the Degree (0-9).
// Degree-0 forms have special spellings per the affix Vr table.
func csRootVr(degree int, ctx g.Context) string {
	if degree == 0 {
		switch ctx {
		case g.EXS:
			return "ae"
		case g.FNC:
			return "ea"
		case g.RPS:
			return "üo"
		case g.AMG:
			return "üö"
		}
	}
	return phonology.VowelForm(int(ctx)+1, degree)
}

// insertGlottalVv inserts a glottal stop into a Slot II Vv per §1.7.
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

// padForANT implements §5.8.8: if Final is FramedVerbal and the
// rendered body has fewer than three syllables, append the default
// Slot IX Vc "a" (THM) so the diacritic has somewhere to land.
func padForANT(f g.Formative, body string) string {
	if _, ok := f.Final.(g.FramedVerbal); !ok {
		return body
	}
	if countVowelConjuncts(body) >= 3 {
		return body
	}
	if SlotIX(f.Final) == "" {
		body += "a"
	}
	return body
}

// applyDefaultElisions drops the leading Vv "a" and/or trailing THM Vc
// "a" when the slot configuration allows and the result still has
// enough syllables for f.Final's stress role.
func applyDefaultElisions(f g.Formative, body string) string {
	canVv := canElideLeadingVv(f, body)
	canVc := canElideTrailingTHMVc(f, body)
	slack := countVowelConjuncts(body) - requiredSyllables(f.Final)
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
// Only fully-default CrRoot Vv is elidable; CsRoot and RefRoot use
// special Vv markers that aren't "a", and any non-default Stem,
// Version, or Concat keeps the Vv visible.
func canElideLeadingVv(f g.Formative, body string) bool {
	if f.Concat != nil {
		return false
	}
	cr, ok := f.Root.(g.CrRoot)
	if !ok {
		return false
	}
	if cr.Stem != g.S1 || cr.Version != g.PRC {
		return false
	}
	if len(f.SlotV) > 0 {
		return false
	}
	return strings.HasPrefix(body, "a")
}

// canElideTrailingTHMVc reports whether Final is a Case-carrying
// variant with THM that can drop its trailing "a" per §3.1.3.
func canElideTrailingTHMVc(f g.Formative, body string) bool {
	var c g.Case
	switch v := f.Final.(type) {
	case g.UnframedNominal:
		c = v.Case
	case g.FramedVerbal:
		c = v.Case
	default:
		return false
	}
	if c != g.THM {
		return false
	}
	return strings.HasSuffix(body, "a")
}

// requiredSyllables returns the minimum vowel-conjunct count that
// preserves the formative's category through a round-trip parse.
func requiredSyllables(f g.Final) int {
	switch f.(type) {
	case g.FramedVerbal:
		return 3
	case g.UnframedNominal:
		return 2
	case g.UnframedVerbal:
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

// SlotV renders stem affixes in their Cs+Vx (reversed) surface order.
func SlotV(affixes []g.Affix) string {
	var b strings.Builder
	for _, a := range affixes {
		b.WriteString(a.Consonant)
		b.WriteString(parse.AffixVowel(a.Type, a.Degree))
	}
	return b.String()
}

// SlotVII renders Ca-scoped affixes in their Vx+Cs surface order.
func SlotVII(affixes []g.Affix) string {
	var b strings.Builder
	for _, a := range affixes {
		b.WriteString(parse.AffixVowel(a.Type, a.Degree))
		b.WriteString(a.Consonant)
	}
	return b.String()
}

// SlotVIII renders the VnCn slot. Empty when SlotVIII is nil.
func SlotVIII(s g.SlotVIII) string {
	if s == nil {
		return ""
	}
	switch v := s.(type) {
	case g.VnCnValence:
		return Valence(v.Valence) + moodCnP1[v.MoodScope]
	case g.VnCnPhase:
		return Phase(v.Phase) + moodCnP1[v.MoodScope]
	case g.VnCnEffect:
		return Effect(v.Effect) + moodCnP1[v.MoodScope]
	case g.VnCnLevel:
		return Level(v.Level) + moodCnP1[v.MoodScope]
	case g.VnCnAspect:
		return Aspect(v.Aspect) + moodCnP2[v.MoodScope]
	}
	return ""
}

// SlotIX renders the final slot (Vc or Vk) based on the Final variant.
func SlotIX(f g.Final) string {
	switch v := f.(type) {
	case g.UnframedNominal:
		return g.CaseToVc(v.Case)
	case g.FramedVerbal:
		return g.CaseToVc(v.Case)
	case g.UnframedVerbal:
		return Vk(v.Vk)
	}
	return ""
}

// Vk renders a verbal-ending variant as its surface vowel.
func Vk(v g.Vk) string {
	switch x := v.(type) {
	case g.Assertive:
		return Validation(x.Validation)
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

var moodCnP1 = [...]string{"h", "hl", "hr", "hm", "hn", "hň"}
var moodCnP2 = [...]string{"w", "hw", "hrw", "hmw", "hnw", "hňw"}

func sentencePrefix(body string) string {
	// §3.13 sentence-start marker is "ç" prepended directly to the
	// surface form. A formative that begins with a consonant gets the
	// reduced "çë" + body form; vowel-initial bodies just get "ç".
	if body == "" {
		return body
	}
	first, _ := utf8.DecodeRuneInString(body)
	if parse.IsVowelChar(first) {
		return "ç" + body
	}
	return "çë" + body
}
