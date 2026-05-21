package slots

import (
	"fmt"
	"strings"

	"github.com/christian-oudard/ithkuil/allomorph"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/surface"
)

// ToGrammar converts a Layout into a grammar.Formative — Layer D
// forward. Each string slot is decoded via the per-slot parsers in
// package parse, then composed.
func ToGrammar(l Layout) (g.Formative, error) {
	cc := parse.ParseCc(l.Cc)
	concat := cc.Concat
	shortcut := cc.Shortcut

	vcLookup := l.Vc
	if l.MovedGlottal {
		vcLookup = restoreMovedGlottal(l.Vc)
	}
	final, err := finalFromVc(vcLookup, l.Stress)
	if err != nil {
		return g.Formative{}, err
	}

	// Slot VIII is provisional in the Layout: Layer C peels off the
	// last (Vn, Cn) candidate by Cn-shape alone. If the Vn doesn't
	// decode against that Cn, fold the pair back into Slot VII as a
	// regular affix.
	var slotVIII g.SlotVIII
	slotVII := affixesVxCs(l.SlotVII)
	if l.Cn != "" {
		if s8, ok := parse.ParseVnCn(l.Vn, l.Cn); ok {
			slotVIII = s8
		} else {
			t, d := parse.ClassifyAffixVowel(l.Vn)
			slotVII = append(slotVII, g.Affix{Type: t, Degree: d, Consonant: l.Cn})
		}
	}
	slotV := affixesVxCs(l.SlotV)

	root, slotVI, err := rootFromLayout(l, shortcut)
	if err != nil {
		return g.Formative{}, err
	}

	f := g.Formative{
		Concat:          concat,
		Root:            root,
		SlotV:           slotV,
		SlotVI:          slotVI,
		SlotVII:         slotVII,
		SlotVIII:        slotVIII,
		Final:           final,
		SentenceStarter: l.SentenceStarter,
	}
	f.Surface = hintsFromLayout(l, f)
	return f, nil
}

// hintsFromLayout derives the SurfaceHints recording the orthographic
// choices the parsed Layout embodied. The returned struct is always
// non-nil so a parsed Formative re-renders verbatim — that's the whole
// point of lossless tracking. Programmatic callers who skip parse and
// want canonical defaults leave Formative.Surface as nil.
//
// Each flag records the speaker's actual choice:
//
//   - CcShortcut: the §3.2 Cc shortcut (w/y/hl/hm/hr/hn) was used.
//   - CnCaShortcut: the §3.8.1.2 Cn→Ca shortcut was applied.
//   - MovedGlottal: the §3.9.1 V_C glottal was moved earlier.
//   - KeepVv: the default Vv "a" was emitted instead of being elided.
//     Only recorded when elision was actually available — otherwise
//     every long-form word with Vv="a" would set it spuriously.
//   - KeepVc: same gating for the trailing THM Vc "a".
func hintsFromLayout(l Layout, f g.Formative) *g.SurfaceHints {
	return &g.SurfaceHints{
		CcShortcut:   isShortcutCc(l.Cc),
		CnCaShortcut: l.CnInCa,
		MovedGlottal: l.MovedGlottal,
		KeepVv:       canElideLeadingVv(&l, f),
		KeepVc:       canElideTrailingTHMVc(&l, f),
	}
}

// rootFromLayout decodes the Root and SlotVI together — they're
// coupled by shortcut handling (the Cc-Vv pair encodes both the
// shortcut variant and Ca).
func rootFromLayout(l Layout, shortcut parse.ShortcutVariant) (g.Root, g.SlotVI, error) {
	switch l.Kind {
	case CrFormative:
		if shortcut != parse.ShortcutNone {
			slotII, ok := parse.ParseSlotII(l.Vv)
			if !ok {
				return nil, g.SlotVI{}, fmt.Errorf("invalid Vv %q in shortcut", l.Vv)
			}
			series := parse.VvSeries(l.Vv)
			return g.CrRoot{
				Cluster: l.Cr,
				Stem:    slotII.Stem,
				Version: slotII.Version,
				SlotIV:  g.DefaultSlotIV,
			}, parse.ShortcutCa(shortcut, series), nil
		}
		var slotII g.SlotII
		if l.Vv == "" {
			slotII = g.DefaultSlotII
		} else {
			parsed, ok := parse.ParseSlotII(l.Vv)
			if !ok {
				return nil, g.SlotVI{}, fmt.Errorf("invalid Vv %q", l.Vv)
			}
			slotII = parsed
		}
		var slotIV g.SlotIV
		if l.Vr == "" {
			slotIV = g.DefaultSlotIV
		} else {
			parsed, ok := parse.ParseSlotIV(l.Vr)
			if !ok {
				return nil, g.SlotVI{}, fmt.Errorf("invalid Vr %q", l.Vr)
			}
			slotIV = parsed
		}
		slotVI, ok := allomorph.ParseCa(l.Ca)
		if !ok {
			return nil, g.SlotVI{}, fmt.Errorf("unrecognized Ca %q", l.Ca)
		}
		return g.CrRoot{
			Cluster: l.Cr,
			Stem:    slotII.Stem,
			Version: slotII.Version,
			SlotIV:  slotIV,
		}, slotVI, nil
	case CsRootFormative:
		sv, ok := parse.ParseSpecialVv(l.Vv)
		if !ok || sv.Function == nil {
			return nil, g.SlotVI{}, fmt.Errorf("invalid Cs-root Vv %q", l.Vv)
		}
		degree, ctx, ok := parse.ParseAffixVr(l.Vr)
		if !ok {
			return nil, g.SlotVI{}, fmt.Errorf("invalid Cs-root Vr %q", l.Vr)
		}
		slotVI, ok := allomorph.ParseCa(l.Ca)
		if !ok {
			return nil, g.SlotVI{}, fmt.Errorf("unrecognized Ca %q", l.Ca)
		}
		return g.CsRoot{
			Cs:       l.Cr,
			Degree:   degree,
			Version:  sv.Version,
			Function: *sv.Function,
			Context:  ctx,
		}, slotVI, nil
	case RefRootFormative:
		sv, ok := parse.ParseSpecialVv(l.Vv)
		if !ok {
			return nil, g.SlotVI{}, fmt.Errorf("invalid ref-root Vv %q", l.Vv)
		}
		slotIV, ok := parse.ParseSlotIV(l.Vr)
		if !ok {
			slotIV = g.DefaultSlotIV
		}
		slotVI, ok := allomorph.ParseCa(l.Ca)
		if !ok {
			return nil, g.SlotVI{}, fmt.Errorf("unrecognized Ca %q", l.Ca)
		}
		return g.RefRoot{
			C1:      l.Cr,
			Version: sv.Version,
			SlotIV:  slotIV,
		}, slotVI, nil
	}
	return nil, g.SlotVI{}, fmt.Errorf("unknown root kind %d", l.Kind)
}

// affixesVxCs decodes a list of AffixChunks (each carrying a raw Vx
// vowel and Cs consonant) into grammar Affixes.
func affixesVxCs(chunks []AffixChunk) []g.Affix {
	if len(chunks) == 0 {
		return nil
	}
	out := make([]g.Affix, len(chunks))
	for i, c := range chunks {
		t, d := parse.ClassifyAffixVowel(c.Vx)
		out[i] = g.Affix{Type: t, Degree: d, Consonant: c.Cs}
	}
	return out
}

// restoreMovedGlottal re-inserts the glottal stop that the §3.9.1
// SPECIAL NOTE shortening rule shifted off the Vc. Per §1.7: a single
// vowel reduplicates around the glottal (a → a'a), and a multi-rune
// disyllabic conjunct takes an intervocalic glottal (ai → a'i, uä → u'ä).
func restoreMovedGlottal(vc string) string {
	rs := []rune(vc)
	switch len(rs) {
	case 0:
		return vc
	case 1:
		return string(rs[0]) + "'" + string(rs[0])
	}
	return string(rs[0]) + "'" + string(rs[1:])
}

// finalFromVc builds the Final variant from the trailing Slot IX
// vowel (may be empty) and the observed stress.
func finalFromVc(vc string, stress surface.Stress) (g.Final, error) {
	switch stress {
	case surface.Ultimate, surface.Monosyllabic:
		if vc == "" {
			return g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}, nil
		}
		vk, ok := parse.ParseVk(vc)
		if !ok {
			return nil, fmt.Errorf("invalid Vk %q", vc)
		}
		return g.UnframedVerbal{Vk: vk}, nil
	case surface.Antepenultimate:
		c := g.THM
		if vc != "" {
			cs, ok := parse.ParseCase(vc)
			if !ok {
				return nil, fmt.Errorf("invalid Vc %q", vc)
			}
			c = cs
		}
		return g.FramedVerbal{Case: c}, nil
	case surface.Penultimate:
		c := g.THM
		if vc != "" {
			cs, ok := parse.ParseCase(vc)
			if !ok {
				return nil, fmt.Errorf("invalid Vc %q", vc)
			}
			c = cs
		}
		return g.UnframedNominal{Case: c}, nil
	}
	return nil, fmt.Errorf("unknown stress %v", stress)
}

// FromGrammar converts a grammar.Formative into a Layout — Layer D
// inverse. Surface choices (shortcut yes/no, default-value elisions,
// special-Vv selection) are made here. The opts argument lets callers
// request a shortcut surface form when the formative permits it.
func FromGrammar(f g.Formative, opts Options) Layout {
	if f.Root == nil {
		panic("slots: Formative.Root is nil")
	}
	if f.Final == nil {
		panic("slots: Formative.Final is nil")
	}
	l := Layout{SentenceStarter: f.SentenceStarter}

	useShortcut := (opts.Shortcut || (f.Surface != nil && f.Surface.CcShortcut)) && canUseShortcut(f)
	l.Cc = ccFromGrammar(f.Concat, useShortcut, f)

	switch r := f.Root.(type) {
	case g.CrRoot:
		l.Kind = CrFormative
		l.Cr = r.Cluster
		if useShortcut {
			series := shortcutSeries(f.SlotVI)
			l.Vv = g.SlotIIToVvSeries(g.SlotII{Stem: r.Stem, Version: r.Version}, series)
			// Vr, Ca elided
		} else {
			l.Vv = g.SlotIIToVv(g.SlotII{Stem: r.Stem, Version: r.Version})
			l.Vr = g.SlotIVToVr(r.SlotIV)
			l.Ca = allomorph.ConstructCa(f.SlotVI)
		}
	case g.CsRoot:
		l.Kind = CsRootFormative
		l.Cr = r.Cs
		l.Vv = csRootVv(r.Function, r.Version)
		l.Vr = csRootVr(r.Degree, r.Context)
		l.Ca = allomorph.ConstructCa(f.SlotVI)
	case g.RefRoot:
		l.Kind = RefRootFormative
		l.Cr = r.C1
		l.Vv = refRootVv(r.Version)
		l.Vr = g.SlotIVToVr(r.SlotIV)
		l.Ca = allomorph.ConstructCa(f.SlotVI)
	}

	for _, a := range f.SlotV {
		l.SlotV = append(l.SlotV, AffixChunk{
			Vx: parse.AffixVowel(a.Type, a.Degree),
			Cs: a.Consonant,
		})
	}
	for _, a := range f.SlotVII {
		l.SlotVII = append(l.SlotVII, AffixChunk{
			Vx: parse.AffixVowel(a.Type, a.Degree),
			Cs: a.Consonant,
		})
	}

	if f.SlotVIII != nil {
		l.Vn, l.Cn = vnCnFromSlotVIII(f.SlotVIII)
	}

	l.Vc, l.Stress = slotIXFromFinal(f.Final)

	// §3.8.1.2 shortcut is decided before default-value elision so the
	// shortcut's freed-up syllable isn't claimed by elision first — that
	// ordering let elision burn enough slack to keep the shortcut's
	// minimum-syllables guard from firing, and the long form leaked out.
	//
	// Surface hints, when present, override the canonical auto-fire
	// rules: a hint set false suppresses the shortcut even when its
	// conditions match, so a long-form input round-trips back to the
	// long form.
	if f.Surface == nil || f.Surface.CnCaShortcut {
		maybeMoveCnToCa(&l, f)
	}
	if f.Surface == nil || f.Surface.MovedGlottal {
		maybeShortenVcGlottal(&l, f)
	}
	applyDefaultElisions(&l, f)
	return l
}

// maybeShortenVcGlottal applies the §3.9.1 SPECIAL NOTE shortening for
// Relational and Affinitive cases (37-52): the Vc glottal-stop, which
// would otherwise sit on the trailing case vowel, is moved earlier in
// the word and rides into the first conjunct after Vr at render time.
//
// The spec disallows the shift in two contexts:
//
//   - When a Slot IV/VI a+Ca shortcut is in play. In that surface the
//     Vr has been elided into the Cc-Vv pair, so there is no Vr to
//     carry the glottal.
//   - When the §3.8.1.2 Cn→Ca shortcut has been applied. The §3.6.2
//     footnote and §3.9.1 itself make the two mutually exclusive.
//
// On parse, stripMovedGlottal lifts the glottal off whichever conjunct
// carried it; restoreMovedGlottal then reassembles the canonical Vc.
// Here we do the inverse on the way out so re-rendered output matches
// what a speaker would naturally write.
func maybeShortenVcGlottal(l *Layout, f g.Formative) {
	var c g.Case
	switch v := f.Final.(type) {
	case g.UnframedNominal:
		c = v.Case
	case g.FramedVerbal:
		c = v.Case
	default:
		return
	}
	grp := c.Group()
	if grp != g.Relational && grp != g.Affinitive {
		return
	}
	if l.Vr == "" || l.CnInCa {
		return
	}
	stripped := stripVvGlottal(l.Vc)
	if stripped == l.Vc {
		return
	}
	l.Vc = stripped
	l.MovedGlottal = true
}

// maybeMoveCnToCa applies the §3.8.1.2 shortening: when Slot VIII has
// default MNO Valence with a Pattern-1 non-FAC Mood/Case-Scope, and
// Slot VI is default ("l"), the Cn consonant takes the Ca position and
// Vn/Cn elide. Render reads CnInCa to emit the shortcut form.
//
// We only apply the shortcut when the resulting body would still carry
// enough syllables to host the stress diacritic — dropping below that
// minimum would render the formative ambiguous against the
// monosyllabic-default rule.
func maybeMoveCnToCa(l *Layout, f g.Formative) {
	if len(f.SlotV) > 0 || l.Ca != "l" || f.SlotVI != g.DefaultSlotVI {
		return
	}
	v, ok := f.SlotVIII.(g.VnCnValence)
	if !ok || v.Valence != g.MNO {
		return
	}
	if !isMovedCn(l.Cn) {
		return
	}
	// CnInCa elides the Vn vowel — confirm the body still has room
	// for the stress diacritic afterwards.
	if vowelCount(l)-1 < requiredSyllables(f.Final) {
		return
	}
	l.CnInCa = true
}

// Options controls orthographic choices that don't affect the grammar.
type Options struct {
	// Shortcut requests the Cc-Vv shortcut form when permissible.
	Shortcut bool
}

// canUseShortcut reports whether the formative's grammar permits a
// Cc-shortcut surface form: a CrRoot with default SlotIV and a SlotVI
// that the shortcut table can encode. Slot V is allowed per §3.6.2 —
// the renderer signals end-of-Slot-V with a glottal on the final Vx.
func canUseShortcut(f g.Formative) bool {
	cr, ok := f.Root.(g.CrRoot)
	if !ok {
		return false
	}
	if cr.SlotIV != g.DefaultSlotIV {
		return false
	}
	return shortcutSeries(f.SlotVI) != 0
}

// shortcutSeries returns the Vv series (1-4) used to encode the given
// SlotVI under the W shortcut, or 0 if no Y-row encoding exists either.
// The caller knows which shortcut letter (w or y) to pair this with
// via the SlotVI value itself.
func shortcutSeries(s g.SlotVI) int {
	for _, sc := range []parse.ShortcutVariant{parse.ShortcutW, parse.ShortcutY} {
		for series := 1; series <= 4; series++ {
			if parse.ShortcutCa(sc, series) == s {
				return series
			}
		}
	}
	return 0
}

// shortcutVariant returns the Cc shortcut letter (W or Y) that pairs
// with the SlotVI when the formative is rendered as a shortcut.
func shortcutVariant(s g.SlotVI) parse.ShortcutVariant {
	for series := 1; series <= 4; series++ {
		if parse.ShortcutCa(parse.ShortcutW, series) == s {
			return parse.ShortcutW
		}
		if parse.ShortcutCa(parse.ShortcutY, series) == s {
			return parse.ShortcutY
		}
	}
	return parse.ShortcutNone
}

// ccFromGrammar picks the Cc consonant for a (Concat, Shortcut) pair.
func ccFromGrammar(concat *g.ConcatenationStatus, useShortcut bool, f g.Formative) string {
	if useShortcut {
		variant := shortcutVariant(f.SlotVI)
		if concat == nil {
			switch variant {
			case parse.ShortcutW:
				return "w"
			case parse.ShortcutY:
				return "y"
			}
		}
		switch *concat {
		case g.Type1:
			switch variant {
			case parse.ShortcutW:
				return "hl"
			case parse.ShortcutY:
				return "hm"
			}
		case g.Type2:
			switch variant {
			case parse.ShortcutW:
				return "hr"
			case parse.ShortcutY:
				return "hn"
			}
		}
		return ""
	}
	if concat == nil {
		return ""
	}
	switch *concat {
	case g.Type1:
		return "h"
	case g.Type2:
		return "hw"
	}
	return ""
}

// csRootVv encodes (Function, Version) as one of the four special Vv
// markers for Cs-roots.
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

// refRootVv encodes Version as the reference-root special Vv.
func refRootVv(ver g.Version) string {
	switch ver {
	case g.PRC:
		return "ae"
	case g.CPT:
		return "ea"
	}
	return ""
}

// csRootVr encodes (Degree, Context) for a Cs-root. Degree-0 forms have
// special spellings per the affix Vr table.
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

// vnCnFromSlotVIII decomposes a Slot VIII value into its (Vn, Cn) pair.
func vnCnFromSlotVIII(s g.SlotVIII) (string, string) {
	switch v := s.(type) {
	case g.VnCnValence:
		return valenceVowel(v.Valence), moodCnP1(v.MoodScope)
	case g.VnCnPhase:
		return phaseVowel(v.Phase), moodCnP1(v.MoodScope)
	case g.VnCnEffect:
		return effectVowel(v.Effect), moodCnP1(v.MoodScope)
	case g.VnCnLevel:
		return levelVowel(v.Level), moodCnP1(v.MoodScope)
	case g.VnCnAspect:
		return aspectVowel(v.Aspect), moodCnP2(v.MoodScope)
	}
	return "", ""
}

func valenceVowel(v g.Valence) string {
	return [...]string{"a", "ä", "e", "i", "ëi", "ö", "o", "ü", "u"}[v]
}
func phaseVowel(p g.Phase) string {
	return [...]string{"ai", "au", "ei", "eu", "ëu", "ou", "oi", "iu", "ui"}[p]
}
func effectVowel(e g.Effect) string {
	return [...]string{"ia", "ie", "io", "iö", "eë", "uö", "uo", "ue", "ua"}[e]
}
func levelVowel(l g.Level) string {
	return [...]string{"ao", "aö", "eo", "eö", "oë", "öe", "oe", "öa", "oa"}[l]
}

var aspectVowels = [...]string{
	"a", "ä", "e", "i", "ëi", "ö", "o", "ü", "u",
	"ai", "au", "ei", "eu", "ëu", "ou", "oi", "iu", "ui",
	"ia", "ie", "io", "iö", "eë", "uö", "uo", "ue", "ua",
	"ao", "aö", "eo", "eö", "oë", "öe", "oe", "öa", "oa",
}

func aspectVowel(a g.Aspect) string { return aspectVowels[a] }

var moodCnP1Table = [...]string{"h", "hl", "hr", "hm", "hn", "hň"}
var moodCnP2Table = [...]string{"w", "hw", "hrw", "hmw", "hnw", "hňw"}

func moodCnP1(m g.Mood) string { return moodCnP1Table[m] }
func moodCnP2(m g.Mood) string { return moodCnP2Table[m] }

// slotIXFromFinal picks the trailing vowel and the stress diacritic to
// apply based on the formative's grammatical category.
func slotIXFromFinal(f g.Final) (string, surface.Stress) {
	switch v := f.(type) {
	case g.UnframedNominal:
		return g.CaseToVc(v.Case), surface.Penultimate
	case g.FramedVerbal:
		return g.CaseToVc(v.Case), surface.Antepenultimate
	case g.UnframedVerbal:
		return vkVowel(v.Vk), surface.Ultimate
	}
	return "", surface.Penultimate
}

// vkVowel renders a Vk variant as its surface vowel.
func vkVowel(v g.Vk) string {
	switch x := v.(type) {
	case g.Assertive:
		return [...]string{"a", "ä", "e", "i", "ëi", "ö", "o", "ü", "u"}[x.Validation]
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

// applyDefaultElisions drops the leading Vv "a" and/or trailing THM Vc
// "a" when the slot configuration allows and the formative still has
// enough syllables for the stress diacritic to land. The §5.8.8 ANT
// pad is also applied here when the body is too short.
func applyDefaultElisions(l *Layout, f g.Formative) {
	// §5.8.8: a FramedVerbal with fewer than three syllables pads with a
	// trailing "a" (THM Vc) so the antepenult diacritic has a place to
	// sit.
	if _, framed := f.Final.(g.FramedVerbal); framed {
		if vowelCount(l) < 3 && l.Vc == "" {
			l.Vc = "a"
		}
	}

	canVv := canElideLeadingVv(l, f)
	canVc := canElideTrailingTHMVc(l, f) || canElideMonosyllabicVerbalVc(l, f)
	// Surface hints from a parsed long-form input pin the leading Vv
	// and/or trailing Vc in place. KeepVv / KeepVc only matter when the
	// elision was available — without the gate every word would set
	// them, which is the opposite of what they record.
	if f.Surface != nil {
		if f.Surface.KeepVv {
			canVv = false
		}
		if f.Surface.KeepVc {
			canVc = false
		}
	}
	slack := vowelCount(l) - requiredSyllables(f.Final)
	if slack < 0 {
		slack = 0
	}
	switch {
	case canVv && canVc && slack >= 2:
		l.Vv = ""
		l.Vc = ""
	case canVv && slack >= 1:
		l.Vv = ""
	case canVc && slack >= 1:
		l.Vc = ""
	}
}

// canElideMonosyllabicVerbalVc reports whether the trailing Vc "a" can
// drop because the formative is UnframedVerbal{Assertive{OBS}} and the
// resulting body would be exactly one syllable. Per §3.10 a
// monosyllabic word carries implicit ultimate stress with no
// diacritic, so eliding the Vk-vowel "a" round-trips faithfully via
// finalFromVc's monosyllabic branch.
//
// Only the Assertive/OBS combination is eligible — its surface vowel
// "a" happens to coincide with the THM-default that already elides
// elsewhere, and dropping it produces a form that the parser will
// reconstitute as Assertive/OBS via the monosyllabic-implicit-ultimate
// rule. Any other Vk has a distinguishing vowel that the speaker
// would actually write.
func canElideMonosyllabicVerbalVc(l *Layout, f g.Formative) bool {
	uv, ok := f.Final.(g.UnframedVerbal)
	if !ok {
		return false
	}
	asr, ok := uv.Vk.(g.Assertive)
	if !ok || asr.Validation != g.OBS {
		return false
	}
	if l.Vc != "a" {
		return false
	}
	// Eliding Vc must leave at least one syllable so the body remains
	// a pronounceable monosyllabic-equivalent.
	return vowelCount(l)-1 >= 1
}

func canElideLeadingVv(l *Layout, f g.Formative) bool {
	if f.Concat != nil {
		return false
	}
	if l.Kind != CrFormative {
		return false
	}
	if l.Cc != "" {
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
	return strings.HasPrefix(l.Vv, "a") && l.Vv == "a"
}

func canElideTrailingTHMVc(l *Layout, f g.Formative) bool {
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
	return l.Vc == "a"
}

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

// vowelCount counts the number of vowel-conjuncts the Layout will emit
// when rendered. Used by elision logic to confirm the body has enough
// syllables for the diacritic to land. The §3.8.1.2 Cn→Ca shortcut
// elides Vn at render time even though l.Vn is still set, so CnInCa
// subtracts one.
func vowelCount(l *Layout) int {
	n := 0
	if l.Vv != "" {
		n++
	}
	if l.Vr != "" {
		n++
	}
	for _, a := range l.SlotV {
		if a.Vx != "" {
			n++
		}
	}
	for _, a := range l.SlotVII {
		if a.Vx != "" {
			n++
		}
	}
	if l.Vn != "" && !l.CnInCa {
		n++
	}
	if l.Vc != "" {
		n++
	}
	return n
}
