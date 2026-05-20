package layout

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

	final, err := finalFromVc(l.Vc, l.Stress)
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

	return g.Formative{
		Concat:          concat,
		Root:            root,
		SlotV:           slotV,
		SlotVI:          slotVI,
		SlotVII:         slotVII,
		SlotVIII:        slotVIII,
		Final:           final,
		SentenceStarter: l.SentenceStarter,
	}, nil
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
		panic("layout: Formative.Root is nil")
	}
	if f.Final == nil {
		panic("layout: Formative.Final is nil")
	}
	l := Layout{SentenceStarter: f.SentenceStarter}

	useShortcut := opts.Shortcut && canUseShortcut(f)
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

	applyDefaultElisions(&l, f)
	return l
}

// Options controls orthographic choices that don't affect the grammar.
type Options struct {
	// Shortcut requests the Cc-Vv shortcut form when permissible.
	Shortcut bool
}

// canUseShortcut reports whether the formative's grammar permits a
// Cc-shortcut surface form: a CrRoot with default SlotIV, no Slot V,
// and a SlotVI that the shortcut table can encode.
func canUseShortcut(f g.Formative) bool {
	cr, ok := f.Root.(g.CrRoot)
	if !ok {
		return false
	}
	if cr.SlotIV != g.DefaultSlotIV {
		return false
	}
	if len(f.SlotV) > 0 {
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
	canVc := canElideTrailingTHMVc(l, f)
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
// syllables for the diacritic to land.
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
	if l.Vn != "" {
		n++
	}
	if l.Vc != "" {
		n++
	}
	return n
}
