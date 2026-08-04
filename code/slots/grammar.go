package slots

import (
	"fmt"
	"strings"
	"unicode/utf8"

	"github.com/christian-oudard/ithkuil/allomorph"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
)

// ToGrammar converts a Layout into a grammar.Formative — Layer D
// forward. Each string slot is decoded via the per-slot parsers in
// package parse, then composed.
func ToGrammar(l Layout) (g.Formative, error) {
	var fs faults
	cc := parse.ParseCc(l.Cc)
	concat := cc.Concat
	shortcut := cc.Shortcut

	vcLookup := l.Vc
	if l.MovedGlottal {
		vcLookup = restoreMovedGlottal(l.Vc)
	}
	final := finalFromVc(&fs, vcLookup, l.Stress, concat)

	// Slot VIII is provisional in the Layout: Layer C peels off the
	// last (Vn, Cn) candidate by Cn-shape alone. If the Vn doesn't
	// decode against that Cn, fold the pair back into Slot VII as a
	// regular affix.
	var slotVIII g.SlotVIII
	slotVII := affixesVxCs(&fs, "Vx", l.SlotVII)
	if l.Cn != "" {
		if s8, ok := parse.ParseVnCn(l.Vn, l.Cn); ok {
			// The defaults say nothing an absent slot does not, so
			// only one of the two reaches a Formative.
			if !g.SlotVIIIIsDefault(s8) {
				slotVIII = s8
			}
		} else if t, d, ok := parse.AffixVowelDegree(l.Vn); ok {
			slotVII = append(slotVII, g.Affix{Type: t, Degree: d, Consonant: l.Cn})
		} else {
			fs.add("Vn", l.Vn, fmt.Sprintf(
				"no Valence, Phase, Effect, Level or Aspect is written %q against Cn %q, and it is not a Vx affix vowel either",
				l.Vn, l.Cn))
		}
	}
	slotV := affixesVxCs(&fs, "Vx", l.SlotV)

	root, slotVI := rootFromLayout(&fs, l, shortcut)
	if err := fs.err(Render(l)); err != nil {
		return g.Formative{}, err
	}

	return g.Formative{
		Concat:   concat,
		Root:     root,
		SlotV:    slotV,
		SlotVI:   slotVI,
		SlotVII:  slotVII,
		SlotVIII: slotVIII,
		Final:    final,
	}, nil
}

// rootFromLayout decodes the Root and SlotVI together — they're
// coupled by shortcut handling (the Cc-Vv pair encodes both the
// shortcut variant and Ca).
func rootFromLayout(fs *faults, l Layout, shortcut parse.ShortcutVariant) (g.Root, g.SlotVI) {
	// ca decodes the Slot VI cluster, which every root kind needs and
	// none of them reads differently.
	ca := func() g.SlotVI {
		slotVI, ok := allomorph.ParseCa(l.Ca)
		if !ok {
			fs.add("Ca", l.Ca, "no Ca complex is written "+l.Ca+
				"; a Ca spells Configuration, Affiliation, Perspective, Extension and Essence in that order")
			return g.DefaultSlotVI
		}
		return slotVI
	}
	switch l.Kind {
	case CrFormative:
		if shortcut != parse.ShortcutNone {
			slotII, ok := parse.ParseSlotII(l.Vv)
			if !ok {
				fs.add("Vv", l.Vv, "no Stem/Version pair is written "+l.Vv+" in a Slot I shortcut")
				slotII = g.DefaultSlotII
			}
			series := parse.VvSeries(l.Vv)
			return g.CrRoot{
				Cluster: l.Cr,
				Stem:    slotII.Stem,
				Version: slotII.Version,
				SlotIV:  g.DefaultSlotIV,
			}, parse.ShortcutCa(shortcut, series)
		}
		slotII := g.DefaultSlotII
		if l.Vv != "" {
			parsed, ok := parse.ParseSlotII(l.Vv)
			if !ok {
				fs.add("Vv", l.Vv, "no Stem/Version pair is written "+l.Vv)
			} else {
				slotII = parsed
			}
		}
		// §3 (Permissible Consonant Forms): no Cr root and no Cs affix
		// begins with h-, w- or y-. A segmentation that lands one of
		// those in the root has gone wrong somewhere upstream, and
		// saying so beats handing back a root that cannot exist.
		if strings.HasPrefix(l.Cr, "h") || strings.HasPrefix(l.Cr, "w") || strings.HasPrefix(l.Cr, "y") {
			fs.add("Cr", l.Cr, "§3 admits no root beginning with h-, w- or y-")
		}
		slotIV := g.DefaultSlotIV
		if l.Vr != "" {
			parsed, ok := parse.ParseSlotIV(l.Vr)
			if !ok {
				fs.add("Vr", l.Vr, "no Function/Specification/Context triple is written "+l.Vr)
			} else {
				slotIV = parsed
			}
		}
		return g.CrRoot{
			Cluster: l.Cr,
			Stem:    slotII.Stem,
			Version: slotII.Version,
			SlotIV:  slotIV,
		}, ca()
	case CsRootFormative:
		sv, ok := parse.ParseSpecialVv(l.Vv)
		if !ok || sv.Function == nil {
			fs.add("Vv", l.Vv, "a §4.2 Cs-root formative writes one of the special Vv forms here, and "+
				l.Vv+" is not one that carries a Function")
			return g.DefaultCrRoot(l.Cr), ca()
		}
		degree, ctx, ok := parse.ParseAffixVr(l.Vr)
		if !ok {
			fs.add("Vr", l.Vr, "no affix degree and Context pair is written "+l.Vr)
		}
		return g.CsRoot{
			Cs:       l.Cr,
			Degree:   degree,
			Version:  sv.Version,
			Function: *sv.Function,
			Context:  ctx,
		}, ca()
	case RefRootFormative:
		sv, ok := parse.ParseSpecialVv(l.Vv)
		if !ok {
			fs.add("Vv", l.Vv, "a §4.6.4 personal-reference root writes one of the special Vv forms here, and "+
				l.Vv+" is not one")
		}
		slotIV, ok := parse.ParseSlotIV(l.Vr)
		if !ok {
			slotIV = g.DefaultSlotIV
		}
		refs, ok := parse.DecomposeRefCluster(l.Cr)
		if !ok || len(refs) == 0 {
			fs.add("Cr", l.Cr, l.Cr+" is not a chain of referent forms, so it cannot be a §4.6.4 personal-reference root")
		}
		return g.RefRoot{
			Refs:    refs,
			Version: sv.Version,
			SlotIV:  slotIV,
		}, ca()
	}
	fs.add("Cr", l.Cr, fmt.Sprintf("unknown root kind %d", l.Kind))
	return g.DefaultCrRoot(l.Cr), g.DefaultSlotVI
}

// affixesVxCs decodes a list of AffixChunks (each carrying a raw Vx
// vowel and Cs consonant) into grammar Affixes. A Vx outside the §3.5
// table is a parse failure, not a degree-0 affix.
func affixesVxCs(fs *faults, name string, chunks []AffixChunk) []g.Affix {
	if len(chunks) == 0 {
		return nil
	}
	out := make([]g.Affix, 0, len(chunks))
	for i, c := range chunks {
		slot := subscriptSlot(name, i+1)
		// §3.5: "No C_S form can contain a glottal-stop." §1.7 Rule 1
		// says where one between a vowel and a consonant really belongs:
		// after the vowel-form, so it is the V_X in front that carries
		// it. A segmentation that hands it to the C_S has gone wrong
		// earlier, and saying so beats building the affix, because the
		// renderer cannot place the glottal, drops it, and the affix
		// comes back a degree off in a different slot.
		if strings.Contains(c.Cs, "'") {
			fs.add(subscriptSlot("Cs", i+1), c.Cs,
				"§3.5 admits no glottal stop in an affix Cs; the Vx in front of it carries one")
			continue
		}
		t, d, ok := parse.AffixVowelDegree(c.Vx)
		if !ok {
			fs.add(slot, c.Vx, "no affix Type and degree is written "+c.Vx+
				"; a Vx spells one of the nine degrees in one of three Types")
			continue
		}
		// A Column-4 vowel is §4.6.5's Transrelative-case shortcut only
		// when its Cs is a referential form. §4.6.5 bars a referential
		// affix from taking the Abstract Perspective increments -w and
		// -y precisely so that it cannot be confused with a §3.9.2
		// case-accessor, whose fourteen Cs increments all end in one.
		// A Column-4 vowel on anything else is therefore an accessor,
		// which decodes to a case rather than a referent.
		if t == g.Column4Affix {
			_, isRef := parse.DecomposeRefAffixCs(c.Cs)
			_, _, isAccessor := g.ParseAccessorCs(c.Cs)
			if !isRef && !isAccessor {
				fs.add(subscriptSlot("Cs", i+1), c.Cs, "the Column-4 vowel "+c.Vx+
					" in front of this needs either a referential form (§4.6.5) or a case-accessor increment (§3.9.2), and "+
					c.Cs+" is neither")
				continue
			}
		}
		out = append(out, g.Affix{Type: t, Degree: d, Consonant: c.Cs})
	}
	return out
}

// restoreMovedGlottal re-inserts the glottal stop that the §3.9.1
// SPECIAL NOTE shortening rule shifted off the Vc, in the §1.7 Rule 3
// placement that a word-final Vc always takes.
func restoreMovedGlottal(vc string) string { return phonology.GlottalizeVowel(vc) }

// finalFromVc builds the Final variant from the trailing Slot IX
// vowel (may be empty) and the observed stress.
// The stress decides which table Slot IX is read against, so a fault
// here names the reading that was attempted: telling someone their Vc
// is not a case, without saying that penultimate stress is why a case
// was wanted, leaves out the half they can act on.
func finalFromVc(fs *faults, vc string, stress phonology.Stress, concat g.ConcatenationStatus) g.Final {
	if concat != g.ConcatNone {
		return formatFromVf(fs, vc, stress)
	}
	// caseFinal reads Vc as a case, which is what every stress but
	// ultimate wants, and falls back to THM so the remaining slots are
	// still judged.
	caseFinal := func() g.Case {
		if vc == "" {
			return g.THM
		}
		cs, ok := parse.ParseCase(vc)
		if !ok {
			fs.add("Vc", vc, "no case is written "+vc+", and "+stress.String()+
				" stress reads Slot IX as a case")
			return g.THM
		}
		return cs
	}
	switch stress {
	case phonology.Ultimate, phonology.Monosyllabic:
		if vc == "" {
			return g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
		}
		vk, ok := parse.ParseVk(vc)
		if !ok {
			fs.add("Vk", vc, "no illocution and validation pair is written "+vc+
				", and ultimate stress reads Slot IX as a Vk")
			vk = g.Assertive{Validation: g.OBS}
		}
		return g.UnframedVerbal{Vk: vk}
	case phonology.Antepenultimate:
		return g.FramedVerbal{Case: caseFinal()}
	case phonology.Penultimate:
		return g.UnframedNominal{Case: caseFinal()}
	}
	fs.add("Vc", vc, fmt.Sprintf("stress %v has no Slot IX reading", stress))
	return g.UnframedNominal{Case: g.THM}
}

// formatFromVf decodes the Slot IX vowel of a concatenated formative.
//
// §3.1.3: a dependent's Slot IX is a V_F Format, never a V_K, so
// stress does not choose between a nominal and a verbal reading here.
// It chooses the case group instead. The glottal stop that marks cases
// 37-68 elsewhere is not written on a dependent, because Slot I
// already spends a glottal on the no-concatenation C_C; ultimate
// stress stands in for it, and the vowel is the plain 1-36 form of the
// case 36 places below. Reinstating the glottal per §1.7 is exactly
// what restoreMovedGlottal does, so the promotion is a lookup, not a
// second table.
//
// A dependent is therefore always an UnframedNominal. Antepenultimate
// stress has no reading at all: §3.1.3 gives ultimate one job already,
// and the spec never frames a dependent.
func formatFromVf(fs *faults, vc string, stress phonology.Stress) g.Final {
	switch stress {
	case phonology.Ultimate:
		// §3.1.3: PRN, like THM, may elide its -a-, but only on a
		// polysyllable, so that the stress it depends on is audible.
		if vc == "" {
			return g.UnframedNominal{Case: g.PRN}
		}
		c, ok := parse.ParseCase(restoreMovedGlottal(vc))
		if !ok {
			fs.add("Vf", vc, "no case 37-68 is written "+vc+
				"; ultimate stress on a dependent reads Slot IX as one of those")
			c = g.THM
		}
		return g.UnframedNominal{Case: c}
	case phonology.Penultimate, phonology.Monosyllabic:
		// §3.1.3: a monosyllabic dependent is an unframed nominal in
		// THM, not the verbal reading a monosyllable would get anywhere
		// else.
		c := g.THM
		if vc != "" {
			cs, ok := parse.ParseCase(vc)
			switch {
			case !ok:
				fs.add("Vf", vc, "no case is written "+vc)
			// §3.1.6: cases 37-68 are spelled without their glottal on a
			// dependent, so a glottal here is not a higher case, it is a
			// word that does not parse.
			case cs > g.SIT:
				fs.add("Vf", vc, "this is case "+cs.String()+
					"; a dependent writes cases 37-68 without their glottal, under ultimate stress")
			default:
				c = cs
			}
		}
		return g.UnframedNominal{Case: c}
	}
	fs.add("Vf", vc, fmt.Sprintf("§3.1.3 gives a dependent only penultimate or ultimate stress, not %v", stress))
	return g.UnframedNominal{Case: g.THM}
}

// FromGrammar converts a grammar.Formative into a Layout — Layer D
// inverse. Three of the encoding choices are optional shortenings that
// the spec permits rather than requires (see encoding), so instead of
// deciding each one greedily we lay the word out every legal way and
// keep the best by romanizationCost. Default-value elisions run within each
// candidate. There is no option to request a particular spelling —
// non-canonical romanizations exist only as input to the parser.
func FromGrammar(f g.Formative) Layout {
	if f.Root == nil {
		panic("slots: Formative.Root is nil")
	}
	if f.Final == nil {
		panic("slots: Formative.Final is nil")
	}
	best := layoutFor(f, encoding{})
	bestCost := romanizationCost(best, encoding{})
	for _, e := range allEncodings[1:] {
		l := layoutFor(f, e)
		if c := romanizationCost(l, e); c.better(bestCost) {
			best, bestCost = l, c
		}
	}
	return best
}

// encoding selects which of the optional shortenings to attempt. Each
// is permissive in the spec ("may"), so switching one off always
// yields a legal romanization; switching one on has no effect unless the
// formative also meets that shortcut's own conditions.
type encoding struct {
	ccShortcut bool // §3.2 Slot IV/VI a+Ca shortcut, Cc = w-/y-
	cnInCa     bool // §3.8.1.2 Mood/Case-Scope Cn moved into the Ca slot
	vcGlottal  bool // §3.9.1 Vc glottal-stop moved earlier in the word
}

// allEncodings is every combination, plainest first so it wins ties.
var allEncodings = func() []encoding {
	var out []encoding
	for _, cc := range []bool{false, true} {
		for _, cn := range []bool{false, true} {
			for _, vc := range []bool{false, true} {
				out = append(out, encoding{ccShortcut: cc, cnInCa: cn, vcGlottal: vc})
			}
		}
	}
	return out
}()

func (e encoding) count() int {
	n := 0
	for _, b := range []bool{e.ccShortcut, e.cnInCa, e.vcGlottal} {
		if b {
			n++
		}
	}
	return n
}

// cost ranks candidate encodings of one Formative. §3.2 justifies the
// shortcuts as "shortening the formative by one syllable", so syllable
// count leads. A shortcut that buys no syllable but forces a glottal
// stop (§3.6.2 marks the end of Slot V that way once the Ca is gone)
// is a loss, hence glottals next. Length breaks the remaining ties.
// Past that, a shortcut that has bought nothing on any of those three
// measures is pure overhead for reader and writer alike, so the plain
// spelling wins. Romanization is the final backstop, so the choice is
// always deterministic.
type cost struct {
	syllables int
	glottals  int
	runes     int
	shortcuts int
	rom       string
}

func (c cost) better(than cost) bool {
	if c.syllables != than.syllables {
		return c.syllables < than.syllables
	}
	if c.glottals != than.glottals {
		return c.glottals < than.glottals
	}
	if c.runes != than.runes {
		return c.runes < than.runes
	}
	if c.shortcuts != than.shortcuts {
		return c.shortcuts < than.shortcuts
	}
	return c.rom < than.rom
}

func romanizationCost(l Layout, e encoding) cost {
	s := Render(l)
	c := cost{runes: len([]rune(s)), shortcuts: e.count(), rom: s}
	for _, conj := range phonology.SplitConjuncts(s) {
		if phonology.IsVowelConjunct(conj) {
			c.syllables++
		}
	}
	c.glottals = strings.Count(s, "'")
	return c
}

// layoutFor builds the Layout for one choice of optional shortenings.
func layoutFor(f g.Formative, e encoding) Layout {
	l := Layout{}

	useShortcut := e.ccShortcut && canUseShortcut(f)
	l.Cc = ccFromGrammar(f.Concat, useShortcut, f)

	switch r := f.Root.(type) {
	case g.CrRoot:
		l.Kind = CrFormative
		l.Cr = r.Cluster
		if useShortcut {
			series := shortcutSeries(f.SlotVI)
			l.Vv = parse.SlotIIToVvSeries(g.SlotII{Stem: r.Stem, Version: r.Version}, series)
			// Vr, Ca elided
		} else {
			l.Vv = parse.SlotIIToVv(g.SlotII{Stem: r.Stem, Version: r.Version})
			l.Vr = parse.SlotIVToVr(r.SlotIV)
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
		l.Cr = parse.RefCluster(r.Refs)
		l.Vv = refRootVv(r.Version)
		l.Vr = parse.SlotIVToVr(r.SlotIV)
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

	if f.SlotVIII != nil && !g.SlotVIIIIsDefault(f.SlotVIII) {
		l.Vn, l.Cn = vnCnFromSlotVIII(f.SlotVIII)
	}

	l.Vc, l.Stress = slotIXFromFinal(f)

	// §3.8.1.2 shortcut is decided before default-value elision so the
	// shortcut's freed-up syllable isn't claimed by elision first — that
	// ordering let elision burn enough slack to keep the shortcut's
	// minimum-syllables guard from firing, and the long form leaked out.
	if e.cnInCa {
		maybeMoveCnToCa(&l, f)
	}
	if e.vcGlottal {
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
//   - When a Slot IV/VI a+Ca shortcut is in play. In that romanization the
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

// canUseShortcut reports whether the formative's grammar permits a
// Cc-shortcut romanization: a CrRoot with default SlotIV and a SlotVI
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
	if shortcutSeries(f.SlotVI) == 0 {
		return false
	}
	// Cc shortcut compresses Slot IV/VI into the Cc-Vv pair, giving a
	// minimal body of Vv-Cr-Vc — two syllables. Final categories that
	// need three (FramedVerbal's antepenultimate stress) can't fit
	// the diacritic and must take the long form.
	if _, framed := f.Final.(g.FramedVerbal); framed {
		return false
	}
	return true
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
func ccFromGrammar(concat g.ConcatenationStatus, useShortcut bool, f g.Formative) string {
	if useShortcut {
		variant := shortcutVariant(f.SlotVI)
		if concat == g.ConcatNone {
			switch variant {
			case parse.ShortcutW:
				return "w"
			case parse.ShortcutY:
				return "y"
			}
		}
		switch concat {
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
	switch concat {
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
// VnCnFromSlotVIII returns the written Vn vowel and Cn consonant for a
// SlotVIII variant. Exposes the inverse of parse.ParseVnCn so callers
// outside this package can encode typed SlotVIII values into the raw
// pair stored on grammar.ModularAdjunct.
func VnCnFromSlotVIII(s g.SlotVIII) (string, string) {
	return vnCnFromSlotVIII(s)
}

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

// The Pattern-2 FAC value is printed "w/y", with no rule anywhere in
// the document for choosing between them, and none derivable from
// usage: arţtuläwá and erčuläyá differ only in the glide, with the same
// Ca, Vn and Vk. The official examples split 51 w to 12 y over 63
// instances and the community corpus 415 to 30 over 445; both appear
// after every plain vowel and several diphthongs, and -ou- inverts the
// ratio outright at 12 y to 7 w. So nothing phonological conditions it
// and the minority form is too common to be a slip.
//
// We render w, the majority form, and parse both. That is our choice,
// not Quijada's: parse/slot_viii.go accepts y wherever it accepts w.
// See ERRATA.md §3.8.
var moodCnP2Table = [...]string{"w", "hw", "hrw", "hmw", "hnw", "hňw"}

func moodCnP1(m g.Mood) string { return moodCnP1Table[m] }
func moodCnP2(m g.Mood) string { return moodCnP2Table[m] }

// slotIXFromFinal picks the trailing vowel and the stress diacritic to
// apply based on the formative's grammatical category. The inverse of
// finalFromVc.
func slotIXFromFinal(f g.Formative) (string, phonology.Stress) {
	if f.Concat != g.ConcatNone {
		n, ok := f.Final.(g.UnframedNominal)
		if !ok {
			panic(fmt.Sprintf("slots: concatenated formative with %T final; §3.1.3 gives a dependent a Vf Format, so it is always nominal", f.Final))
		}
		if n.Case > g.SIT {
			return stripVfGlottal(parse.CaseToVc(n.Case)), phonology.Ultimate
		}
		return parse.CaseToVc(n.Case), phonology.Penultimate
	}
	switch v := f.Final.(type) {
	case g.UnframedNominal:
		return parse.CaseToVc(v.Case), phonology.Penultimate
	case g.FramedVerbal:
		return parse.CaseToVc(v.Case), phonology.Antepenultimate
	case g.UnframedVerbal:
		return vkVowel(v.Vk), phonology.Ultimate
	}
	return "", phonology.Penultimate
}

// stripVfGlottal is the inverse of restoreMovedGlottal: it takes the
// canonical Vc of a case in the 37-68 range and returns the vowel a
// concatenated formative writes instead (§3.1.6).
func stripVfGlottal(vc string) string {
	rs := []rune(vc)
	if len(rs) == 3 && rs[0] == rs[2] {
		return string(rs[0])
	}
	return strings.Replace(vc, "'", "", 1)
}

// vkVowel renders a Vk variant as its written vowel.
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
	canVc := (canElideTrailingTHMVc(l, f) || canElideMonosyllabicVerbalVc(l, f)) &&
		validWordFinalAfterVcElision(l)
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
// Only the Assertive/OBS combination is eligible — its written vowel
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
	// §3.10 grants implicit ultimate stress only to *monosyllabic*
	// words. Predict the post-elision vowel count: Vc elision drops
	// one, and a co-firing leading-Vv elision drops another. The
	// result must be exactly one vowel for §3.10 to apply. A
	// multi-syllabic body that dropped Vc would resolve as
	// penultimate stress, not assertive/OBS.
	target := vowelCount(l) - 1
	if canElideLeadingVv(l, f) {
		target--
	}
	return target == 1
}

func canElideLeadingVv(l *Layout, f g.Formative) bool {
	if f.Concat != g.ConcatNone {
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
	// §3.5.1 requires a glottal-stop in Vv whenever Slot V holds two or
	// more affixes, to say so before the listener reaches the consonant
	// runs and has to guess whether they are Cs forms or the Ca. An
	// elided Vv has nowhere to put it, so the slot has to stay.
	//
	// Our own parser copes without the marker — it resolves at the
	// geminated Ca — so nothing round-trips differently and no test
	// noticed. It is still a form §3.5.1 forbids.
	if len(f.SlotV) >= 2 {
		return false
	}
	// Dropping Vv puts Cr at the start of the word, where §3.1/§3.2
	// permit a narrower set of clusters than medial position does.
	// "amlala" can lose its Vv because m- takes a following liquid;
	// "ardvilëilḑá" cannot, because word-initial r- takes only -w or -y.
	if !phonology.WordInitialLegal(l.Cr) {
		return false
	}
	// A root that starts with ç- or cs- can't go word-initial either:
	// stripSentencePrefix reads those as the §1.3.2 sentence-juncture
	// marker and eats them, so "açmuliwá" would re-parse as root -m-.
	if strings.HasPrefix(l.Cr, "ç") || strings.HasPrefix(l.Cr, "cs") {
		return false
	}
	return strings.HasPrefix(l.Vv, "a") && l.Vv == "a"
}

// validWordFinalAfterVcElision reports whether dropping Vc would leave
// a legal word. The conjunct in front of Vc lands at the end of the
// word, where §4.1-§4.5 permit a narrower set than medial position
// does: "hňw" is a well-formed Cn, but §4.1 lets no word end in -w.
// §1.4 names the remedy directly, which is to fill Slot IX instead.
//
// This is the mirror of the word-initial check in canElideLeadingVv.
func validWordFinalAfterVcElision(l *Layout) bool {
	// Which conjunct ends up last depends on the whole layout: a Slot
	// IV/VI shortcut leaves Ca empty and puts the root there, §3.6.2
	// leaves it empty and puts a Slot V affix's Cs there, and Cn or a
	// Slot VII Cs takes it otherwise. Rather than restate that order
	// and let it drift, render the layout without its Vc and look at
	// what actually came last.
	probe := *l
	probe.Vc = ""
	conjs := phonology.SplitConjuncts(Render(probe))
	if len(conjs) == 0 {
		return true
	}
	last := conjs[len(conjs)-1]
	if r, _ := utf8.DecodeRuneInString(last); phonology.IsVowel(r) {
		return true
	}
	return phonology.ClusterLegalAt(phonology.Final, last)
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
	// §3.1.3 lets PRN elide its -a- as well as THM, on a concatenated
	// formative. Both are written "a" there — PRN drops its glottal per
	// §3.1.6 — so the two are told apart by the ultimate stress that
	// stays behind, which is why the elision is barred on a monosyllable
	// and why requiredSyllables keeps two vowels in the word.
	if c != g.THM && !(c == g.PRN && f.Concat != g.ConcatNone) {
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
