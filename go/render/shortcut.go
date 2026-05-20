package render

import (
	"strings"

	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/parse"
)

// canUseShortcut reports whether the formative is shortcut-encodable.
// The shortcut form requires: Root is a CrRoot with default SlotIV,
// SlotVI is one of the eight encodable values, and no Slot V affixes
// (which require Ca gemination incompatible with shortcut form).
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
	if _, _, ok := slotVIToShortcut(f.SlotVI); !ok {
		return false
	}
	return true
}

// renderShortcut emits a formative in Cc-Vv shortcut form. Slot IV
// defaults to STA/BSC/EXS (elided); Slot VI is encoded compositionally
// as a series-shift on the Slot II Vv vowel.
//
// Structure: Cc + Vv + Cr + (Slot VII) + (Slot VIII) + (Slot IX).
func renderShortcut(f g.Formative) string {
	cr := f.Root.(g.CrRoot)
	variant, series, _ := slotVIToShortcut(f.SlotVI)
	cc := shortcutCc(f.Concat, variant)
	vv := g.SlotIIToVvSeries(g.SlotII{Stem: cr.Stem, Version: cr.Version}, series)

	var b strings.Builder
	b.WriteString(cc)
	b.WriteString(vv)
	b.WriteString(cr.Cluster)
	b.WriteString(SlotVII(f.SlotVII))
	b.WriteString(SlotVIII(f.SlotVIII))
	b.WriteString(SlotIX(f.Final))
	body := b.String()
	if canElideTrailingTHMVc(f, body) &&
		countVowelConjuncts(body)-1 >= requiredSyllables(f.Final) {
		body = body[:len(body)-1]
	}
	body = applyFinalStress(body, f.Final)
	if f.SentenceStarter {
		return sentencePrefix(body)
	}
	return body
}

// shortcutVariant is the render-internal W/Y indicator for shortcut
// form. The grammar doesn't store this — both W and Y shortcut forms
// describe the same grammar; the choice is determined by SlotVI's
// place in the shortcut tables.
type shortcutVariant int

const (
	scW shortcutVariant = iota
	scY
)

// shortcutCc encodes Concat + shortcut variant into one of the four
// shortcut Cc consonants. Standalone shortcut forms use bare w/y;
// combinations with Type1/Type2 use hl/hm/hr/hn.
func shortcutCc(concat *g.ConcatenationStatus, sc shortcutVariant) string {
	if concat == nil {
		if sc == scW {
			return "w"
		}
		return "y"
	}
	switch *concat {
	case g.Type1:
		if sc == scW {
			return "hl"
		}
		return "hm"
	case g.Type2:
		if sc == scW {
			return "hr"
		}
		return "hn"
	}
	return ""
}

// slotVIToShortcut is the inverse of parse.ShortcutCa. Given a SlotVI,
// it returns the (variant, series) pair that encodes it, or ok=false
// if the SlotVI isn't one of the eight shortcut-encodable values.
func slotVIToShortcut(vi g.SlotVI) (shortcutVariant, int, bool) {
	for _, variant := range []parse.ShortcutVariant{parse.ShortcutW, parse.ShortcutY} {
		for series := 1; series <= 4; series++ {
			if parse.ShortcutCa(variant, series) == vi {
				sc := scW
				if variant == parse.ShortcutY {
					sc = scY
				}
				return sc, series, true
			}
		}
	}
	return 0, 0, false
}
