package render

import (
	"strings"

	g "github.com/coudard/ithkuil/go/grammar"
)

// renderShortcut emits a formative in Slot I Cc-shortcut form. The
// shortcut elides Slot IV (Vr defaults to STA/BSC/EXS) and Slot VI
// (Ca), with the Ca encoded compositionally as a series-shift on the
// Slot II Vv vowel.
//
// Structure: Cc + Vv + Cr + (Slot VII affixes) + (Slot VIII) + (Slot IX).
//
// The Cc consonant combines concatenation status (Type1/Type2 or none)
// with the W/Y shortcut indicator into one of 8 prefixes.
func renderShortcut(f g.Formative) string {
	sc := *f.SlotIShortcut
	cc := shortcutCc(f.SlotI, sc)
	series, ok := slotVIToShortcutSeries(sc, f.SlotVI)
	if !ok {
		// Slot VI doesn't match any of the four encodable values for
		// this shortcut. Fall back to series 1; the round-trip will
		// disagree on SlotVI in this case.
		series = 1
	}
	vv := g.SlotIIToVvSeries(f.SlotII, series)

	var b strings.Builder
	b.WriteString(cc)
	b.WriteString(vv)
	b.WriteString(string(f.SlotIII))
	b.WriteString(SlotVII(f.SlotVII))
	b.WriteString(SlotVIII(f.SlotVIII))
	b.WriteString(SlotIX(f.Final))
	body := b.String()
	if canElideTrailingTHMVc(f, body) &&
		countVowelConjuncts(body)-1 >= requiredSyllables(f.Final) {
		body = body[:len(body)-1]
	}
	return applyFinalStress(body, f.Final)
}

// shortcutCc encodes Slot I as one of the eight Cc consonants. Standalone
// shortcut forms use bare w-/y-; combinations with Type1/Type2 use
// hl-/hm-/hr-/hn-.
func shortcutCc(concat *g.ConcatenationStatus, sc g.CcShortcut) string {
	if concat == nil {
		if sc == g.ShortcutW {
			return "w"
		}
		return "y"
	}
	switch *concat {
	case g.Type1:
		if sc == g.ShortcutW {
			return "hl"
		}
		return "hm"
	case g.Type2:
		if sc == g.ShortcutW {
			return "hr"
		}
		return "hn"
	}
	return ""
}

// slotVIToShortcutSeries is the inverse of parse.ShortcutCa: given the
// Slot VI value carried by a shortcut, returns the Vv series (1-4) that
// encodes it. ok is false if the Slot VI value isn't one of the four
// shortcut-encodable variants.
func slotVIToShortcutSeries(sc g.CcShortcut, vi g.SlotVI) (int, bool) {
	switch sc {
	case g.ShortcutW:
		switch vi {
		case g.DefaultSlotVI:
			return 1, true
		case (g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}):
			return 2, true
		case (g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.N_, Extension: g.DEL, Essence: g.NRM}):
			return 3, true
		case (g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.RPV}):
			return 4, true
		}
	case g.ShortcutY:
		switch vi {
		case (g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}):
			return 1, true
		case (g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.RPV}):
			return 2, true
		case (g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.A_, Extension: g.DEL, Essence: g.NRM}):
			return 3, true
		case (g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.RPV}):
			return 4, true
		}
	}
	return 0, false
}
