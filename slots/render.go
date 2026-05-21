package slots

import (
	"strings"
	"unicode/utf8"

	"github.com/christian-oudard/ithkuil/allomorph"
	"github.com/christian-oudard/ithkuil/surface"
)

// Render produces the surface word that this Layout describes. It is
// the inverse of Parse — round-tripping (`Render(Parse(w))`) returns
// the original w, after default-elision decisions have been baked
// into the Layout.
//
// Render handles the mechanical surface-form details:
//   - re-inserts the §3.5.1 Vv glottal-stop when len(SlotV) ≥ 2
//   - re-applies §3.6.1 Ca gemination when len(SlotV) ≥ 1
//   - reverses Slot V to its Cs+Vx surface order
//   - applies the stress diacritic via surface.Apply
//   - prepends the sentence-start ç prefix when requested
func Render(l Layout) string {
	var b strings.Builder
	b.WriteString(l.Cc)
	b.WriteString(applyVvGlottal(l.Vv, len(l.SlotV)))
	b.WriteString(l.Cr)
	b.WriteString(l.Vr)
	// Slot V surface order: §3.5 reverses to Cs-Vx when Ca is present
	// (the geminated Ca handles the boundary). In shortcut form (Ca
	// elided), §3.5's NOTE keeps the standard Vx-Cs order and §3.6.2
	// adds a glottal-stop end marker on the final Vx.
	shortcutSlotV := l.Ca == "" && len(l.SlotV) > 0
	for k, a := range l.SlotV {
		isLast := k == len(l.SlotV)-1
		if shortcutSlotV {
			// End-marker glottal goes between the final Vx and its Cs.
			// The Cs prevents the glottal from landing word-final, so
			// no §1.7 rule 3 epenthesis is needed.
			b.WriteString(a.Vx)
			if isLast {
				b.WriteString("'")
			}
			b.WriteString(a.Cs)
		} else {
			b.WriteString(a.Cs)
			b.WriteString(a.Vx)
		}
	}
	ca := l.Ca
	if len(l.SlotV) > 0 {
		ca = allomorph.GeminateCa(ca)
	}
	// §3.8.1.2 shortcut: the Pattern-1 Cn cluster takes the Ca slot
	// and Vn is elided. Layer C populated Ca/Vn/Cn with defaults so
	// Layer D could decode normally; here we collapse them back.
	if l.CnInCa {
		b.WriteString(l.Cn)
	} else {
		b.WriteString(ca)
	}
	for _, a := range l.SlotVII {
		b.WriteString(a.Vx)
		b.WriteString(a.Cs)
	}
	if !l.CnInCa {
		b.WriteString(l.Vn)
		b.WriteString(l.Cn)
	}
	b.WriteString(l.Vc)

	body := surface.Apply(b.String(), l.Stress)
	if l.SentenceStarter {
		return sentencePrefix(body)
	}
	return body
}

// applyVvGlottal re-inserts the §3.5.1 glottal-stop into Vv when the
// formative has 2+ Slot V affixes.
func applyVvGlottal(vv string, slotVLen int) string {
	if slotVLen < 2 || vv == "" {
		return vv
	}
	rs := []rune(vv)
	if len(rs) == 1 {
		return string(rs[0]) + "'" + string(rs[0])
	}
	if len(rs) == 2 {
		return string(rs[0]) + "'" + string(rs[1])
	}
	return vv + "'"
}

// sentencePrefix prepends the §3.13 sentence-start marker.
func sentencePrefix(body string) string {
	if body == "" {
		return body
	}
	first, _ := utf8.DecodeRuneInString(body)
	if surface.IsVowel(first) {
		return "ç" + body
	}
	return "çë" + body
}
