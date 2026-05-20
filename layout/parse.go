package layout

import (
	"fmt"
	"unicode/utf8"

	"github.com/christian-oudard/ithkuil/allomorph"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/surface"
)

// Parse decodes a surface Ithkuil word into a Layout. It runs Layers A
// (Strip), B (SplitConjuncts), and C (slot-by-slot classification) in
// one pass, but does no grammar value decoding — see ToGrammar for that.
func Parse(word string) (Layout, error) {
	if word == "" {
		return Layout{}, fmt.Errorf("layout: empty word")
	}
	bare, stress := surface.Strip(word)
	body, sentenceStarter := stripSentencePrefix(bare)
	conjs := surface.MergeGlottalVowels(surface.SplitConjuncts(body))
	conjs, movedGlottal := stripMovedGlottal(conjs)
	if len(conjs) < 3 {
		return Layout{}, fmt.Errorf("word %q too short (got %d conjuncts, need at least 3)", word, len(conjs))
	}
	l := Layout{
		SentenceStarter: sentenceStarter,
		MovedGlottal:    movedGlottal,
		Stress:          stress,
	}
	i := 0

	// Slot I: optional Cc consonant carrying concat status and/or
	// shortcut indicator.
	if surface.IsConsonantConjunct(conjs[0]) {
		if r := parse.ParseCc(conjs[0]); r.Concat != nil || r.Shortcut != parse.ShortcutNone {
			l.Cc = conjs[0]
			i++
		}
	}

	// Identify the shape by what's at position i.
	switch {
	case i < len(conjs) && surface.IsVowelConjunct(conjs[i]):
		// Vowel-initial: Vv-Cr-Vr-…-Ca-… (possibly special-Vv).
		if err := parseVowelInitial(&l, conjs, i); err != nil {
			return Layout{}, fmt.Errorf("%v (word %q)", err, word)
		}
	default:
		// Consonant-initial: Cr-Vr-Ca-… (no Vv).
		if l.Cc != "" {
			return Layout{}, fmt.Errorf("Slot I prefix with consonant-initial body not supported (word %q)", word)
		}
		if err := parseConsonantInitial(&l, conjs, i); err != nil {
			return Layout{}, fmt.Errorf("%v (word %q)", err, word)
		}
	}
	return l, nil
}

// stripSentencePrefix mirrors fullparse.stripSentencePrefix, returning
// the body without the leading ç marker and whether one was present.
func stripSentencePrefix(word string) (string, bool) {
	if word == "" {
		return word, false
	}
	r, sz := utf8.DecodeRuneInString(word)
	if r != 'ç' {
		return word, false
	}
	rest := word[sz:]
	if rest == "" {
		return word, false
	}
	r2, sz2 := utf8.DecodeRuneInString(rest)
	if r2 == 'ë' && rest[sz2:] != "" {
		return rest[sz2:], true
	}
	if r2 == 'ç' {
		return "y" + rest[sz2:], true
	}
	return rest, true
}

// parseVowelInitial handles Vv-Cr-… (with optional special-Vv) and the
// shortcut form (Cc-Vv-Cr-… with Vr elided).
func parseVowelInitial(l *Layout, conjs []string, i int) error {
	vv := conjs[i]

	// Special Vv selects an alternate root kind. Shortcut forms can
	// never combine with special-Vv (they encode SlotVI in the
	// Cc+Vv pair, which is mutually exclusive with the Cs/Ref-root
	// Vv markers).
	if parse.IsSpecialVv(vv) {
		if isShortcutCc(l.Cc) {
			return fmt.Errorf("shortcut Cc %q cannot combine with special Vv %q", l.Cc, vv)
		}
		l.Vv = vv
		i++
		if i >= len(conjs) {
			return fmt.Errorf("special Vv %q with no Cs/C1", vv)
		}
		l.Cr = conjs[i]
		i++
		if i >= len(conjs) {
			return fmt.Errorf("special Vv: missing Vr")
		}
		l.Vr = conjs[i]
		i++
		if parse.IsRefRootVv(vv) {
			l.Kind = RefRootFormative
		} else {
			l.Kind = CsRootFormative
		}
		return parseFromCa(l, conjs, i, false)
	}

	// Standard or shortcut Cr formative.
	l.Kind = CrFormative

	// Detect the §3.5.1 Vv glottal-stop (signals Slot V ≥ 2). We
	// store the un-glottalized vowel; the SlotV count alone tells
	// the renderer to re-insert it.
	hadGlottalVv := false
	if stripped := stripVvGlottal(vv); stripped != vv {
		hadGlottalVv = true
		l.Vv = stripped
	} else {
		l.Vv = vv
	}
	i++

	if i >= len(conjs) {
		return fmt.Errorf("missing Cr after Vv")
	}
	l.Cr = conjs[i]
	i++

	// Shortcut form: no Vr, no Ca, jump to Slot VII/VIII/IX.
	if isShortcutCc(l.Cc) {
		if hadGlottalVv {
			return fmt.Errorf("shortcut form cannot carry a Slot V glottal-stop in Vv")
		}
		return parseAfterCa(l, conjs, i)
	}

	// Regular long form: Vr next.
	if i >= len(conjs) {
		return fmt.Errorf("missing Vr after Cr")
	}
	if !surface.IsVowelConjunct(conjs[i]) {
		return fmt.Errorf("expected Vr vowel after Cr, got %q", conjs[i])
	}
	l.Vr = conjs[i]
	i++

	return parseFromCa(l, conjs, i, true)
}

// parseConsonantInitial handles the Cr-Vr-Ca-… shape with Vv elided.
func parseConsonantInitial(l *Layout, conjs []string, i int) error {
	l.Kind = CrFormative
	l.Cr = conjs[i]
	i++
	if i >= len(conjs) {
		return fmt.Errorf("missing Vr after Cr")
	}
	if !surface.IsVowelConjunct(conjs[i]) {
		return fmt.Errorf("expected Vr vowel after Cr, got %q", conjs[i])
	}
	l.Vr = conjs[i]
	i++
	return parseFromCa(l, conjs, i, true)
}

// parseFromCa scans from index i looking for the Ca cluster. If
// allowSlotV is true, intervening (Cs, Vx) pairs before a geminated Ca
// are recorded as Slot V affixes; the geminated Ca is replaced with its
// bare form. After Ca, parseAfterCa handles the rest.
func parseFromCa(l *Layout, conjs []string, i int, allowSlotV bool) error {
	if i >= len(conjs) {
		return fmt.Errorf("missing Ca cluster")
	}

	// Try to find a geminated Ca that signals Slot V.
	if allowSlotV {
		geminatedAt := -1
		var bareCa string
		for j := i; j < len(conjs); j += 2 {
			if !surface.IsConsonantConjunct(conjs[j]) {
				break
			}
			if bare, ok := allomorph.CaUngeminate[conjs[j]]; ok {
				geminatedAt = j
				bareCa = bare
				break
			}
		}
		if geminatedAt > i {
			// Slot V is the (Cs, Vx) pairs between i and geminatedAt.
			for j := i; j < geminatedAt; j += 2 {
				if j+1 >= geminatedAt {
					return fmt.Errorf("Slot V missing Vx at conjunct %d", j)
				}
				l.SlotV = append(l.SlotV, AffixChunk{
					Cs: conjs[j],
					Vx: conjs[j+1],
				})
			}
			l.Ca = bareCa
			return parseAfterCa(l, conjs, geminatedAt+1)
		}
	}

	// No Slot V — the conjunct at i is the bare Ca.
	if !surface.IsConsonantConjunct(conjs[i]) {
		return fmt.Errorf("expected Ca consonant cluster, got %q", conjs[i])
	}
	l.Ca = conjs[i]
	return parseAfterCa(l, conjs, i+1)
}

// parseAfterCa decodes the conjuncts that follow Ca: zero-or-more
// Slot VII (Vx, Cs) pairs, optional Slot VIII (Vn, Cn), optional
// Slot IX (Vc or Vk).
func parseAfterCa(l *Layout, conjs []string, i int) error {
	type vcPair struct{ v, c string }
	var pairs []vcPair
	for i+1 < len(conjs) {
		v, c := conjs[i], conjs[i+1]
		if !surface.IsVowelConjunct(v) || !surface.IsConsonantConjunct(c) {
			break
		}
		pairs = append(pairs, vcPair{v: v, c: c})
		i += 2
	}
	trailing := conjs[i:]
	if len(trailing) > 1 {
		return fmt.Errorf("unexpected trailing conjuncts after Ca: %v", trailing)
	}
	if len(trailing) == 1 {
		if !surface.IsVowelConjunct(trailing[0]) {
			return fmt.Errorf("expected trailing vowel for Slot IX, got %q", trailing[0])
		}
		l.Vc = trailing[0]
	}

	// The last (v, c) pair might be Slot VIII (VnCn). Slot VIII Cn
	// is restricted to a specific set of consonants; if the last
	// pair's c matches, peel it off.
	if n := len(pairs); n > 0 && parse.IsValidCn(pairs[n-1].c) {
		l.Vn = pairs[n-1].v
		l.Cn = pairs[n-1].c
		pairs = pairs[:n-1]
	}
	for _, p := range pairs {
		l.SlotVII = append(l.SlotVII, AffixChunk{Vx: p.v, Cs: p.c})
	}
	return nil
}

// stripMovedGlottal handles the §3.9.1 SPECIAL NOTE rule: the V_C
// glottal-stop for cases 37-52 may be shifted to any earlier vocalic
// form (V_R / V_X / V_N). On the surface this manifests as a
// consonant conjunct whose first rune is "'" (e.g., "la'la" splits as
// [l, a, 'l, a]). When the preceding conjunct is a vowel we strip the
// leading "'" and flag the layout so ToGrammar reads the Vc with the
// glottal re-attached.
func stripMovedGlottal(conjs []string) ([]string, bool) {
	moved := false
	out := make([]string, 0, len(conjs))
	for i, c := range conjs {
		if !moved && i > 0 && surface.IsVowelConjunct(conjs[i-1]) &&
			len(c) > 1 && c[0] == '\'' {
			moved = true
			out = append(out, c[1:])
			continue
		}
		out = append(out, c)
	}
	return out, moved
}

// stripVvGlottal removes a §3.5.1 glottal-stop from a Vv vowel. The
// returned value equals v when no glottal is present.
func stripVvGlottal(v string) string {
	rs := []rune(v)
	for i, r := range rs {
		if r != '\'' {
			continue
		}
		before := rs[:i]
		after := rs[i+1:]
		if len(before) == 1 && len(after) == 1 && before[0] == after[0] {
			return string(before[0])
		}
		return string(before) + string(after)
	}
	return v
}

// isShortcutCc reports whether a Cc consonant indicates the shortcut
// form (the variants that elide Vr and encode Ca via Cc+Vv).
func isShortcutCc(cc string) bool {
	switch cc {
	case "w", "y", "hl", "hm", "hr", "hn":
		return true
	}
	return false
}
