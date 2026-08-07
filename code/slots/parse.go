package slots

import (
	"fmt"
	"strings"
	"unicode/utf8"

	"github.com/christian-oudard/ithkuil/allomorph"
	"github.com/christian-oudard/ithkuil/fault"
	"github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
)

// Parse decodes a romanized Ithkuil word into a Layout, reading it as
// phonology first. It is ParseWord composed with phonology.ParseWord,
// for callers holding raw text; one that has parsed the word already
// calls ParseWord instead of paying for the pass twice.
//
// A §3.1.7 chain is several words joined by a hyphen, so it is not a
// formative and phonology.ParseWord turns it away.
func Parse(word string) (Layout, error) {
	w, err := phonology.ParseWord(word)
	if err != nil {
		return Layout{}, err
	}
	return ParseWord(w)
}

// ParseWord assigns a word's conjuncts to slots. It classifies, and
// nothing else: normalization, the phonotactic rules, the stress
// reading, and the conjunct split all happened in phonology.ParseWord
// and arrive here as the Word. No grammar value decoding happens here
// either — see ToGrammar for that.
func ParseWord(w phonology.Word) (Layout, error) {
	word := w.String()
	stress := w.Stress()
	if stress == phonology.InvalidStress {
		return Layout{}, fault.One(word, shape("stress", word, "a word carries at most one stress mark"))
	}
	// A sentence-initial word may carry a prefix that is no part of its
	// slot structure; the conjuncts are re-split when it is dropped.
	conjs := w.Conjuncts()
	if body := stripSentencePrefix(w.Bare()); body != w.Bare() {
		conjs = phonology.MergeGlottalVowels(phonology.SplitConjuncts(body))
	}
	if len(conjs) < 3 {
		return Layout{}, fault.One(word, shape("shape", word, fmt.Sprintf(
			"a formative needs at least a Cr, a Vr and a Ca; this has %s",
			fault.Plural(len(conjs), "conjunct"))))
	}
	l := Layout{
		Stress: stress,
	}
	i := 0

	// Slot I: optional Cc consonant carrying concat status and/or
	// shortcut indicator.
	if phonology.IsConsonantConjunct(conjs[0]) {
		if r := parse.ParseCc(conjs[0]); r.Concat != grammar.ConcatNone || r.Shortcut != parse.ShortcutNone {
			l.Cc = conjs[0]
			i++
		}
	}

	// §3.9.1 moved-glottal stripping runs only outside the shortcut
	// form. In shortcut form the same "'C" written pattern is the
	// §3.6.2 Slot V end-of-slot marker, not a moved Vc glottal — the
	// §3.6.2 footnote makes the two mutually exclusive.
	//
	// It also runs only where there is a case to move a glottal off.
	// §3.9.1 shortens V_C for cases 37-52; an ultimate-stress formative
	// carries V_K instead, and V_K has no glottalized forms, so any
	// glottal in one of those is marking something else.
	if !isShortcutCc(l.Cc) && stress != phonology.Ultimate && stress != phonology.Monosyllabic {
		var movedGlottal bool
		conjs, movedGlottal = stripMovedGlottal(conjs, i)
		l.MovedGlottal = movedGlottal
	}

	// Identify the shape by what's at position i.
	switch {
	case i < len(conjs) && phonology.IsVowelConjunct(conjs[i]):
		// Vowel-initial: Vv-Cr-Vr-…-Ca-… (possibly special-Vv).
		if err := parseVowelInitial(&l, conjs, i); err != nil {
			return Layout{}, shapeErr(word, err)
		}
	default:
		// Consonant-initial: Cr-Vr-Ca-… (no Vv).
		if l.Cc != "" {
			return Layout{}, fault.One(word, shape("Cc", l.Cc,
				"a Slot I prefix needs a Vv after it, and this word begins with a consonant cluster"))
		}
		if err := parseConsonantInitial(&l, conjs, i); err != nil {
			return Layout{}, shapeErr(word, err)
		}
	}
	return l, nil
}

// stripSentencePrefix discards the optional sentence-juncture marker
// at the start of a word. Per §1.3.2, the ç(ë)- prefix (and its
// §5.8.8 cs-/cse-/csw-/cscs- equivalents) is purely prosodic and
// "normally never written"; when present in input it carries no
// grammatical information, so we just drop it. csw / cscs rewrite
// to w / y so the shortcut Cc remains visible to downstream parsing.
func stripSentencePrefix(word string) string {
	if word == "" {
		return word
	}
	// ç-family.
	r, sz := utf8.DecodeRuneInString(word)
	if r == 'ç' {
		rest := word[sz:]
		if rest == "" {
			return word
		}
		r2, sz2 := utf8.DecodeRuneInString(rest)
		if r2 == 'ë' && rest[sz2:] != "" {
			return rest[sz2:]
		}
		if r2 == 'ç' {
			return "y" + rest[sz2:]
		}
		return rest
	}
	// cs-family. "cs" must be a two-byte prefix; we also handle "cse",
	// "csw" (cs + w-shortcut Cc), and "cscs" (cs + y-shortcut Cc).
	if strings.HasPrefix(word, "cscs") {
		// cs + y, with the y written as a doubled cs.
		rest := word[len("cscs"):]
		if rest == "" {
			return word
		}
		return "y" + rest
	}
	if strings.HasPrefix(word, "csw") {
		rest := word[len("cs"):] // keep the w as a normal Cc
		if rest == "" {
			return word
		}
		return rest
	}
	if strings.HasPrefix(word, "cse") {
		rest := word[len("cse"):]
		if rest == "" {
			return word
		}
		return rest
	}
	if strings.HasPrefix(word, "cs") {
		rest := word[len("cs"):]
		if rest == "" {
			return word
		}
		// Bare "cs-" only legal before a vowel; otherwise we shouldn't
		// have matched (the "cse-" branch above handles the consonant
		// case explicitly).
		r2, _ := utf8.DecodeRuneInString(rest)
		if !phonology.IsVowel(r2) {
			return word
		}
		return rest
	}
	return word
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
			return shape("Vv", vv, "the Slot I shortcut "+l.Cc+" takes a plain Vv, not a special one")
		}
		l.Vv = vv
		i++
		if i >= len(conjs) {
			return shape("Cr", "", "the special Vv "+vv+" needs a Cs or C1 cluster after it")
		}
		l.Cr = conjs[i]
		i++
		if i >= len(conjs) {
			return shape("Vr", "", "a special Vv needs a Vr after the root")
		}
		l.Vr = conjs[i]
		i++
		if parse.IsRefRootVv(vv) {
			l.Kind = RefRootFormative
		} else {
			l.Kind = CsRootFormative
		}
		// §4.2: a specialized C_S-root "operates like a standard
		// formative except that Slots II and IV take specialized V_V
		// and V_R forms and the Slot III C_R form is replaced by the
		// C_S-form of a V_X C_S affix". Slot V is not among the
		// exceptions, so the §3.6.1 geminated Ca means here what it
		// means anywhere else.
		return parseFromCa(l, conjs, i, true)
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
		return shape("Cr", "", "a Vv needs a root cluster after it")
	}
	l.Cr = conjs[i]
	i++

	// Shortcut form: no Vr, no Ca. Slot V may still appear per §3.6.2,
	// in which case the final Slot V Vx carries a glottal-stop end-of-
	// slot marker. After Slot V (or immediately, if absent), continue
	// with Slot VII / VIII / IX.
	if isShortcutCc(l.Cc) {
		newConjs, newI, err := parseShortcutSlotV(l, conjs, i)
		if err != nil {
			return err
		}
		if hadGlottalVv && len(l.SlotV) < 2 {
			return shape("Vv", vv, fmt.Sprintf(
				"a §3.5.1 glottal stop on Vv marks two or more Slot V affixes; this has %d", len(l.SlotV)))
		}
		return parseAfterCa(l, newConjs, newI)
	}

	// Regular long form: Vr next.
	if i >= len(conjs) {
		return shape("Vr", "", "the root cluster needs a Vr after it")
	}
	if !phonology.IsVowelConjunct(conjs[i]) {
		return shape("Vr", conjs[i], "the root cluster needs a vowel after it, and "+conjs[i]+" is a consonant cluster")
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
		return shape("Vr", "", "the root cluster needs a Vr after it")
	}
	if !phonology.IsVowelConjunct(conjs[i]) {
		return shape("Vr", conjs[i], "the root cluster needs a vowel after it, and "+conjs[i]+" is a consonant cluster")
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
		return shape("Ca", "", "a formative needs a Ca complex after Vr")
	}

	// Try to find a geminated Ca that signals Slot V.
	if allowSlotV {
		geminatedAt := -1
		var bareCa string
		for j := i; j < len(conjs); j += 2 {
			if !phonology.IsConsonantConjunct(conjs[j]) {
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
					return shape("Vx", "", "every Slot V affix Cs needs its Vx vowel")
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
	if !phonology.IsConsonantConjunct(conjs[i]) {
		return shape("Ca", conjs[i], "the Ca slot holds a consonant cluster, and "+conjs[i]+" is a vowel")
	}
	// §3.8.1.2 shortcut: a Pattern-1 Cn cluster in the Ca slot
	// (hl/hr/hm/hn/hň, but not the FAC/CCN "h") means Vn=MNO and
	// Ca=default-l were both elided. The cluster is the Cn affix.
	if isMovedCn(conjs[i]) {
		l.Ca = "l"
		l.Vn = "a"
		l.Cn = conjs[i]
		l.CnInCa = true
		return parseAfterCa(l, conjs, i+1)
	}
	l.Ca = conjs[i]
	return parseAfterCa(l, conjs, i+1)
}

// parseShortcutSlotV walks (Vx, Cs) pairs starting at i, looking for
// the §3.6.2 end-of-Slot-V glottal-stop. In shortcut form Slot V is
// written-ordered Vx-Cs (unlike the normal form, where it is reversed
// to Cs-Vx because the geminated Ca handles the boundary). The end of
// Slot V is signalled by a glottal infixed into the final Vx; that
// glottal romanizations as a leading "'" on the next consonant conjunct
// (since SplitConjuncts groups "'" with the consonant that follows).
//
// Returns the (possibly-modified) conjunct slice and the new index to
// resume parsing from. If no glottal-marked Cs is found within a
// contiguous run of (Vx, Cs) pairs, no Slot V is recorded and the
// caller picks up at the original position — the shortcut form
// simply had no Slot V.
func parseShortcutSlotV(l *Layout, conjs []string, i int) ([]string, int, error) {
	var collected []AffixChunk
	j := i
	for j+1 < len(conjs) {
		vx, cs := conjs[j], conjs[j+1]
		if !phonology.IsVowelConjunct(vx) || !phonology.IsConsonantConjunct(cs) {
			break
		}
		// §3.6.2 marks the end of Slot V with a glottal-stop infixed
		// into the final Vx. §1.7 gives it two landing spots: after the
		// vowel-form, where SplitConjuncts hands it to us on the front
		// of the following Cs ("ëu" + "'ţř"), or inside it when the
		// first placement won't do ("ë'u", and "a'a" for a single
		// vowel). Both mean the same thing.
		if strings.HasPrefix(cs, "'") {
			// A bare "'" conjunct is a word-final glottal: the marker
			// is there but the Cs it should precede is not. An affix
			// is its Cs, so there is no affix to record.
			if cs == "'" {
				return nil, 0, shape("Cs", "", "the §3.6.2 end-marker glottal after Vx "+vx+" needs a Cs after it")
			}
			collected = append(collected, AffixChunk{Vx: vx, Cs: cs[1:]})
			l.SlotV = append(l.SlotV, collected...)
			out := make([]string, 0, len(conjs))
			out = append(out, conjs[:j]...)
			out = append(out, conjs[j+2:]...)
			return out, j, nil
		}
		if stripped := stripVvGlottal(vx); stripped != vx {
			collected = append(collected, AffixChunk{Vx: stripped, Cs: cs})
			l.SlotV = append(l.SlotV, collected...)
			out := make([]string, 0, len(conjs))
			out = append(out, conjs[:j]...)
			out = append(out, conjs[j+2:]...)
			return out, j, nil
		}
		collected = append(collected, AffixChunk{Vx: vx, Cs: cs})
		j += 2
	}
	// No glottal-marked Cs found → not a Slot V context after all.
	return conjs, i, nil
}

// isMovedCn reports whether c is a Pattern-1 Cn consonant cluster
// that has been moved into the Ca slot per §3.8.1.2. FAC/CCN ("h") is
// excluded: it elides instead of being moved.
func isMovedCn(c string) bool {
	switch c {
	case "hl", "hr", "hm", "hn", "hň":
		return true
	}
	return false
}

// parseAfterCa decodes the conjuncts that follow Ca: zero-or-more
// Slot VII (Vx, Cs) pairs, optional Slot VIII (Vn, Cn), optional
// Slot IX (Vc or Vk).
func parseAfterCa(l *Layout, conjs []string, i int) error {
	type vcPair struct{ v, c string }
	var pairs []vcPair
	for i+1 < len(conjs) {
		v, c := conjs[i], conjs[i+1]
		if !phonology.IsVowelConjunct(v) || !phonology.IsConsonantConjunct(c) {
			break
		}
		pairs = append(pairs, vcPair{v: v, c: c})
		i += 2
	}
	trailing := conjs[i:]
	if len(trailing) > 1 {
		return shape("shape", strings.Join(trailing, ""),
			"nothing follows Slot IX, and these conjuncts sit after it")
	}
	if len(trailing) == 1 {
		if !phonology.IsVowelConjunct(trailing[0]) {
			return shape("Vc", trailing[0], "Slot IX holds a vowel, and "+trailing[0]+" is a consonant cluster")
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
// form (V_R / V_X / V_N). On the romanization this manifests as a
// consonant conjunct whose first rune is "'" (e.g., "la'la" splits as
// [l, a, 'l, a]). When the preceding conjunct is a vowel we strip the
// leading "'" and flag the layout so ToGrammar reads the Vc with the
// glottal re-attached.
func stripMovedGlottal(conjs []string, vvIdx int) ([]string, bool) {
	// The unmoved Vc glottal lives in the last vowel conjunct, so only
	// an earlier one can be a §3.9.1 shortening.
	lastVowel := -1
	for i, c := range conjs {
		if phonology.IsVowelConjunct(c) {
			lastVowel = i
		}
	}
	// §3.9.1 may move the glottal onto any vocalic form *after Slot II*,
	// so a glottal on the Vv is never this one — it is the §3.5.1 marker
	// for two or more Slot V affixes. Skip both spellings of it: inside
	// the Vv itself ("i'i"), and on the head of the following conjunct
	// ("a'rt"), which is where §1.7's first placement puts it.
	skipA, skipB := -1, -1
	if vvIdx >= 0 && vvIdx < len(conjs) && phonology.IsVowelConjunct(conjs[vvIdx]) {
		skipA, skipB = vvIdx, vvIdx+1
	}
	moved := false
	out := make([]string, 0, len(conjs))
	for i, c := range conjs {
		switch {
		case moved || i == skipA || i == skipB:
		case i > 0 && phonology.IsVowelConjunct(conjs[i-1]) && len(c) > 1 && c[0] == '\'':
			// Glottal at the head of a consonant conjunct ("la'la").
			moved = true
			out = append(out, c[1:])
			continue
		case i < lastVowel && phonology.IsVowelConjunct(c) && strings.Contains(c, "'"):
			// Glottal intervocalic within a vowel conjunct, which is
			// how MergeGlottalVowels leaves a Vr-borne one ("přa'ölua").
			moved = true
			out = append(out, stripVvGlottal(c))
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
