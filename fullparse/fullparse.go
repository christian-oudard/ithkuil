// Package fullparse turns a surface Ithkuil word into a grammar.Formative
// by sequencing the per-slot parsers from package parse.
package fullparse

import (
	"fmt"
	"unicode/utf8"

	"github.com/christian-oudard/ithkuil/allomorph"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/surface"
)

// stripSentencePrefix removes a leading ç marker if present.
//
//	çë…   → strip both (çë acts as ç + default-Vv "ë")
//	çç…   → strip ç, replace next ç with "y" (sentence prefix + y shortcut)
//	ç…    → strip ç only
//
// Returns the stripped word and whether the prefix was present.
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

// ParseFormative decodes a single surface Ithkuil word into a Formative.
// Returns a descriptive error if the word doesn't match a recognized
// formative shape.
func ParseFormative(word string) (g.Formative, error) {
	word, hasSentencePrefix := stripSentencePrefix(word)
	conjs := parse.SplitConjuncts(word)
	_, surfStress := surface.Strip(word)
	stress := parse.Stress(surfStress) // surface.Stress and parse.Stress share an iota layout
	conjs = parse.MergeGlottalVowels(conjs)

	if len(conjs) < 3 {
		return g.Formative{}, fmt.Errorf("word %q too short for formative (got %d conjuncts, need at least 3)", word, len(conjs))
	}

	// Slot I: parse Cc consonant if present. Cc can carry concat
	// status, a Ca shortcut indicator, or both.
	var concat *g.ConcatenationStatus
	shortcut := parse.ShortcutNone
	if parse.IsConsonantConjunct(conjs[0]) {
		r := parse.ParseCc(conjs[0])
		if r.Concat != nil || r.Shortcut != parse.ShortcutNone {
			concat = r.Concat
			shortcut = r.Shortcut
			conjs = conjs[1:]
		}
	}

	if len(conjs) < 2 {
		return g.Formative{}, fmt.Errorf("word %q too short after Slot I (got %d conjuncts)", word, len(conjs))
	}

	if shortcut != parse.ShortcutNone {
		f, err := parseShortcutFormative(conjs, shortcut, concat, stress)
		if err != nil {
			return g.Formative{}, fmt.Errorf("%v (word %q)", err, word)
		}
		f.SentenceStarter = hasSentencePrefix
		return f, nil
	}

	if len(conjs) < 3 {
		return g.Formative{}, fmt.Errorf("word %q too short after Slot I (got %d conjuncts)", word, len(conjs))
	}

	if parse.IsVowelConjunct(conjs[0]) {
		f, err := parseVowelInitial(conjs, concat, stress)
		if err != nil {
			return g.Formative{}, fmt.Errorf("%v (word %q)", err, word)
		}
		f.SentenceStarter = hasSentencePrefix
		return f, nil
	}

	if concat != nil {
		return g.Formative{}, fmt.Errorf("Slot I prefix with consonant-initial body not supported (word %q)", word)
	}
	f, err := parseConsonantInitial(conjs, stress)
	if err != nil {
		return g.Formative{}, fmt.Errorf("%v (word %q)", err, word)
	}
	f.SentenceStarter = hasSentencePrefix
	return f, nil
}

// parseShortcutFormative handles the Vv-Cr-… shape after a Cc shortcut.
// SlotVI is resolved from (variant, series); Slot IV defaults to
// STA/BSC/EXS. The grammar output is a regular CrRoot — the shortcut
// is just a surface encoding choice that the parser resolves away.
func parseShortcutFormative(conjs []string, sc parse.ShortcutVariant, concat *g.ConcatenationStatus, stress parse.Stress) (g.Formative, error) {
	if len(conjs) < 2 {
		return g.Formative{}, fmt.Errorf("shortcut formative needs Vv+Cr, got %d conjuncts", len(conjs))
	}
	if !parse.IsVowelConjunct(conjs[0]) {
		return g.Formative{}, fmt.Errorf("shortcut formative: expected Vv vowel, got %q", conjs[0])
	}
	slotII, ok := parse.ParseSlotII(conjs[0])
	if !ok {
		return g.Formative{}, fmt.Errorf("invalid Vv %q in shortcut", conjs[0])
	}
	series := parse.VvSeries(conjs[0])
	slotVI := parse.ShortcutCa(sc, series)

	slotVII, slotVIII, final, err := parseAfterCa(conjs[2:], stress)
	if err != nil {
		return g.Formative{}, err
	}
	return g.Formative{
		Concat: concat,
		Root: g.CrRoot{
			Cluster: conjs[1],
			Stem:    slotII.Stem,
			Version: slotII.Version,
			SlotIV:  g.DefaultSlotIV,
		},
		SlotVI:   slotVI,
		SlotVII:  slotVII,
		SlotVIII: slotVIII,
		Final:    final,
	}, nil
}

// parseVowelInitial handles the canonical Vv-Cr-Vr-Ca-... structure.
// Special Vv markers (ëi/eë/ëu/oë/ae/ea) route to a Cs- or RefRoot.
func parseVowelInitial(conjs []string, concat *g.ConcatenationStatus, stress parse.Stress) (g.Formative, error) {
	if len(conjs) < 4 {
		return g.Formative{}, fmt.Errorf("vowel-initial formative needs at least 4 conjuncts, got %d", len(conjs))
	}
	if parse.IsSpecialVv(conjs[0]) {
		return parseSpecialVvFormative(conjs, concat, stress)
	}
	slotII, ok := parse.ParseSlotII(stripVvGlottal(conjs[0]))
	if !ok {
		return g.Formative{}, fmt.Errorf("invalid Vv %q", conjs[0])
	}
	slotIV, ok := parse.ParseSlotIV(conjs[2])
	if !ok {
		return g.Formative{}, fmt.Errorf("invalid Vr %q", conjs[2])
	}
	slotVI, slotV, afterCa, err := parseSlotVAndCa(conjs, 3)
	if err != nil {
		return g.Formative{}, err
	}
	slotVII, slotVIII, final, err := parseAfterCa(conjs[afterCa:], stress)
	if err != nil {
		return g.Formative{}, err
	}
	return g.Formative{
		Concat: concat,
		Root: g.CrRoot{
			Cluster: conjs[1],
			Stem:    slotII.Stem,
			Version: slotII.Version,
			SlotIV:  slotIV,
		},
		SlotV:    slotV,
		SlotVI:   slotVI,
		SlotVII:  slotVII,
		SlotVIII: slotVIII,
		Final:    final,
	}, nil
}

// stripVvGlottal removes a §3.5.1 glottal-stop from Vv.
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

// parseSlotVAndCa decodes Slot V (if any) and Slot VI starting at
// startIdx in conjs. Slot V's presence is signaled by the gemination
// of the Slot VI Ca per §3.6.1.
func parseSlotVAndCa(conjs []string, startIdx int) (g.SlotVI, []g.Affix, int, error) {
	geminatedAt := -1
	var slotVIFromGem g.SlotVI
	for i := startIdx; i < len(conjs); i += 2 {
		if !parse.IsConsonantConjunct(conjs[i]) {
			break
		}
		if vi, ok := allomorph.ParseGeminatedCa(conjs[i]); ok {
			geminatedAt = i
			slotVIFromGem = vi
			break
		}
	}
	if geminatedAt == -1 || geminatedAt == startIdx {
		slotVI, ok := allomorph.ParseCa(conjs[startIdx])
		if !ok {
			return g.SlotVI{}, nil, 0, fmt.Errorf("unrecognized Ca %q", conjs[startIdx])
		}
		return slotVI, nil, startIdx + 1, nil
	}
	var slotV []g.Affix
	for i := startIdx; i < geminatedAt; i += 2 {
		if i+1 >= geminatedAt {
			return g.SlotVI{}, nil, 0, fmt.Errorf("Slot V missing Vx at conj %d", i)
		}
		cs := conjs[i]
		vx := conjs[i+1]
		t, d := parse.ClassifyAffixVowel(vx)
		slotV = append(slotV, g.Affix{Type: t, Degree: d, Consonant: cs})
	}
	return slotVIFromGem, slotV, geminatedAt + 1, nil
}

// parseSpecialVvFormative handles Vv markers that select an alternate
// formative shape (Cs-root or referential).
func parseSpecialVvFormative(conjs []string, concat *g.ConcatenationStatus, stress parse.Stress) (g.Formative, error) {
	sv, ok := parse.ParseSpecialVv(conjs[0])
	if !ok {
		return g.Formative{}, fmt.Errorf("invalid special Vv %q", conjs[0])
	}
	slotVI, ok := allomorph.ParseCa(conjs[3])
	if !ok {
		return g.Formative{}, fmt.Errorf("unrecognized Ca %q", conjs[3])
	}
	slotVII, slotVIII, final, err := parseAfterCa(conjs[4:], stress)
	if err != nil {
		return g.Formative{}, err
	}

	var root g.Root
	if sv.Function != nil {
		// Cs-root: Vr is (degree, Context); Specification is implicitly BSC.
		degree, ctx, ok := parse.ParseAffixVr(conjs[2])
		if !ok {
			return g.Formative{}, fmt.Errorf("invalid Cs-root Vr %q", conjs[2])
		}
		root = g.CsRoot{
			Cs:       conjs[1],
			Degree:   degree,
			Version:  sv.Version,
			Function: *sv.Function,
			Context:  ctx,
		}
	} else {
		// Reference-root: Vr is normal SlotIV; Function defaults to STA.
		slotIV, ok := parse.ParseSlotIV(conjs[2])
		if !ok {
			slotIV = g.DefaultSlotIV
		}
		root = g.RefRoot{
			C1:      conjs[1],
			Version: sv.Version,
			SlotIV:  slotIV,
		}
	}

	return g.Formative{
		Concat:   concat,
		Root:     root,
		SlotVI:   slotVI,
		SlotVII:  slotVII,
		SlotVIII: slotVIII,
		Final:    final,
	}, nil
}

// parseConsonantInitial handles Cr-Vr-Ca-... where Vv is elided. Slot
// II defaults to (S1, PRC), Slot IV is parsed normally.
func parseConsonantInitial(conjs []string, stress parse.Stress) (g.Formative, error) {
	if len(conjs) < 3 {
		return g.Formative{}, fmt.Errorf("consonant-initial formative needs at least 3 conjuncts, got %d", len(conjs))
	}
	slotIV, ok := parse.ParseSlotIV(conjs[1])
	if !ok {
		return g.Formative{}, fmt.Errorf("invalid Vr %q", conjs[1])
	}
	slotVI, ok := allomorph.ParseCa(conjs[2])
	if !ok {
		return g.Formative{}, fmt.Errorf("unrecognized Ca %q", conjs[2])
	}
	slotVII, slotVIII, final, err := parseAfterCa(conjs[3:], stress)
	if err != nil {
		return g.Formative{}, err
	}
	return g.Formative{
		Root: g.CrRoot{
			Cluster: conjs[0],
			Stem:    g.S1,
			Version: g.PRC,
			SlotIV:  slotIV,
		},
		SlotVI:   slotVI,
		SlotVII:  slotVII,
		SlotVIII: slotVIII,
		Final:    final,
	}, nil
}

// parseAfterCa decodes Slot VII, optional Slot VIII, and Final.
func parseAfterCa(tail []string, stress parse.Stress) ([]g.Affix, g.SlotVIII, g.Final, error) {
	type vcPair struct{ v, c string }
	var pairs []vcPair
	i := 0
	for i+1 < len(tail) {
		v, c := tail[i], tail[i+1]
		if !parse.IsVowelConjunct(v) || !parse.IsConsonantConjunct(c) {
			break
		}
		pairs = append(pairs, vcPair{v: v, c: c})
		i += 2
	}
	trailing := tail[i:]
	if len(trailing) > 1 {
		return nil, nil, nil, fmt.Errorf("unexpected trailing conjuncts after Ca: %v", trailing)
	}

	trailingV := ""
	if len(trailing) == 1 {
		if !parse.IsVowelConjunct(trailing[0]) {
			return nil, nil, nil, fmt.Errorf("expected vowel for Slot IX, got %q", trailing[0])
		}
		trailingV = trailing[0]
	}
	final, ok := parseFinal(trailingV, stress)
	if !ok {
		return nil, nil, nil, fmt.Errorf("invalid Slot IX %q for stress %v", trailingV, stress)
	}

	var slotVIII g.SlotVIII
	if n := len(pairs); n > 0 && parse.IsValidCn(pairs[n-1].c) {
		if s8, ok := parse.ParseVnCn(pairs[n-1].v, pairs[n-1].c); ok {
			slotVIII = s8
			pairs = pairs[:n-1]
		}
	}

	var slotVII []g.Affix
	for _, p := range pairs {
		t, d := parse.ClassifyAffixVowel(p.v)
		slotVII = append(slotVII, g.Affix{Type: t, Degree: d, Consonant: p.c})
	}

	return slotVII, slotVIII, final, nil
}

// parseFinal builds Final from observed stress + trailing vowel.
func parseFinal(vowel string, stress parse.Stress) (g.Final, bool) {
	switch stress {
	case parse.Ultimate, parse.Monosyllabic:
		if vowel == "" {
			return g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}, true
		}
		vk, ok := parse.ParseVk(vowel)
		if !ok {
			return nil, false
		}
		return g.UnframedVerbal{Vk: vk}, true
	case parse.Antepenultimate:
		c := g.THM
		if vowel != "" {
			cs, ok := parse.ParseCase(vowel)
			if !ok {
				return nil, false
			}
			c = cs
		}
		return g.FramedVerbal{Case: c}, true
	case parse.Penultimate:
		c := g.THM
		if vowel != "" {
			cs, ok := parse.ParseCase(vowel)
			if !ok {
				return nil, false
			}
			c = cs
		}
		return g.UnframedNominal{Case: c}, true
	}
	return nil, false
}
