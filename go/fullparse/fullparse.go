// Package fullparse turns a surface Ithkuil word into a grammar.Formative
// by sequencing the per-slot parsers from package parse.
//
// Currently supported shapes:
//   - Vowel-initial:                 Vv-Cr-Vr-Ca-(VxCs...)-(VnCn)-(Vc/Vk)
//   - Slot I prefix ("h"/"hw"):      h-Vv-Cr-Vr-Ca-...
//   - Consonant-initial (no Cc):     Cr-Vr-Ca-... (Vv elided to S1/PRC)
//
// Still to come: Slot I shortcut forms (w/y/hl/hm/hr/hn) and Slot V
// (CsVx stem affixes, which require Vv-special-marker disambiguation).
package fullparse

import (
	"fmt"
	"unicode/utf8"

	"github.com/coudard/ithkuil/go/allomorph"
	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/parse"
)

// stripSentencePrefix removes a leading ç marker if present. Three
// cases follow the Haskell reference:
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
	stress := parse.DetectStress(word)
	// Merge glottalized case vowels (i'a, a'a, …) into single conjuncts
	// so the case-vowel lookup table sees them whole.
	conjs = parse.MergeGlottalVowels(conjs)

	if len(conjs) < 3 {
		return g.Formative{}, fmt.Errorf("word %q too short for formative (got %d conjuncts, need at least 3)", word, len(conjs))
	}

	// Slot I (Cc): the first conjunct can carry concatenation status,
	// a Ca shortcut, or both. Six combinations recognized so far:
	//   h  → Type1, no shortcut
	//   hw → Type2, no shortcut
	//   hl → Type1 + ShortcutW
	//   hm → Type1 + ShortcutY
	//   hr → Type2 + ShortcutW
	//   hn → Type2 + ShortcutY
	//   w  → ShortcutW alone
	//   y  → ShortcutY alone
	var slotI *g.ConcatenationStatus
	var shortcut *g.CcShortcut
	if parse.IsConsonantConjunct(conjs[0]) {
		stripped := true
		switch conjs[0] {
		case "h":
			t := g.Type1
			slotI = &t
		case "hw":
			t := g.Type2
			slotI = &t
		case "hl":
			t := g.Type1
			s := g.ShortcutW
			slotI = &t
			shortcut = &s
		case "hm":
			t := g.Type1
			s := g.ShortcutY
			slotI = &t
			shortcut = &s
		case "hr":
			t := g.Type2
			s := g.ShortcutW
			slotI = &t
			shortcut = &s
		case "hn":
			t := g.Type2
			s := g.ShortcutY
			slotI = &t
			shortcut = &s
		case "w":
			s := g.ShortcutW
			shortcut = &s
		case "y":
			s := g.ShortcutY
			shortcut = &s
		default:
			stripped = false
		}
		if stripped {
			conjs = conjs[1:]
		}
	}

	if len(conjs) < 2 {
		return g.Formative{}, fmt.Errorf("word %q too short after Slot I (got %d conjuncts)", word, len(conjs))
	}

	if shortcut != nil {
		// Shortcut path: Vv-Cr-(affixes/Vc). No Vr conjunct, no Ca
		// conjunct — both are filled from the shortcut + Vv series.
		f, err := parseShortcutFormative(conjs, *shortcut, stress)
		if err != nil {
			return g.Formative{}, fmt.Errorf("%v (word %q)", err, word)
		}
		f.SlotI = slotI
		f.SlotIShortcut = shortcut
		f.SentenceStarter = hasSentencePrefix
		return f, nil
	}

	if len(conjs) < 3 {
		return g.Formative{}, fmt.Errorf("word %q too short after Slot I (got %d conjuncts)", word, len(conjs))
	}

	if parse.IsVowelConjunct(conjs[0]) {
		f, err := parseVowelInitial(conjs, stress)
		if err != nil {
			return g.Formative{}, fmt.Errorf("%v (word %q)", err, word)
		}
		f.SlotI = slotI
		f.SentenceStarter = hasSentencePrefix
		return f, nil
	}

	// Consonant-initial: Vv elided to default S1/PRC.
	if slotI != nil {
		return g.Formative{}, fmt.Errorf("Slot I prefix with consonant-initial body not supported (word %q)", word)
	}
	f, err := parseConsonantInitial(conjs, stress)
	if err != nil {
		return g.Formative{}, fmt.Errorf("%v (word %q)", err, word)
	}
	f.SentenceStarter = hasSentencePrefix
	return f, nil
}

// parseShortcutFormative handles the Vv-Cr-… shape that follows a
// shortcut Cc. The shortcut elides Vr (defaults to STA/BSC/EXS) and
// supplies SlotVI from a fixed table indexed by the Vv series.
func parseShortcutFormative(conjs []string, sc g.CcShortcut, stress parse.Stress) (g.Formative, error) {
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
	root := g.Root(conjs[1])

	slotVII, slotVIII, final, err := parseAfterCa(conjs[2:], stress)
	if err != nil {
		return g.Formative{}, err
	}
	return g.Formative{
		SlotII:   slotII,
		SlotIII:  root,
		SlotIV:   g.DefaultSlotIV,
		SlotVI:   slotVI,
		SlotVII:  slotVII,
		SlotVIII: slotVIII,
		Final:    final,
	}, nil
}

// parseVowelInitial handles the canonical Vv-Cr-Vr-Ca-... structure.
// conjs must start with a vowel and have at least 4 entries.
// Special Vv markers (ëi/eë/ëu/oë/ae/ea) route to parseSpecialVvFormative.
func parseVowelInitial(conjs []string, stress parse.Stress) (g.Formative, error) {
	if len(conjs) < 4 {
		return g.Formative{}, fmt.Errorf("vowel-initial formative needs at least 4 conjuncts, got %d", len(conjs))
	}
	if parse.IsSpecialVv(conjs[0]) {
		return parseSpecialVvFormative(conjs, stress)
	}
	slotII, ok := parse.ParseSlotII(stripVvGlottal(conjs[0]))
	if !ok {
		return g.Formative{}, fmt.Errorf("invalid Vv %q", conjs[0])
	}
	root := g.Root(conjs[1])
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
		SlotII:   slotII,
		SlotIII:  root,
		SlotIV:   slotIV,
		SlotV:    slotV,
		SlotVI:   slotVI,
		SlotVII:  slotVII,
		SlotVIII: slotVIII,
		Final:    final,
	}, nil
}

// stripVvGlottal removes a §3.5.1 glottal-stop from Vv. For single
// vowels the glottal is inserted between a reduplicated pair (V → V'V),
// so we collapse back to one vowel. For diphthongs it sits between the
// two members (V1V2 → V1'V2), so we just drop the glottal.
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
//
// Returns the SlotVI, the Slot V affix list (nil if absent), and the
// index just past the Ca conjunct so the caller can continue parsing.
func parseSlotVAndCa(conjs []string, startIdx int) (g.SlotVI, []g.Affix, int, error) {
	// Scan odd-offset positions (consonants) for the first cluster
	// that matches a geminated Ca. Cs forms cannot contain geminates
	// (§3.6.1 note), so the first geminated match identifies Slot VI.
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
		// No Slot V — conjs[startIdx] is the plain (un-geminated) Ca.
		// (A geminated Ca at startIdx itself would mean Slot V is
		// empty but Ca is geminated, which the spec doesn't allow.)
		slotVI, ok := allomorph.ParseCa(conjs[startIdx])
		if !ok {
			return g.SlotVI{}, nil, 0, fmt.Errorf("unrecognized Ca %q", conjs[startIdx])
		}
		return slotVI, nil, startIdx + 1, nil
	}
	// Decode Slot V Cs-Vx pairs between Vr and the geminated Ca.
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
// formative shape:
//
//   - Cs-root (ëi/eë/ëu/oë): Vv encodes (Version, Function); the Cr
//     position holds an affix Cs; the Vr decodes as (degree, Context)
//     via parse.ParseAffixVr; Specification defaults to BSC; the
//     CsRootDegree field records the degree.
//   - Reference-root (ae/ea): Vv encodes Version (Function unset);
//     the Cr position holds a referential C1; Vr decodes normally.
//
// In both cases SlotII.Stem is S1.
func parseSpecialVvFormative(conjs []string, stress parse.Stress) (g.Formative, error) {
	sv, ok := parse.ParseSpecialVv(conjs[0])
	if !ok {
		return g.Formative{}, fmt.Errorf("invalid special Vv %q", conjs[0])
	}
	cr := g.Root(conjs[1])
	slotVI, ok := allomorph.ParseCa(conjs[3])
	if !ok {
		return g.Formative{}, fmt.Errorf("unrecognized Ca %q", conjs[3])
	}
	slotVII, slotVIII, final, err := parseAfterCa(conjs[4:], stress)
	if err != nil {
		return g.Formative{}, err
	}

	f := g.Formative{
		SlotII:   g.SlotII{Stem: g.S1, Version: sv.Version},
		SlotIII:  cr,
		SlotVI:   slotVI,
		SlotVII:  slotVII,
		SlotVIII: slotVIII,
		Final:    final,
	}

	if sv.Function != nil {
		// Cs-root: Vr is (degree, Context); Specification = BSC.
		degree, ctx, ok := parse.ParseAffixVr(conjs[2])
		if !ok {
			return g.Formative{}, fmt.Errorf("invalid Cs-root Vr %q", conjs[2])
		}
		f.SlotIV = g.SlotIV{
			Function:      *sv.Function,
			Specification: g.BSC,
			Context:       ctx,
		}
		f.CsRootDegree = &degree
	} else {
		// Reference-root: Vr is normal Slot IV.
		if slotIV, ok := parse.ParseSlotIV(conjs[2]); ok {
			f.SlotIV = slotIV
		} else {
			f.SlotIV = g.DefaultSlotIV
		}
	}
	return f, nil
}

// parseConsonantInitial handles Cr-Vr-Ca-... where Vv is elided. Slot II
// defaults to (S1, PRC).
func parseConsonantInitial(conjs []string, stress parse.Stress) (g.Formative, error) {
	if len(conjs) < 3 {
		return g.Formative{}, fmt.Errorf("consonant-initial formative needs at least 3 conjuncts, got %d", len(conjs))
	}
	root := g.Root(conjs[0])
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
		SlotII:   g.DefaultSlotII,
		SlotIII:  root,
		SlotIV:   slotIV,
		SlotVI:   slotVI,
		SlotVII:  slotVII,
		SlotVIII: slotVIII,
		Final:    final,
	}, nil
}

// parseAfterCa decodes the conjuncts that follow Ca into Slot VII
// affixes, optional Slot VIII (VnCn), and the Formative's Final
// category. Conjuncts alternate V-C; a final unpaired V is the Vc/Vk
// vowel that, together with the surface stress, determines Final.
//
// Detection rule for Slot VIII: if the LAST V-C pair has a consonant
// that is a valid Cn (h/hl/hr/hm/hn/hň or w/y/hw/hrw/hmw/hnw/hňw),
// that pair is VnCn rather than an affix. This matches the Kotlin
// and Haskell reference implementations.
func parseAfterCa(tail []string, stress parse.Stress) ([]g.Affix, g.SlotVIII, g.Final, error) {
	// Pair leading (V, C) chunks. Anything left over is the trailing Vc.
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

	// Slot IX: either the trailing vowel, or elided.
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

	// Slot VIII: if the last pair's consonant is a valid Cn, it is
	// VnCn (and is removed from the affix list).
	var slotVIII g.SlotVIII
	if n := len(pairs); n > 0 && parse.IsValidCn(pairs[n-1].c) {
		s8, ok := parse.ParseVnCn(pairs[n-1].v, pairs[n-1].c)
		if ok {
			slotVIII = disambiguateSlotVIII(final, s8)
			pairs = pairs[:n-1]
		}
	}

	// Remaining pairs are Slot VII affixes (VxCs ordering).
	var slotVII []g.Affix
	for _, p := range pairs {
		t, d := parse.ClassifyAffixVowel(p.v)
		slotVII = append(slotVII, g.Affix{Type: t, Degree: d, Consonant: p.c})
	}

	return slotVII, slotVIII, final, nil
}

// disambiguateSlotVIII rewrites the MoodOrScope variant inside a
// SlotVIII to match the formative's grammatical category. Verbal
// formatives (UnframedVerbal, FramedVerbal) carry MoodVal; nominal
// formatives (UnframedNominal) carry CaseScopeVal.
func disambiguateSlotVIII(final g.Final, s g.SlotVIII) g.SlotVIII {
	verbal := false
	switch final.(type) {
	case g.UnframedVerbal, g.FramedVerbal:
		verbal = true
	}
	flip := func(ms g.MoodOrScope) g.MoodOrScope {
		if verbal {
			if cs, ok := ms.(g.CaseScopeVal); ok {
				return g.MoodVal{Mood: g.CaseScopeToMood(cs.CaseScope)}
			}
			return ms
		}
		if m, ok := ms.(g.MoodVal); ok {
			return g.CaseScopeVal{CaseScope: g.MoodToCaseScope(m.Mood)}
		}
		return ms
	}
	switch v := s.(type) {
	case g.VnCnValence:
		return g.VnCnValence{Valence: v.Valence, MS: flip(v.MS)}
	case g.VnCnPhase:
		return g.VnCnPhase{Phase: v.Phase, MS: flip(v.MS)}
	case g.VnCnEffect:
		return g.VnCnEffect{Effect: v.Effect, MS: flip(v.MS)}
	case g.VnCnLevel:
		return g.VnCnLevel{Level: v.Level, Absolute: v.Absolute, MS: flip(v.MS)}
	case g.VnCnAspect:
		return g.VnCnAspect{Aspect: v.Aspect, MS: flip(v.MS)}
	}
	return s
}

// parseFinal builds the Formative.Final from the observed surface
// stress and the trailing Vc/Vk vowel (or "" if elided). Stress drives
// the variant choice; Ultimate/Monosyllabic produce UnframedVerbal,
// Antepenultimate produces FramedVerbal, Penultimate produces
// UnframedNominal.
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
