package parse

import (
	"fmt"

	"github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/surface"
)

// isValidModularConsonant reports whether c is permitted as the
// consonant of a modular-adjunct VnCn pair (Slot 2 Cn or Slot 3 Cm).
// The set is h-prefixed (h/hl/hr/hm/hn/hň plus their aspect-pattern
// twins hw/hrw/hmw/hnw/hňw), the lone w/y aspect-pattern markers,
// and n/ň for Slot 3 Cm.
func isValidModularConsonant(c string) bool {
	switch c {
	case "h", "hl", "hr", "hm", "hn", "hň",
		"w", "y", "hw", "hrw", "hmw", "hnw", "hňw",
		"n", "ň":
		return true
	}
	return false
}

// ParseModular reads a modular adjunct from its conjunct list. The
// general shape is [w/y] (Vn Cn){0-3} V(final).
//
// Cases recognized:
//   - "Vn Cn" — single (Vn, Cn) pair, no prefix or trailing vowel.
//   - "w V" / "y V" — scope prefix + aspect-only modular (no VnCn).
//   - "[w/y] Vn Cn ... V" — full form with prefix, up to 3 pairs,
//     and a trailing final vowel.
func ParseModular(word string) (grammar.ModularAdjunct, error) {
	conjs := surface.SplitConjuncts(word)
	if len(conjs) == 0 {
		return grammar.ModularAdjunct{}, fmt.Errorf("modular adjunct: empty input")
	}

	// Optional w/y scope prefix.
	scope := grammar.ModularScopeDefault
	switch conjs[0] {
	case "w":
		scope = grammar.ModularScopeParent
		conjs = conjs[1:]
	case "y":
		scope = grammar.ModularScopeConcat
		conjs = conjs[1:]
	}

	if len(conjs) == 0 {
		return grammar.ModularAdjunct{}, fmt.Errorf("modular adjunct: nothing after scope prefix")
	}

	// Walk Vn Cn pairs; whatever vowel comes alone at the end is the
	// final aspect/scope vowel. The first pair uses Cn (h-prefixed
	// or w/y); subsequent pairs may use Cm (n/ň) per §4.3 Slot 3.
	type rawPair struct{ vn, cn string }
	var pairs []rawPair
	var final string
	for i := 0; i < len(conjs); {
		if i+1 < len(conjs) &&
			surface.IsVowelConjunct(conjs[i]) &&
			isValidModularConsonant(conjs[i+1]) {
			pairs = append(pairs, rawPair{vn: conjs[i], cn: conjs[i+1]})
			i += 2
			continue
		}
		if i == len(conjs)-1 && surface.IsVowelConjunct(conjs[i]) {
			final = conjs[i]
			i++
			continue
		}
		return grammar.ModularAdjunct{}, fmt.Errorf("modular adjunct: unexpected conjunct %q", conjs[i])
	}
	if len(pairs) > 3 {
		return grammar.ModularAdjunct{}, fmt.Errorf("modular adjunct: %d VnCn pairs (max 3)", len(pairs))
	}
	if len(pairs) == 0 && final == "" {
		return grammar.ModularAdjunct{}, fmt.Errorf("modular adjunct: no VnCn pair or final vowel")
	}

	// V_H reach scope: §4.3 Slot 4 — when ultimate stress is present
	// and there's at least one (Vn, Cn) pair, the trailing vowel is a
	// scope marker rather than another Vn. Decode it into Reach; the
	// vowel is consumed by Reach and doesn't enter Content.
	reach := grammar.ModularReachNone
	_, stress := surface.Strip(word)
	if stress == surface.Ultimate && len(pairs) > 0 && final != "" {
		if r, ok := decodeVH(final); ok {
			reach = r
			final = ""
		}
	}

	// Build typed Content from the surface pairs (and the trailing
	// aspect vowel when present without a Cn — lone-aspect modular).
	var content []grammar.SlotVIII
	for _, p := range pairs {
		s, ok := ParseVnCn(p.vn, p.cn)
		if !ok {
			return grammar.ModularAdjunct{}, fmt.Errorf("modular adjunct: cannot decode (Vn=%q, Cn=%q)", p.vn, p.cn)
		}
		content = append(content, s)
	}
	if final != "" {
		// Lone-aspect modular: just the aspect vowel, default FAC mood.
		asp, ok := ParseVnAspect(final)
		if !ok {
			return grammar.ModularAdjunct{}, fmt.Errorf("modular adjunct: trailing vowel %q is not an aspect", final)
		}
		content = append(content, grammar.VnCnAspect{Aspect: asp, MoodScope: grammar.FAC})
	}

	return grammar.ModularAdjunct{
		Scope:   scope,
		Reach:   reach,
		Content: content,
	}, nil
}

// decodeVH maps a V_H vowel from §4.3 Slot 4 to a ModularReach value.
// "i" and "u" both encode the formative-only reach.
func decodeVH(v string) (grammar.ModularReach, bool) {
	switch NormalizeAccents(v) {
	case "a":
		return grammar.ModularReachCaseMoodIll, true
	case "e":
		return grammar.ModularReachCaseMood, true
	case "i", "u":
		return grammar.ModularReachFormative, true
	case "o":
		return grammar.ModularReachAdjacent, true
	}
	return 0, false
}
