package parse

import (
	"fmt"

	"github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
)

// isCn reports whether c is a §4.3 Slot 2 C_N: the h-prefixed set
// (h/hl/hr/hm/hn/hň) plus its aspect-pattern twins (w/y and
// hw/hrw/hmw/hnw/hňw). C_N carries the pair's Mood/Case-Scope.
func isCn(c string) bool {
	switch c {
	case "h", "hl", "hr", "hm", "hn", "hň",
		"w", "y", "hw", "hrw", "hmw", "hnw", "hňw":
		return true
	}
	return false
}

// isCm reports whether c is a §4.3 Slot 3 C_M. The inventory is two
// consonants and the spec spends both on one distinction — "C_M = n if
// V_N represents an Aspect, otherwise C_M = ň" — so unlike C_N it
// carries no Mood/Case-Scope, and Slot 3 takes the default FAC.
func isCm(c string) bool { return c == "n" || c == "ň" }

// parseCmPair decodes a Slot 3 (V_N C_M) pair.
func parseCmPair(vn, cm string) (grammar.SlotVIII, bool) {
	if cm == "n" {
		asp, ok := ParseVnAspect(vn)
		if !ok {
			return nil, false
		}
		return grammar.VnCnAspect{Aspect: asp, MoodScope: grammar.FAC}, true
	}
	return ParseVnPattern1(vn, grammar.FAC)
}

// ParseModular reads a modular adjunct from its conjunct list. §4.3
// gives it four slots:
//
//	1  ' or w- or y-       optional scope prefix
//	2  (V_N C_N)           optional
//	3  (V_N C_M)           optional
//	4  V_N or V_H          mandatory
//
// So there are at most two (V_N C_N) pairs, and the trailing vowel is
// slot 4 rather than a third pair. Slot 2 takes a C_N and slot 3 takes
// a C_M; the two inventories are disjoint.
//
// Cases recognized:
//   - "Vn Cn" — single pair, no prefix or trailing vowel.
//   - "w V" / "y V" — scope prefix + aspect-only modular (no VnCn).
//   - "[w/y] Vn Cn [Vn Cm] V" — full form.
func ParseModular(word string) (grammar.ModularAdjunct, error) {
	conjs := phonology.SplitConjuncts(word)
	if len(conjs) == 0 {
		return grammar.ModularAdjunct{}, shape(word, "shape", word, "a modular adjunct needs at least one conjunct")
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
		return grammar.ModularAdjunct{}, shape(word, "shape", word,
			"the w/y scope prefix needs a Vn Cn pair or an aspect vowel after it")
	}

	// Walk the (V_N C) pairs; whatever vowel comes alone at the end is
	// slot 4. Slot 2 takes a C_N, slot 3 a C_M.
	type rawPair struct{ vn, c string }
	var pairs []rawPair
	var final string
	for i := 0; i < len(conjs); {
		if i+1 < len(conjs) && phonology.IsVowelConjunct(conjs[i]) {
			c := conjs[i+1]
			slot := len(pairs) + 2
			ok := false
			switch slot {
			case 2:
				ok = isCn(c)
			case 3:
				ok = isCm(c)
			}
			if !ok {
				return grammar.ModularAdjunct{}, shape(word, fmt.Sprintf("C%d", slot), c,
					fmt.Sprintf("Slot %d of a modular adjunct takes %s", slot, slotConsonants(slot)))
			}
			pairs = append(pairs, rawPair{vn: conjs[i], c: c})
			i += 2
			continue
		}
		if i == len(conjs)-1 && phonology.IsVowelConjunct(conjs[i]) {
			final = conjs[i]
			i++
			continue
		}
		return grammar.ModularAdjunct{}, shape(word, "shape", conjs[i],
			"a modular adjunct alternates Vn and Cn from here, and "+conjs[i]+" fits neither position")
	}
	if len(pairs) == 0 && final == "" {
		return grammar.ModularAdjunct{}, shape(word, "shape", word,
			"a modular adjunct holds at least one Vn Cn pair or a lone aspect vowel")
	}

	// V_H reach scope: §4.3 Slot 4 — when ultimate stress is present
	// and there's at least one (Vn, Cn) pair, the trailing vowel is a
	// scope marker rather than another Vn. Decode it into Reach; the
	// vowel is consumed by Reach and doesn't enter Content.
	reach := grammar.ModularReachNone
	_, stress := phonology.Strip(word)
	if stress == phonology.Ultimate && len(pairs) > 0 && final != "" {
		if r, ok := decodeVH(final); ok {
			reach = r
			final = ""
		}
	}

	// Build typed Content from the romanization pairs (and the trailing
	// aspect vowel when present without a Cn — lone-aspect modular).
	var content []grammar.SlotVIII
	for _, p := range pairs {
		var s grammar.SlotVIII
		var ok bool
		if isCm(p.c) {
			s, ok = parseCmPair(p.vn, p.c)
		} else {
			s, ok = ParseVnCn(p.vn, p.c)
		}
		if !ok {
			return grammar.ModularAdjunct{}, value(word, "Vn", p.vn,
				"no Valence, Phase, Effect, Level or Aspect is written "+p.vn+" against "+p.c)
		}
		content = append(content, s)
	}
	if final != "" {
		// Lone-aspect modular: just the aspect vowel, default FAC mood.
		//
		// §4.3's Slot 4 is mandatory and holds "Aspect or
		// Valence/Phase/Level/Effect or Specialized Scope", given as
		// "V_N or V_H" — a bare vowel with no consonant, and a V_N
		// vowel does not by itself say which category it is. The four
		// Pattern-1 categories and the four Aspect columns share the
		// same vowel forms one for one; in formative Slot VIII the
		// following C_N resolves it, and Slot 4 has no C_N. The other
		// two slots both carry the distinction — Slot 2 has its C_N and
		// Slot 3 is given a consonant for no other purpose ("C_M = n if
		// V_N represents an Aspect, otherwise C_M = ň") — so the
		// mandatory slot is the one left without a marker. §4.3's own
		// example uhlaini ends in a Slot-4 i, which is RCP Valence or
		// PRG Aspect with nothing to choose between them.
		//
		// Stress does not help: the vowel is read as V_H only under
		// ultimate stress, handled above, so penultimate stress already
		// means "this is a V_N" and cannot also encode which V_N.
		//
		// We read Aspect. Slot 3 exists to say "the V_N beside me is an
		// Aspect", which would be pointless if a bare V_N were not an
		// Aspect by default, and every attested Slot-4 vowel we have
		// decodes as one.
		asp, ok := ParseVnAspect(final)
		if !ok {
			return grammar.ModularAdjunct{}, value(word, "Vn", final,
				"no aspect is written "+final+"; a modular adjunct's lone trailing vowel is read as one")
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

// slotConsonants names what a modular adjunct's Slot 2 and Slot 3
// admit. §4.3 gives Slot 3 its C_M for one purpose — to say whether
// the V_N beside it is an Aspect — so the two inventories are
// disjoint and naming the wrong one would send a reader to the wrong
// table.
func slotConsonants(slot int) string {
	if slot == 3 {
		return "a C_M consonant, n or ň"
	}
	return "a C_N consonant"
}
