package parse

import (
	"fmt"

	"github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/surface"
)

// ParseSingleAffix reads a single-affix adjunct word: one Vx-Cs pair,
// optionally followed by a Vs scope vowel ([Vx Cs] or [Vx Cs Vs]).
// The Cs-Vx ordering is also accepted for compatibility with the
// older affix-pair form.
func ParseSingleAffix(word string) (grammar.SingleAffixAdjunct, error) {
	conjs := surface.SplitConjuncts(word)
	var a grammar.SingleAffixAdjunct
	switch len(conjs) {
	case 2:
		x, y := conjs[0], conjs[1]
		switch {
		case surface.IsVowelConjunct(x) && surface.IsConsonantConjunct(y):
			a = grammar.SingleAffixAdjunct{Vx: x, Cs: y}
		case surface.IsConsonantConjunct(x) && surface.IsVowelConjunct(y):
			a = grammar.SingleAffixAdjunct{Vx: y, Cs: x}
		default:
			return grammar.SingleAffixAdjunct{}, fmt.Errorf("single-affix adjunct: %q + %q is not a vowel/consonant pair", x, y)
		}
	case 3:
		// Vx-Cs-Vs form: vowel, consonant, vowel.
		x, y, z := conjs[0], conjs[1], conjs[2]
		if !(surface.IsVowelConjunct(x) && surface.IsConsonantConjunct(y) && surface.IsVowelConjunct(z)) {
			return grammar.SingleAffixAdjunct{}, fmt.Errorf("single-affix adjunct: %q+%q+%q doesn't match Vx-Cs-Vs", x, y, z)
		}
		a = grammar.SingleAffixAdjunct{Vx: x, Cs: y, Vs: z}
	default:
		return grammar.SingleAffixAdjunct{}, fmt.Errorf("single-affix adjunct: expected 2 or 3 conjuncts, got %d", len(conjs))
	}
	scope, ok := grammar.VsScope(a.Vs)
	if !ok {
		return grammar.SingleAffixAdjunct{}, fmt.Errorf("single-affix adjunct: %q is not a valid Vs scope vowel", a.Vs)
	}
	a.Scope = scope
	return a, nil
}

// isCzConsonant reports whether c is a valid Cz scope consonant used
// as the boundary between the first affix and the rest in a
// multiple-affix adjunct.
func isCzConsonant(c string) bool {
	switch c {
	case "h", "'h", "'hl", "'hr", "hw", "'hw":
		return true
	}
	return false
}

// ParseMultipleAffix reads a multiple-affix adjunct word with structure
// [ë] Cs Vx Cz (VxCs)+ [Vz]. Cs is the first affix's consonant, Vx its
// vowel; Cz is a scope consonant; subsequent VxCs pairs follow; an
// optional final Vz scope vowel may close the word.
//
// "xaheitr" → first=(x, a), Cz=h, more=[(ei, tr)], Vz=""
// "xaheitre" → first=(x, a), Cz=h, more=[(ei, tr)], Vz="e"
// "xa'heitr" → first=(x, a), Cz='h, more=[(ei, tr)], Vz=""
func ParseMultipleAffix(word string) (grammar.MultipleAffixAdjunct, error) {
	conjs := surface.SplitConjuncts(word)
	// Strip optional leading "ë" prefix.
	if len(conjs) > 0 && conjs[0] == "ë" {
		conjs = conjs[1:]
	}
	if len(conjs) < 4 {
		return grammar.MultipleAffixAdjunct{}, fmt.Errorf("multiple-affix adjunct: expected ≥4 conjuncts after ë-prefix, got %d", len(conjs))
	}
	cs, vx, cz := conjs[0], conjs[1], conjs[2]
	if !surface.IsConsonantConjunct(cs) || !surface.IsVowelConjunct(vx) {
		return grammar.MultipleAffixAdjunct{}, fmt.Errorf("multiple-affix adjunct: first pair %q+%q not Cs+Vx", cs, vx)
	}
	if !isCzConsonant(cz) {
		return grammar.MultipleAffixAdjunct{}, fmt.Errorf("multiple-affix adjunct: %q is not a Cz scope consonant", cz)
	}

	// Remaining conjuncts: alternating (Vx Cs)* with an optional trailing Vz.
	rest := conjs[3:]
	var more []grammar.AffixPair
	var vz string
	for i := 0; i < len(rest); {
		if i+1 < len(rest) &&
			surface.IsVowelConjunct(rest[i]) &&
			surface.IsConsonantConjunct(rest[i+1]) {
			more = append(more, grammar.AffixPair{Vx: rest[i], Cs: rest[i+1]})
			i += 2
			continue
		}
		// Trailing vowel (Vz).
		if i == len(rest)-1 && surface.IsVowelConjunct(rest[i]) {
			vz = rest[i]
			i++
			continue
		}
		return grammar.MultipleAffixAdjunct{}, fmt.Errorf("multiple-affix adjunct: unexpected conjunct %q at position %d", rest[i], 3+i)
	}
	if len(more) < 1 {
		return grammar.MultipleAffixAdjunct{}, fmt.Errorf("multiple-affix adjunct: at least one trailing VxCs pair required")
	}
	firstScope, ok := grammar.CzScope(cz)
	if !ok {
		// isCzConsonant already accepted cz, so this is unreachable.
		return grammar.MultipleAffixAdjunct{}, fmt.Errorf("multiple-affix adjunct: %q has no scope mapping", cz)
	}
	restScope := firstScope
	if vz != "" && vz != "ai" {
		s, ok := grammar.VzScope(vz)
		if !ok {
			return grammar.MultipleAffixAdjunct{}, fmt.Errorf("multiple-affix adjunct: %q is not a valid Vz scope vowel", vz)
		}
		restScope = s
	}
	return grammar.MultipleAffixAdjunct{
		First:      grammar.AffixPair{Vx: vx, Cs: cs},
		Cz:         cz,
		Affixes:    more,
		Vz:         vz,
		FirstScope: firstScope,
		RestScope:  restScope,
	}, nil
}
