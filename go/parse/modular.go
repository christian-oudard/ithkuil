package parse

import (
	"fmt"

	"github.com/coudard/ithkuil/go/grammar"
)

// ParseModular reads a modular adjunct from its conjunct list. The
// general shape is [w/y] (Vn Cn){0-3} V(final).
//
// Cases recognized:
//   - "Vn Cn" — single (Vn, Cn) pair, no prefix or trailing vowel.
//   - "w V" / "y V" — scope prefix + aspect-only modular (no VnCn).
//   - "[w/y] Vn Cn ... V" — full form with prefix, up to 3 pairs,
//     and a trailing final vowel.
func ParseModular(word string) (grammar.ModularAdjunct, error) {
	conjs := SplitConjuncts(word)
	if len(conjs) < 2 {
		return grammar.ModularAdjunct{}, fmt.Errorf("modular adjunct: expected at least 2 conjuncts, got %d", len(conjs))
	}

	// Optional w/y scope prefix.
	prefix := ""
	if conjs[0] == "w" || conjs[0] == "y" {
		prefix = conjs[0]
		conjs = conjs[1:]
	}

	if len(conjs) == 0 {
		return grammar.ModularAdjunct{}, fmt.Errorf("modular adjunct: nothing after scope prefix")
	}

	// Walk Vn Cn pairs; whatever vowel comes alone at the end is the
	// final aspect/scope vowel.
	var pairs []grammar.VnCnPair
	var final string
	for i := 0; i < len(conjs); {
		if i+1 < len(conjs) &&
			IsVowelConjunct(conjs[i]) &&
			IsValidCn(conjs[i+1]) {
			pairs = append(pairs, grammar.VnCnPair{Vn: conjs[i], Cn: conjs[i+1]})
			i += 2
			continue
		}
		if i == len(conjs)-1 && IsVowelConjunct(conjs[i]) {
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

	ma := grammar.ModularAdjunct{
		Prefix: prefix,
		Pairs:  pairs,
		Final:  final,
	}
	// Backwards compatibility: when there's exactly one pair, set the
	// flat Vn/Cn fields so older callers still work.
	if len(pairs) == 1 && final == "" && prefix == "" {
		ma.Vn = pairs[0].Vn
		ma.Cn = pairs[0].Cn
	}
	return ma, nil
}
