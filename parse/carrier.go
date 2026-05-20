package parse

import (
	"fmt"
	"strings"

	"github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/surface"
)

// carrierByForm is the reverse index from consonant cluster to
// CarrierType. Built at init time.
var carrierByForm = func() map[string]grammar.CarrierType {
	m := make(map[string]grammar.CarrierType, len(grammar.AllCarrierTypes))
	for _, c := range grammar.AllCarrierTypes {
		m[grammar.CarrierTypeForm(c)] = c
	}
	return m
}()

// ParseCarrierType decodes a carrier consonant cluster (hl/hm/hn/hň)
// into a CarrierType. Returns false for any other input.
func ParseCarrierType(s string) (grammar.CarrierType, bool) {
	c, ok := carrierByForm[s]
	return c, ok
}

// ParseCarrier reads a carrier adjunct word: a CarrierType cluster
// (hl/hm/hn/hň) followed by at least one trailing conjunct. The
// trailing content is joined and stored in Vc — it's often a single
// case vowel ("hla" → {Carrier, "a"}) but the Haskell reference
// accepts longer trailing matter ("hnas" → {Naming, "as"}) so this
// parser does too.
func ParseCarrier(word string) (grammar.CarrierAdjunct, error) {
	conjs := surface.SplitConjuncts(word)
	if len(conjs) < 2 {
		return grammar.CarrierAdjunct{}, fmt.Errorf("carrier adjunct: expected ≥2 conjuncts, got %d", len(conjs))
	}
	cs := conjs[0]
	ct, ok := ParseCarrierType(cs)
	if !ok {
		return grammar.CarrierAdjunct{}, fmt.Errorf("carrier adjunct: %q is not a carrier consonant", cs)
	}
	content := strings.Join(conjs[1:], "")
	return grammar.CarrierAdjunct{Type: ct, Vc: content}, nil
}
