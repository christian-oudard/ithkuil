package parse

import (
	"fmt"
	"strings"

	"github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
)

// carrierByForm is the reverse index from consonant cluster to
// CarrierType. Built at init time.
var carrierByForm = func() map[string]grammar.CarrierType {
	m := make(map[string]grammar.CarrierType, len(grammar.AllCarrierTypes))
	for _, c := range grammar.AllCarrierTypes {
		m[CarrierTypeForm(c)] = c
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
// (hl/hm/hn/hň) followed by a case vowel. The case vowel is decoded
// at parse time so the in-memory model carries the typed Case rather
// than the raw written vowel.
func ParseCarrier(word string) (grammar.CarrierAdjunct, error) {
	conjs := phonology.SplitConjuncts(word)
	if len(conjs) < 2 {
		return grammar.CarrierAdjunct{}, fmt.Errorf("carrier adjunct: expected ≥2 conjuncts, got %d", len(conjs))
	}
	cs := conjs[0]
	ct, ok := ParseCarrierType(cs)
	if !ok {
		return grammar.CarrierAdjunct{}, fmt.Errorf("carrier adjunct: %q is not a carrier consonant", cs)
	}
	vc := strings.Join(conjs[1:], "")
	c, ok := ParseCase(vc)
	if !ok {
		return grammar.CarrierAdjunct{}, fmt.Errorf("carrier adjunct: %q is not a recognized case vowel", vc)
	}
	return grammar.CarrierAdjunct{Type: ct, Case: c}, nil
}
