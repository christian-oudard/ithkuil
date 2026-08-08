package parse

import (
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
		return grammar.CarrierAdjunct{}, shape(word, "shape", word,
			"a carrier adjunct is a carrier consonant plus a case vowel, and this has one conjunct (§4.5)")
	}
	cs := conjs[0]
	ct, ok := ParseCarrierType(cs)
	if !ok {
		return grammar.CarrierAdjunct{}, shape(word, "Cp", cs,
			"a carrier adjunct opens with hl, hm, hn or hň (§4.5)")
	}
	vc := strings.Join(conjs[1:], "")
	c, ok := ParseCase(vc)
	if !ok {
		return grammar.CarrierAdjunct{}, value(word, "Vc", vc,
			"no case is written "+vc+"; a carrier adjunct carries a case and nothing else (§4.5, §3.9.1)")
	}
	return grammar.CarrierAdjunct{Type: ct, Case: c}, nil
}
