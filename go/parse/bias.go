package parse

import "github.com/coudard/ithkuil/go/grammar"

// biasByForm is the reverse index from surface consonant cluster to
// Bias value, built once at init time.
var biasByForm = func() map[string]grammar.Bias {
	m := make(map[string]grammar.Bias, len(grammar.AllBiases))
	for _, b := range grammar.AllBiases {
		m[grammar.BiasForm(b)] = b
	}
	return m
}()

// ParseBias decodes a surface consonant cluster as a Bias adjunct.
// Returns (0, false) for clusters that don't match any Bias form.
func ParseBias(s string) (grammar.Bias, bool) {
	b, ok := biasByForm[s]
	return b, ok
}
