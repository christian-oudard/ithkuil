package parse

import (
	"testing"

	"github.com/christian-oudard/ithkuil/grammar"
)

func TestParseBias_Known(t *testing.T) {
	// Match the Haskell hspec assertions.
	cases := []struct {
		in   string
		want grammar.Bias
	}{
		{"řřx", grammar.DOL},
		{"kff", grammar.DIS},
		{"lf", grammar.ACC},
		{"ẓmm", grammar.DLC},
		{"pss", grammar.MNF},
		{"msf", grammar.RSG},
		{"xtļ", grammar.ARB},
		{"lļ", grammar.ADS},
	}
	for _, c := range cases {
		got, ok := ParseBias(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseBias(%q) = (%v, %v), want (%v, true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestParseBias_RoundTrip(t *testing.T) {
	// Every Bias's form must parse back to that Bias.
	for _, b := range grammar.AllBiases {
		f := grammar.BiasForm(b)
		got, ok := ParseBias(f)
		if !ok {
			t.Errorf("ParseBias(%q) failed for %s", f, b)
			continue
		}
		if got != b {
			t.Errorf("round trip: %s → %q → %s", b, f, got)
		}
	}
}

func TestParseBias_Rejects(t *testing.T) {
	for _, s := range []string{"", "x", "xyz"} {
		if b, ok := ParseBias(s); ok {
			t.Errorf("ParseBias(%q) = %v, want failure", s, b)
		}
	}
}
