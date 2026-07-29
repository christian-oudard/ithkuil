package grammar

import "testing"

func TestBiasCount(t *testing.T) {
	// 61 Bias values per the grammar spec.
	if n := len(AllBiases); n != 61 {
		t.Errorf("AllBiases has %d entries, want 61", n)
	}
}

func TestBiasExpression(t *testing.T) {
	// Spot checks: each non-ANP Bias has a non-empty expression.
	if BiasExpression(DOL) != "Ow! Ouch!" {
		t.Errorf("DOL expression = %q", BiasExpression(DOL))
	}
	if BiasExpression(ANP) != "I'm looking forward to this!" {
		t.Errorf("ANP expression = %q", BiasExpression(ANP))
	}
}
