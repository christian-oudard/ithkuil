package grammar

import "testing"

func TestBiasCount(t *testing.T) {
	// 61 Bias values per the grammar spec.
	if n := len(AllBiases); n != 61 {
		t.Errorf("AllBiases has %d entries, want 61", n)
	}
}

func TestBiasFormsUnique(t *testing.T) {
	seen := map[string]Bias{}
	for _, b := range AllBiases {
		f := BiasForm(b)
		if f == "" {
			t.Errorf("%s has empty form", b)
			continue
		}
		if other, dup := seen[f]; dup {
			t.Errorf("form %q duplicated: %s and %s", f, other, b)
		}
		seen[f] = b
	}
}

func TestBiasFormSpotCheck(t *testing.T) {
	cases := []struct {
		b    Bias
		want string
	}{
		{DOL, "řřx"},
		{DIS, "kff"},
		{ACC, "lf"},
		{DLC, "ẓmm"},
		{MNF, "pss"},
		{RSG, "msf"},
		{ARB, "xtļ"},
		{ADS, "lļ"},
	}
	for _, c := range cases {
		if got := BiasForm(c.b); got != c.want {
			t.Errorf("BiasForm(%s) = %q, want %q", c.b, got, c.want)
		}
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
