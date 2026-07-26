package surface

import "testing"

// Decomposed (NFD) spellings: each diacritic here is a separate
// combining rune, not the precomposed letter it looks like.
const (
	nfdSvel   = "švel"         // s + combining caron
	nfdMala   = "malá"         // a + combining acute
	nfdMalut  = "malëuţřait" // e + diaeresis, t + cedilla, r + caron
	nfdZdot   = "ẓ"            // z + combining dot below
	nfdSvelUp = "Švel"         // capital S + combining caron
)

func TestNormalize(t *testing.T) {
	cases := []struct {
		name string
		in   string
		want string
	}{
		{"already canonical", "švel", "švel"},
		{"decomposed caron", nfdSvel, "švel"},
		{"decomposed acute", nfdMala, "malá"},
		{"decomposed mixed", nfdMalut, "malëuţřait"},
		{"decomposed dot below", nfdZdot, "ẓ"},
		{"capital", "Švel", "švel"},
		{"decomposed capital", nfdSvelUp, "švel"},
		{"capital with acute", "MALÁ", "malá"},
		{"empty", "", ""},
	}
	for _, c := range cases {
		if got := Normalize(c.in); got != c.want {
			t.Errorf("%s: Normalize(%q) = %q, want %q", c.name, c.in, got, c.want)
		}
	}
}

// Normalize must be idempotent: normalizing already-normal text is a
// no-op, which is what lets the parse entry points call it freely.
func TestNormalize_Idempotent(t *testing.T) {
	for _, in := range []string{"švel", "Švel", nfdSvel, nfdMalut, ""} {
		once := Normalize(in)
		if twice := Normalize(once); twice != once {
			t.Errorf("Normalize(%q) not idempotent: %q then %q", in, once, twice)
		}
	}
}
