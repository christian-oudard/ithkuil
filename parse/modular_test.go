package parse

import (
	"testing"

	"github.com/christian-oudard/ithkuil/grammar"
)

func TestParseModular_Valid(t *testing.T) {
	cases := []struct {
		word   string
		vn, cn string
	}{
		{"ah", "a", "h"},
		{"aihl", "ai", "hl"},
		{"iahňw", "ia", "hňw"},
	}
	for _, c := range cases {
		ma, err := ParseModular(c.word)
		if err != nil {
			t.Errorf("ParseModular(%q) error: %v", c.word, err)
			continue
		}
		if ma.Vn != c.vn || ma.Cn != c.cn {
			t.Errorf("ParseModular(%q) = %+v, want Vn=%q Cn=%q",
				c.word, ma, c.vn, c.cn)
		}
	}
}

func TestParseModular_Invalid(t *testing.T) {
	// A single vowel (e.g. "a") is now valid per §4.3 — a lone
	// aspect modular adjunct. Truly invalid inputs: empty string,
	// consonant-only forms (no Vn), and unrecognised conjunct
	// patterns.
	for _, w := range []string{"", "h", "ax", "axyz"} {
		if _, err := ParseModular(w); err == nil {
			t.Errorf("ParseModular(%q) succeeded, want error", w)
		}
	}
}

func TestParseModular_SingleVowel(t *testing.T) {
	// Per §4.3, a single vowel is a lone-aspect modular: no prefix,
	// no VnCn pairs, just a final vowel.
	ma, err := ParseModular("a")
	if err != nil {
		t.Fatalf("ParseModular(%q): %v", "a", err)
	}
	if ma.Final != "a" || ma.Prefix != "" || len(ma.Pairs) != 0 {
		t.Errorf("ParseModular(\"a\") = %+v, want lone final \"a\"", ma)
	}
}

func TestParseModular_ChainsWithVnCn(t *testing.T) {
	// A modular adjunct's text fields should round-trip through ParseVnCn,
	// giving a typed SlotVIII we can disambiguate.
	ma, err := ParseModular("ah")
	if err != nil {
		t.Fatal(err)
	}
	s8, ok := ParseVnCn(ma.Vn, ma.Cn)
	if !ok {
		t.Fatal("ParseVnCn on modular fields failed")
	}
	// "a"/"h" should resolve to VnCnValence{MNO, MoodVal{FAC}}.
	vc, ok := s8.(grammar.VnCnValence)
	if !ok || vc.Valence != grammar.MNO {
		t.Errorf("expected VnCnValence{MNO, ...}, got %#v", s8)
	}
}
