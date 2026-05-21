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
		if len(ma.Content) != 1 {
			t.Errorf("ParseModular(%q): Content len = %d, want 1", c.word, len(ma.Content))
			continue
		}
		_ = c.vn
		_ = c.cn
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
	// just a trailing aspect vowel. Decoded as one VnCnAspect entry
	// with default FAC mood.
	ma, err := ParseModular("a")
	if err != nil {
		t.Fatalf("ParseModular(%q): %v", "a", err)
	}
	if ma.Scope != grammar.ModularScopeDefault || len(ma.Content) != 1 {
		t.Errorf("ParseModular(\"a\") = %+v, want lone aspect content", ma)
	}
	if _, ok := ma.Content[0].(grammar.VnCnAspect); !ok {
		t.Errorf("ParseModular(\"a\") content = %T, want VnCnAspect", ma.Content[0])
	}
}

func TestParseModular_ChainsWithVnCn(t *testing.T) {
	// A modular adjunct's typed Content should match ParseVnCn output
	// on the original surface (Vn, Cn) pair.
	ma, err := ParseModular("ah")
	if err != nil {
		t.Fatal(err)
	}
	if len(ma.Content) != 1 {
		t.Fatalf("Content len = %d, want 1", len(ma.Content))
	}
	s8 := ma.Content[0]
	// "a"/"h" should resolve to VnCnValence{MNO, ...}.
	vc, ok := s8.(grammar.VnCnValence)
	if !ok || vc.Valence != grammar.MNO {
		t.Errorf("expected VnCnValence{MNO, ...}, got %#v", s8)
	}
}
