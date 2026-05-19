package parse

import (
	"testing"

	"github.com/coudard/ithkuil/go/grammar"
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
	for _, w := range []string{"", "h", "a", "ax", "axyz"} {
		if _, err := ParseModular(w); err == nil {
			t.Errorf("ParseModular(%q) succeeded, want error", w)
		}
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
