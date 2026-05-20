package parse

import (
	"testing"

	"github.com/christian-oudard/ithkuil/grammar"
)

func TestParseVk_Series1(t *testing.T) {
	// Series 1 = ASR + each of the 9 validations.
	cases := []struct {
		in  string
		val grammar.Validation
	}{
		{"a", grammar.OBS},
		{"ä", grammar.REC},
		{"e", grammar.PUP},
		{"i", grammar.RPR},
		{"ëi", grammar.USP},
		{"ö", grammar.IMA},
		{"o", grammar.CVN},
		{"ü", grammar.ITU},
		{"u", grammar.INF},
	}
	for _, c := range cases {
		s, ok := ParseVk(c.in)
		as, isAs := s.(grammar.Assertive)
		if !ok || !isAs || as.Validation != c.val {
			t.Errorf("ParseVk(%q) = (%v,%v), want Assertive{%v}",
				c.in, s, ok, c.val)
		}
	}
}

func TestParseVk_Series2(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.Vk
	}{
		{"ai", grammar.Directive{}},
		{"au", grammar.Declarative{}},
		{"ei", grammar.Interrogative{}},
		{"eu", grammar.Verificative{}},
		{"ou", grammar.Admonitive{}},
		{"oi", grammar.Potentiative{}},
		{"iu", grammar.Hortative{}},
		{"ui", grammar.Conjectural{}},
	}
	for _, c := range cases {
		s, ok := ParseVk(c.in)
		if !ok || s != c.want {
			t.Errorf("ParseVk(%q) = (%v,%v), want %v", c.in, s, ok, c.want)
		}
	}
}

func TestParseVk_AcceptsAccents(t *testing.T) {
	// Stressed vowel should normalize before lookup.
	s, ok := ParseVk("á")
	as, isAs := s.(grammar.Assertive)
	if !ok || !isAs || as.Validation != grammar.OBS {
		t.Errorf("ParseVk(\"á\") = (%v,%v), want Assertive{OBS}", s, ok)
	}
}

func TestParseVk_Rejects(t *testing.T) {
	for _, s := range []string{"", "x", "ia", "ao"} {
		if v, ok := ParseVk(s); ok {
			t.Errorf("ParseVk(%q) = (%v,true), want failure", s, v)
		}
	}
}
