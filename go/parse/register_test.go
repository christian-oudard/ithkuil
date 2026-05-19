package parse

import (
	"testing"

	"github.com/coudard/ithkuil/go/grammar"
)

func TestParseRegister(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.Register
	}{
		{"ha", grammar.DSV},
		{"he", grammar.PNT},
		{"hi", grammar.SPF},
		{"ho", grammar.EXM},
		{"hu", grammar.CGT},
	}
	for _, c := range cases {
		got, ok := ParseRegister(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseRegister(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
	if _, ok := ParseRegister(""); ok {
		t.Error("ParseRegister(\"\") returned ok=true, want false")
	}
	if _, ok := ParseRegister("xx"); ok {
		t.Error("ParseRegister(\"xx\") returned ok=true, want false")
	}
}

func TestParseRegisterFinal(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.Register
	}{
		{"hai", grammar.DSV},
		{"hei", grammar.PNT},
		{"hiu", grammar.SPF},
		{"hoi", grammar.EXM},
		{"hui", grammar.CGT},
		{"hüi", grammar.END},
	}
	for _, c := range cases {
		got, ok := ParseRegisterFinal(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseRegisterFinal(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}
