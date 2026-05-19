package parse

import (
	"testing"

	"github.com/coudard/ithkuil/go/grammar"
)

func TestParseCarrierType(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.CarrierType
	}{
		{"hl", grammar.Carrier},
		{"hm", grammar.Quotative},
		{"hn", grammar.Naming},
		{"hň", grammar.Phrasal},
	}
	for _, c := range cases {
		got, ok := ParseCarrierType(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseCarrierType(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestParseCarrierType_Rejects(t *testing.T) {
	for _, s := range []string{"", "h", "xy", "hp"} {
		if c, ok := ParseCarrierType(s); ok {
			t.Errorf("ParseCarrierType(%q) = %v, want failure", s, c)
		}
	}
}

func TestParseCarrier(t *testing.T) {
	c, err := ParseCarrier("hla")
	if err != nil {
		t.Fatalf("ParseCarrier(\"hla\") error: %v", err)
	}
	if c.Type != grammar.Carrier || c.Vc != "a" {
		t.Errorf("ParseCarrier(\"hla\") = %v, want {Carrier, a}", c)
	}

	c, err = ParseCarrier("hňui")
	if err != nil {
		t.Fatalf("ParseCarrier(\"hňui\") error: %v", err)
	}
	if c.Type != grammar.Phrasal || c.Vc != "ui" {
		t.Errorf("ParseCarrier(\"hňui\") = %v, want {Phrasal, ui}", c)
	}
}

func TestParseCarrier_Rejects(t *testing.T) {
	for _, w := range []string{"", "hl", "a", "xy", "hpa"} {
		if _, err := ParseCarrier(w); err == nil {
			t.Errorf("ParseCarrier(%q) succeeded, want error", w)
		}
	}
}
