package parse

import (
	"testing"

	"github.com/coudard/ithkuil/go/grammar"
)

func TestParseCaseRoundTrip(t *testing.T) {
	for _, c := range grammar.AllCases {
		v := grammar.CaseToVc(c)
		got, ok := ParseCase(v)
		if !ok {
			t.Errorf("ParseCase(%q) failed for %s", v, c)
			continue
		}
		if got != c {
			t.Errorf("round trip: %s → %q → %s", c, v, got)
		}
	}
}

func TestParseCaseAlternates(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.Case
	}{
		// Series 3 alternates
		{"uä", grammar.APL},
		{"uë", grammar.PUR},
		{"üä", grammar.TRA},
		{"üë", grammar.DFR},
		{"öë", grammar.TSP},
		{"öä", grammar.CMM},
		{"ië", grammar.CMP},
		{"iä", grammar.CSD},
		// ST1 alternates
		{"u'ä", grammar.LOC},
		{"i'ë", grammar.NAV},
	}
	for _, c := range cases {
		got, ok := ParseCase(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseCase(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestParseCaseAcceptsAccents(t *testing.T) {
	// Stressed vowels should normalize before lookup.
	cases := []struct {
		in   string
		want grammar.Case
	}{
		{"á", grammar.THM},
		{"é", grammar.ABS},
		{"ô", grammar.EFF},
	}
	for _, c := range cases {
		got, ok := ParseCase(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseCase(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestParseCaseRejectsJunk(t *testing.T) {
	for _, s := range []string{"", "x", "qq", "abc"} {
		if c, ok := ParseCase(s); ok {
			t.Errorf("ParseCase(%q) = %v, want failure", s, c)
		}
	}
}
