package parse

import (
	"testing"

	"github.com/christian-oudard/ithkuil/grammar"
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

// The series-3 alternates are the forms taken after a y- or w- glide
// (§1.6). Cases 53-60 repeat series 3 minus vowel-tier 8 with a
// glottal-stop, so form 9 ua / iä becomes u'a / i'ä — NAV. The tier-8
// alternate ië drops out of the range entirely, and i'ë is nothing.
func TestParseCaseSeriesThreeAlternates(t *testing.T) {
	pairs := []struct {
		alt  string
		want grammar.Case
	}{
		{"uä", grammar.APL}, {"uë", grammar.PUR},
		{"üä", grammar.TRA}, {"üë", grammar.DFR},
		{"öë", grammar.TSP}, {"öä", grammar.CMM},
		{"ië", grammar.CMP}, {"iä", grammar.CSD},
		{"u'ä", grammar.LOC}, {"u'ë", grammar.ATD},
		{"ü'ä", grammar.ALL}, {"ü'ë", grammar.ABL},
		{"ö'ë", grammar.IRL}, {"ö'ä", grammar.INV},
		{"i'ä", grammar.NAV},
	}
	for _, p := range pairs {
		if got, ok := ParseCase(p.alt); !ok || got != p.want {
			t.Errorf("ParseCase(%q) = (%v,%v), want (%v,true)",
				p.alt, got, ok, p.want)
		}
	}
	if c, ok := ParseCase("i'ë"); ok {
		t.Errorf("ParseCase(\"i'ë\") = %v, want failure: vowel-tier 8 "+
			"has no case in the 37-68 range", c)
	}
}

func TestParseCaseRejectsJunk(t *testing.T) {
	for _, s := range []string{"", "x", "qq", "abc"} {
		if c, ok := ParseCase(s); ok {
			t.Errorf("ParseCase(%q) = %v, want failure", s, c)
		}
	}
}
