package parse

import (
	"testing"

	"github.com/christian-oudard/ithkuil/surface"
)

func TestParseParsingAdjunct(t *testing.T) {
	cases := []struct {
		in   string
		want surface.Stress
	}{
		{"'a'", surface.Monosyllabic},
		{"'e'", surface.Ultimate},
		{"'o'", surface.Penultimate},
		{"'u'", surface.Antepenultimate},
	}
	for _, c := range cases {
		got, err := ParseParsingAdjunct(c.in)
		if err != nil {
			t.Errorf("ParseParsingAdjunct(%q) error: %v", c.in, err)
			continue
		}
		if got.Stress != c.want {
			t.Errorf("ParseParsingAdjunct(%q) = %v, want %v", c.in, got.Stress, c.want)
		}
	}
}

func TestParseParsingAdjunct_Rejected(t *testing.T) {
	// These all look like the adjunct but don't fit the exact shape.
	bad := []string{
		"",     // empty
		"'",    // single glottal
		"''",   // empty body
		"'i'",  // vowel "i" not assigned
		"'ä'",  // umlaut vowel not assigned
		"'aa'", // two-vowel body
		"'a",   // missing trailing glottal
		"a'",   // missing leading glottal
		"a",    // no glottals
		"'a'b", // trailing junk
	}
	for _, w := range bad {
		if _, err := ParseParsingAdjunct(w); err == nil {
			t.Errorf("ParseParsingAdjunct(%q): expected error, got none", w)
		}
	}
}
