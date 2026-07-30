package roman

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/phonology"
)

// TestMalformed_GlottalPatterns asserts that inputs the parser
// shouldn't be able to make sense of produce a loud failure (parse
// error or phonotactic violation) rather than silently picking one
// interpretation.
//
// The double-glottal-stop case surfaces here. Two adjacent glottal
// stops are rejected by the phonotactic validator (rules 1.7/2.1).
// Structurally impossible combinations (two distinct §3.9.1 moved-
// glottal patterns in one body, etc.) hit parse errors at slot
// decode time. The remaining cases with two glottals in canonical
// positions (Vv §3.5.1, Vx DEG0, Vc cases 37-52, §3.6.2 end-marker)
// are legal; the parser accepts them and the renderer emits the
// canonical equivalent.
func TestMalformed_GlottalPatterns(t *testing.T) {
	cases := []struct {
		in        string
		expectErr string // substring expected somewhere in the failure
	}{
		// Adjacent doubled glottal-stop — phonotactic rule 1.7/2.1.
		{"mla''la", "geminate"},
		// Two-distinct-move pattern: glottal lands on two separate
		// inter-vowel positions, second can't be parsed as anything.
		{"mla'la'a", "invalid"},
	}
	for _, c := range cases {
		t.Run(c.in, func(t *testing.T) {
			// Either fullparse returns an error, or the word does not
			// read as phonology in the first place.
			_, parseErr := ParseFormative(c.in)
			phonErr := phonology.CheckText(c.in)
			if parseErr == nil && phonErr == nil {
				t.Errorf("%q: expected parse error or phonotactic violation, got both clean", c.in)
				return
			}
			combined := ""
			if parseErr != nil {
				combined += parseErr.Error()
			}
			if phonErr != nil {
				combined += " " + phonErr.Error()
			}
			if !strings.Contains(combined, c.expectErr) {
				t.Errorf("%q: failure %q did not mention %q", c.in, combined, c.expectErr)
			}
		})
	}
}
