package compose_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/compose"
	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/render"
	"github.com/christian-oudard/ithkuil/validation"
)

// §3.9.2's seven case-bearing affixes. Each has two Cs increments, one
// for cases 1-36 and one for 37-68; the Vx series then picks the group
// within that range and its degree the case within the group.
//
// The gloss writes "KIND:CASE" — ACC1 through ACC3 for the
// case-accessors, IAC1 through IAC3 for the inverse ones, CST for
// case-stacking.
func TestAccessor_RoundTrip(t *testing.T) {
	gl := &gloss.Glosser{Canonical: true}
	for _, in := range []string{
		"ml-CST:ERG-ERG",     // case-stacking, low increment
		"ml-ACC1:INS-ERG",    // case-accessor, series 1
		"ml-ACC2:ALL-ERG",    // second kind, one our markdown had lost
		"ml-IAC3:PRP-ERG",    // inverse Type-3, high increment
		"ml-CST:THM-t/1-ERG", // alongside an ordinary affix
	} {
		f, err := compose.Formative(in, nil)
		if err != nil {
			t.Errorf("compose(%q): %v", in, err)
			continue
		}
		w := render.Formative(f)
		if r := validation.ValidateWord(w); !r.Valid {
			t.Errorf("%q renders to %q, which our own validator rejects: %v", in, w, r.Errors)
		}
		back, err := fullparse.Formative(w)
		if err != nil {
			t.Errorf("parse(%q) from %q: %v", w, in, err)
			continue
		}
		if again := gl.Formative(back); again != in {
			t.Errorf("round trip of %q came back as %q (surface %q)", in, again, w)
		}
	}
}

// Every one of the 68 cases has to survive the trip through a Vx
// series and degree and back. The four groups above case 36 hold only
// eight cases each, skipping the ü-tier, so the arithmetic there is
// not a plain modulo and is the part most likely to be wrong.
func TestAccessor_EveryCaseEncodes(t *testing.T) {
	seen := map[string]g.Case{}
	for _, c := range g.AllCases {
		series, degree, high, ok := g.AccessorVx(c)
		if !ok {
			t.Errorf("%v has no case-accessor encoding", c)
			continue
		}
		if degree == 8 && high {
			t.Errorf("%v encodes with the ü-tier, which these groups do not use", c)
		}
		back, ok := g.AccessorCase(series, degree, high)
		if !ok {
			t.Errorf("%v encodes as (series %d, degree %d, high %v), which does not decode",
				c, series, degree, high)
			continue
		}
		if back != c {
			t.Errorf("%v round-tripped to %v via (series %d, degree %d, high %v)",
				c, back, series, degree, high)
		}
		key := string(rune(series)) + string(rune(degree)) + map[bool]string{true: "h", false: "l"}[high]
		if prev, dup := seen[key]; dup {
			t.Errorf("%v and %v both encode as (series %d, degree %d, high %v)",
				prev, c, series, degree, high)
		}
		seen[key] = c
	}
}

// The fourteen Cs increments must be distinct, and each must decode
// back to the kind and range it was built from. Three of the seven
// kinds were missing from our markdown copy of the table (issues.md
// G34), so this guards the transcription as much as the code.
func TestAccessor_CsIncrementsAreDistinct(t *testing.T) {
	seen := map[string]string{}
	for _, kind := range g.AllAccessorKinds {
		for _, high := range []bool{false, true} {
			cs := g.AccessorCs(kind, high)
			if cs == "" {
				t.Errorf("%v has no Cs increment for high=%v", kind, high)
				continue
			}
			if prev, dup := seen[cs]; dup {
				t.Errorf("Cs %q is claimed by both %s and %v", cs, prev, kind)
			}
			seen[cs] = kind.String()
			k, h, ok := g.ParseAccessorCs(cs)
			if !ok || k != kind || h != high {
				t.Errorf("ParseAccessorCs(%q) = (%v, %v, %v), want (%v, %v, true)",
					cs, k, h, ok, kind, high)
			}
		}
	}
	if len(seen) != 14 {
		t.Errorf("got %d distinct Cs increments, want 14 (seven kinds x two ranges)", len(seen))
	}
}
