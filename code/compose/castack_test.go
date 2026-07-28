package compose_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/compose"
	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/gloss"
	"github.com/christian-oudard/ithkuil/render"
	"github.com/christian-oudard/ithkuil/validation"
)

// §3.5/§3.7 Ca-stacking: the specialized Vx -üö- marks the following
// Cs as a Ca complex stacked on the Slot VI Ca. The gloss writes it
// "Ca:" plus the same component list Slot VI uses.
//
// Each case goes the whole way round — gloss in, surface out, parse
// back, gloss again — because the two halves can agree on a wrong
// answer and a one-way check would not see it. The surface is also
// validated: a round trip alone cannot tell that what we emitted was
// an impossible word.
func TestCaStack_RoundTrip(t *testing.T) {
	gl := &gloss.Glosser{Canonical: true}
	for _, tc := range []struct{ in, want string }{
		{"ml-Ca:PRX-ERG", "mlalüödo"},
		{"ml-MSS.G-Ca:DPX.COA-ERG", "mlatrüörso"},
		// An all-default stacked Ca writes {Ca}, the same marker Slot
		// VI uses. Whether it means anything is a question about the
		// language; the gloss stays able to say it either way.
		{"ml-Ca:{Ca}-ERG", "mlalüölo"},
		// Slot V, where the affix is reversed to Cs-Vx on the surface.
		{"m-Ca:ASO.PRX-{Ca}-t/1_2", "maldüöllait"},
	} {
		f, err := compose.Formative(tc.in, nil)
		if err != nil {
			t.Errorf("compose(%q): %v", tc.in, err)
			continue
		}
		got := render.Formative(f)
		if got != tc.want {
			t.Errorf("render(compose(%q)) = %q, want %q", tc.in, got, tc.want)
		}
		if r := validation.ValidateWord(got); !r.Valid {
			t.Errorf("%q renders to %q, which our own validator rejects: %v",
				tc.in, got, r.Errors)
		}
		back, err := fullparse.Formative(got)
		if err != nil {
			t.Errorf("parse(%q) from %q: %v", got, tc.in, err)
			continue
		}
		if again := gl.Formative(back); again != tc.in {
			t.Errorf("round trip of %q came back as %q (surface %q)", tc.in, again, got)
		}
	}
}

// The stacking marker must not be read as the Slot VI Ca. If it were,
// affixes after it would land in Slot VII instead of Slot V and the
// scope would silently invert.
func TestCaStack_DoesNotClaimTheSlotVICa(t *testing.T) {
	f, err := compose.Formative("ml-Ca:PRX-t/1-ERG", nil)
	if err != nil {
		t.Fatal(err)
	}
	if len(f.SlotV) != 0 {
		t.Errorf("Ca: token was read as the Slot VI Ca, pushing %d affix(es) into Slot V",
			len(f.SlotV))
	}
	if len(f.SlotVII) != 2 {
		t.Errorf("SlotVII holds %d affixes, want 2 (the stack and t/1)", len(f.SlotVII))
	}
}
