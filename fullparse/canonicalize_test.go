package fullparse_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/render"
)

// TestCanonicalize asserts that a non-canonical input surface is
// parsed correctly and re-renders as the canonical equivalent. The
// parser accepts every spec-legal surface form, but the renderer
// only emits the canonical one. So fullparse(non-canonical) →
// render is *not* the identity for these inputs — by design.
//
// Each pair is (input_we_accept, canonical_output_we_produce).
func TestCanonicalize(t *testing.T) {
	pairs := []struct {
		in, want string
	}{
		// Consonant-initial long form → Cc shortcut canonical.
		{"mlala", "wamla"},
		{"malëuţřait", "wamëuţřait"},
		// Long-form Slot VIII Mood → Cn→Ca shortcut canonical.
		{"amlalahla", "mlahla"},
		// Default Vv "a" emitted instead of elided.
		{"amlala", "wamla"},
		// §3.9.1 long-form Vc-glottal (cases 37-52). The Cc shortcut
		// still takes precedence — Vr is absent in shortcut form so
		// §3.9.1 can't fire, and the glottal stays in Vc.
		{"mlala'a", "wamla'a"},
	}
	for _, p := range pairs {
		t.Run(p.in, func(t *testing.T) {
			f, err := fullparse.ParseFormative(p.in)
			if err != nil {
				t.Fatalf("ParseFormative(%q): %v", p.in, err)
			}
			got := render.Formative(f)
			if got != p.want {
				t.Errorf("canonicalize(%q) = %q, want %q", p.in, got, p.want)
			}
		})
	}
}
