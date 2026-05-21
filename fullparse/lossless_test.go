package fullparse_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/render"
)

// TestLossless_RoundTrip drives parse → render and asserts the surface
// is reproduced verbatim. Each input pair exercises a different
// orthographic choice that grammar.SurfaceHints captures:
//
//   - Cc-shortcut form vs long form
//   - §3.8.1.2 Cn→Ca shortcut vs long form
//   - §3.9.1 moved-glottal form vs canonical Vc-glottal
//   - default Vv "a" elided vs kept
//
// The canonical short and the explicit long form should both survive a
// parse → render cycle without drift.
func TestLossless_RoundTrip(t *testing.T) {
	cases := []string{
		// Long form (no Cn→Ca shortcut) ↔ canonical short.
		"amlalahla",
		"mlahla",
		// §3.9.1: canonical Vc-glottal ↔ moved.
		"mlala'a",
		"mla'la",
		// Cc-shortcut form vs corresponding long form.
		"amlal",
		// Default Vv "a" kept (long) vs elided (short).
		"amlala",
		"mlala",
		// §3.6.2 shortcut + Slot V with end-marker glottal.
		"wamla'r",
		"wamla're",
		// Concat prefixes (h / hw, non-shortcut).
		"hamlala",
		"hwamlala",
		// Cc shortcut forms paired with monosyllabic verbal default.
		"waml",
		"yuml",
		"waiml",
		"hlaml",
		// Canonical Ithkuil test word.
		"malëuţřait",
		// Spec number examples.
		"ksalirsa",
		"cpalörs",
	}
	for _, w := range cases {
		t.Run(w, func(t *testing.T) {
			f, err := fullparse.ParseFormative(w)
			if err != nil {
				t.Fatalf("ParseFormative(%q): %v", w, err)
			}
			got := render.Formative(f)
			if got != w {
				t.Errorf("round-trip drift: parse(%q) → render = %q", w, got)
			}
		})
	}
}

// TestLossless_NilSurface confirms that clearing the Surface hint on a
// parsed Formative falls back to the canonical-defaults render path.
// This is the "programmatic Formative" path: callers who build a
// Formative from scratch leave Surface nil and get canonical.
func TestLossless_NilSurface(t *testing.T) {
	cases := []struct {
		in        string
		canonical string
	}{
		// Long form → canonical short.
		{"amlalahla", "mlahla"},
		// Un-moved glottal → moved canonical.
		{"mlala'a", "mla'la"},
	}
	for _, c := range cases {
		t.Run(c.in, func(t *testing.T) {
			f, err := fullparse.ParseFormative(c.in)
			if err != nil {
				t.Fatalf("ParseFormative(%q): %v", c.in, err)
			}
			f.Surface = nil // simulate a programmatic build
			got := render.Formative(f)
			if got != c.canonical {
				t.Errorf("canonical render: got %q, want %q", got, c.canonical)
			}
		})
	}
}
