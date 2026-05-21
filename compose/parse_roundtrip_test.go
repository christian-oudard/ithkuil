package compose

import (
	"testing"

	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/gloss"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/render"
)

// TestParseString_FullRoundTrip composes a formative from a gloss
// string, renders it to surface text, parses that back, and verifies
// the resulting gloss matches the original compose input's gloss.
// This is the strongest invariant: compose ∘ render ∘ fullparse ∘
// gloss == gloss ∘ compose.
func TestParseString_FullRoundTrip(t *testing.T) {
	lex, err := lexicon.LoadDefault()
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	gl := &gloss.Glosser{Lex: lex}
	inputs := []string{
		"ml",
		"S2/CPT-ml",
		"ml-ERG",
		"S2/CPT-ml-DYN/OBJ-ERG",
		"ml-DEV/3-ERG",
		"ml-MCS/3",
		"ml-PRG",
		"ml-DIR",
		// Ca-complex components.
		"ml-MSS",
		"ml-MSS.G",
		"ml-MSS.G.RPV",
		"ml-G",      // perspective alone
		"ml-RPV",    // essence alone
	}
	for _, in := range inputs {
		t.Run(in, func(t *testing.T) {
			f, err := ParseString(in, lex.Affixes)
			if err != nil {
				t.Fatalf("ParseString: %v", err)
			}
			surf := render.Formative(f)
			if surf == "" {
				t.Fatalf("rendered empty")
			}
			parsed, err := fullparse.ParseFormative(surf)
			if err != nil {
				t.Fatalf("re-parse %q: %v", surf, err)
			}
			want := gl.Formative(f)
			got := gl.Formative(parsed)
			if got != want {
				t.Errorf("round-trip gloss mismatch\n  surf:    %s\n  compose: %s\n  reparse: %s",
					surf, want, got)
			}
		})
	}
}
