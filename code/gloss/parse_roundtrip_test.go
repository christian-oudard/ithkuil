package gloss

import (
	"path/filepath"
	"testing"

	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/roman"
)

// TestFormative_FullRoundTrip reads a formative from a gloss string,
// renders it to a romanization, parses that back, and verifies the
// resulting gloss matches the string it started from. This is the
// strongest invariant: Formative ∘ render ∘ fullparse ∘ ParseFormative
// == identity on canonical gloss.
func TestFormative_FullRoundTrip(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	gl := &Glosser{Lex: lex}
	inputs := []string{
		"ml",
		"S2.CPT-ml",
		"ml-ERG",
		"S2.CPT-ml-DYN.OBJ-ERG",
		"ml-DEV/3-ERG",
		"ml-MCS/3",
		"ml-PRG",
		"ml-DIR",
		// Ca-complex components.
		"ml-MSS",
		"ml-MSS.G",
		"ml-MSS.G.RPV",
		"ml-G",   // perspective alone
		"ml-RPV", // essence alone
	}
	for _, in := range inputs {
		t.Run(in, func(t *testing.T) {
			f, err := ParseFormative(in, lex.Affixes)
			if err != nil {
				t.Fatalf("Formative: %v", err)
			}
			surf := roman.Formative(f)
			if surf == "" {
				t.Fatalf("rendered empty")
			}
			parsed, err := roman.ParseFormative(surf)
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
