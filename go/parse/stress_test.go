package parse

import (
	"testing"

	"github.com/coudard/ithkuil/go/grammar"
)

func TestDetectStress(t *testing.T) {
	cases := []struct {
		word string
		want grammar.Stress
	}{
		// Default penultimate when multi-syllable without marks.
		{"malëuţřait", grammar.Penultimate},
		{"mala", grammar.Penultimate},
		// Ultimate: stress on the final syllable.
		{"malí", grammar.Ultimate},
		{"malëuţřáit", grammar.Ultimate},
		// Penultimate via explicit mark.
		{"malëúţřait", grammar.Penultimate},
		// Antepenultimate: stress earlier than penultimate.
		{"málëuţřait", grammar.Antepenultimate},
		// Monosyllabic.
		{"ma", grammar.Monosyllabic},
		{"a", grammar.Monosyllabic},
	}
	for _, c := range cases {
		got := DetectStress(c.word)
		if got != c.want {
			t.Errorf("DetectStress(%q) = %v, want %v", c.word, got, c.want)
		}
	}
}

func TestIsStressedVowel(t *testing.T) {
	stressed := []rune{'á', 'é', 'í', 'ó', 'ú', 'â', 'ê', 'ô', 'û'}
	unstressed := []rune{'a', 'e', 'i', 'o', 'u', 'ä', 'ë', 'ö', 'ü', 'ï'}
	for _, r := range stressed {
		if !IsStressedVowel(r) {
			t.Errorf("IsStressedVowel(%q) = false, want true", r)
		}
	}
	for _, r := range unstressed {
		if IsStressedVowel(r) {
			t.Errorf("IsStressedVowel(%q) = true, want false", r)
		}
	}
}
