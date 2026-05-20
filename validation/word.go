package validation

import (
	"fmt"
	"strings"
	"unicode/utf8"

	"github.com/christian-oudard/ithkuil/surface"
)

// ithkuilRunes is the set of characters that may appear in well-formed
// Ithkuil V4 surface text — the 31 consonants, the 9 base vowels and
// their diacritic variants (dieresis, acute, circumflex, grave), the
// glottal stop, and the concatenation hyphen.
var ithkuilRunes = func() map[rune]bool {
	m := make(map[rune]bool)
	for _, r := range "pbtdkgfvţḑszšžçxhļcẓčjmnňrlwyř" {
		m[r] = true
	}
	for _, r := range "aäeëiïoöuü" {
		m[r] = true
	}
	for _, r := range "áéíóú" {
		m[r] = true
	}
	for _, r := range "âêîôû" {
		m[r] = true
	}
	for _, r := range "àèìòù" {
		m[r] = true
	}
	for _, r := range "ǎěǐǒǔ" {
		m[r] = true
	}
	m['\''] = true
	m['-'] = true
	return m
}()

// ValidateChars reports any character in word that isn't part of the
// V4 alphabet (consonants, vowels with diacritic variants, glottal,
// hyphen). The error names each offending rune with its codepoint.
func ValidateChars(word string) Result {
	var bad []rune
	for _, r := range word {
		if !ithkuilRunes[r] {
			bad = append(bad, r)
		}
	}
	if len(bad) == 0 {
		return Result{Valid: true}
	}
	parts := make([]string, 0, len(bad))
	for _, r := range bad {
		parts = append(parts, fmt.Sprintf("%q (U+%04X)", r, r))
	}
	return Result{
		Valid: false,
		Errors: []Error{{
			Rule:    "chars",
			Cluster: word,
			Reason:  "non-Ithkuil characters: " + strings.Join(parts, ", "),
		}},
	}
}

// ValidateWord runs the full battery of phonotactic checks on a single
// word: stress placement, each consonant cluster (initial/medial/final
// per its position), and each vowel sequence. Returns a Result whose
// Errors slice collects every violation.
func ValidateWord(word string) Result {
	if word == "" {
		return Result{Valid: true}
	}

	// Non-Ithkuil characters mean no other check can be trusted —
	// short-circuit so callers see the chars error alone, not a pile of
	// downstream cluster/stress complaints derived from garbage input.
	if cr := ValidateChars(word); !cr.Valid {
		return cr
	}

	var errs []Error

	// Stress validation.
	if _, err := ValidateStress(word); err != nil {
		if se, ok := err.(StressError); ok {
			errs = append(errs, Error{
				Rule:    "stress",
				Cluster: word,
				Reason:  se.Error(),
			})
		}
	}

	// Walk conjuncts, classifying each as a consonant cluster or vowel
	// sequence and tracking position.
	conjs := surface.SplitConjuncts(word)
	for i, c := range conjs {
		if c == "" {
			continue
		}
		firstRune, _ := utf8.DecodeRuneInString(c)
		if surface.IsVowel(firstRune) {
			res := ValidateVowelSequence(c)
			if !res.Valid {
				errs = append(errs, res.Errors...)
			}
			continue
		}
		pos := Medial
		if i == 0 {
			pos = Initial
		} else if i == len(conjs)-1 {
			pos = Final
		}
		res := ValidateClusterAt(pos, c)
		if !res.Valid {
			errs = append(errs, res.Errors...)
		}
	}

	if len(errs) == 0 {
		return Result{Valid: true}
	}
	return Result{Valid: false, Errors: errs}
}
