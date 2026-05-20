package validation

import (
	"unicode/utf8"

	"github.com/christian-oudard/ithkuil/surface"
)

// ValidateWord runs the full battery of phonotactic checks on a single
// word: stress placement, each consonant cluster (initial/medial/final
// per its position), and each vowel sequence. Returns a Result whose
// Errors slice collects every violation.
func ValidateWord(word string) Result {
	if word == "" {
		return Result{Valid: true}
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
