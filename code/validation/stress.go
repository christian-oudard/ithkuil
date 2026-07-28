package validation

import (
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
)

// StressError categorizes ways a stress mark can be wrong.
type StressError int

const (
	// DoubleMarkedStress: two accent marks in one word.
	DoubleMarkedStress StressError = iota + 1
	// MarkedDefaultStress: an accent on the default-stress syllable
	// (monosyllabic, or the penult of a multi-syllable word). The
	// surface is unambiguous without the mark.
	MarkedDefaultStress
	// UnrecognizedPlacement: accent on a syllable other than ultimate,
	// penultimate, or antepenultimate.
	UnrecognizedPlacement
)

func (e StressError) Error() string {
	return [...]string{
		"",
		"DoubleMarkedStress",
		"MarkedDefaultStress",
		"UnrecognizedPlacement",
	}[e]
}

// ValidateStress decides which Stress a word is marked for, or
// returns an error if the marking is ill-formed.
func ValidateStress(word string) (parse.Stress, error) {
	syllables, stressIdx, accentCount := phonology.StressPosition(word)

	if accentCount > 1 {
		return 0, DoubleMarkedStress
	}
	hasAccent := accentCount > 0
	if syllables <= 1 {
		if hasAccent {
			return 0, MarkedDefaultStress
		}
		return parse.Monosyllabic, nil
	}
	if !hasAccent {
		return parse.Penultimate, nil
	}
	fromEnd := syllables - 1 - stressIdx
	switch fromEnd {
	case 0:
		return parse.Ultimate, nil
	case 1:
		return 0, MarkedDefaultStress
	case 2:
		return parse.Antepenultimate, nil
	}
	return 0, UnrecognizedPlacement
}
