package parse

import "github.com/christian-oudard/ithkuil/phonology"

// Stress is a type alias for phonology.Stress. The orthographic
// observation lives at the romanization layer (where the bytes of the
// stress mark are interpreted); this alias keeps fullparse and the
// slot-level parsers compiling against the existing type name while
// new code should reach for phonology.Stress directly.
type Stress = phonology.Stress

const (
	Monosyllabic    = phonology.Monosyllabic
	Penultimate     = phonology.Penultimate
	Ultimate        = phonology.Ultimate
	Antepenultimate = phonology.Antepenultimate
)

// stressedVowels are the acute (á é í ó ú) and circumflex (â ê ô û)
// forms — these mark the stressed syllable. The set is asymmetric
// because circumflex doubles as the stressed-umlaut form (â=stressed ä,
// etc.); "i" has no umlaut, so it has no circumflex form either.
var stressedVowels = map[rune]bool{
	'á': true, 'é': true, 'í': true, 'ó': true, 'ú': true,
	'â': true, 'ê': true, 'ô': true, 'û': true,
}

// IsStressedVowel reports whether a rune carries a stress mark.
func IsStressedVowel(r rune) bool { return stressedVowels[r] }
