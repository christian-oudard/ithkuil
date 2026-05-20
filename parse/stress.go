package parse

import "github.com/christian-oudard/ithkuil/surface"

// Stress is a type alias for surface.Stress. The orthographic
// observation lives at the surface layer (where the bytes of the
// stress mark are interpreted); this alias keeps fullparse and the
// slot-level parsers compiling against the existing type name while
// new code should reach for surface.Stress directly.
type Stress = surface.Stress

const (
	Monosyllabic    = surface.Monosyllabic
	Penultimate     = surface.Penultimate
	Ultimate        = surface.Ultimate
	Antepenultimate = surface.Antepenultimate
)

// stressedVowels are the acute (á é í ó ú) and circumflex (â ê ô û)
// forms — these mark the stressed syllable. The set is asymmetric
// because circumflex doubles as the umlaut form and "î" isn't used:
// "i" pairs with the diaeresis hiatus marker "ï" instead.
var stressedVowels = map[rune]bool{
	'á': true, 'é': true, 'í': true, 'ó': true, 'ú': true,
	'â': true, 'ê': true, 'ô': true, 'û': true,
}

// IsStressedVowel reports whether a rune carries a stress mark.
func IsStressedVowel(r rune) bool { return stressedVowels[r] }
