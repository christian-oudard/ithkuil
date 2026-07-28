package grammar

import (
	"github.com/christian-oudard/ithkuil/surface"
)

// ParsingAdjunct (§4.8 / §2.3 ¶5) is a one-syllable adjunct of the
// shape 'V', a single vowel between two glottal stops, placed before
// any word whose syllabic stress the speaker wants to mark explicitly.
// It exists for the unusual case (the spec gives "singing a song" as
// the canonical example) where pitch-accent and the acute/circumflex
// stress diacritics are unavailable as cues to word boundary and
// stress placement.
//
// The vowel in the adjunct indicates the stress of the next word per
// §2.3 paragraph 5:
//
//	'a' → Monosyllabic
//	'e' → Ultimate
//	'o' → Penultimate
//	'u' → Antepenultimate
//
// The adjunct carries no grammatical content of its own — it is purely
// a parsing aid for the next token.
type ParsingAdjunct struct {
	Stress surface.Stress
}
