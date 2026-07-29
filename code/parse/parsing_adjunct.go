package parse

import (
	"fmt"

	"github.com/christian-oudard/ithkuil/phonology"
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
// a parsing aid for the next token, which is why it lives here rather
// than in grammar. Its whole content, the stress of the following
// word, is already recoverable from that word: like the epenthetic -ë-
// or the §4.6.3 \üo-\ prefix, it exists to keep a written word
// unambiguous, not to mean anything.
type ParsingAdjunct struct {
	Stress phonology.Stress
}

// parsingAdjunctVowels maps the four vowels recognized by §2.3 ¶5 to the
// stress they signal for the following word.
var parsingAdjunctVowels = map[string]phonology.Stress{
	"a": phonology.Monosyllabic,
	"e": phonology.Ultimate,
	"o": phonology.Penultimate,
	"u": phonology.Antepenultimate,
}

// ParseParsingAdjunct decodes a §4.8 parsing adjunct word of the shape
// 'V' — a single vowel sandwiched between two glottal stops. The vowel
// signals the stress of the following word per §2.3 paragraph 5. Any
// other shape is rejected; this is a precise three-character word.
func ParseParsingAdjunct(word string) (ParsingAdjunct, error) {
	rs := []rune(word)
	if len(rs) != 3 || rs[0] != '\'' || rs[2] != '\'' {
		return ParsingAdjunct{}, fmt.Errorf("parsing adjunct: expected 'V', got %q", word)
	}
	s, ok := parsingAdjunctVowels[string(rs[1])]
	if !ok {
		return ParsingAdjunct{}, fmt.Errorf("parsing adjunct: vowel %q is not one of a/e/o/u", string(rs[1]))
	}
	return ParsingAdjunct{Stress: s}, nil
}
