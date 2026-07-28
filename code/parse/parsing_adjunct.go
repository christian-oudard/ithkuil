package parse

import (
	"fmt"

	"github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
)

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
func ParseParsingAdjunct(word string) (grammar.ParsingAdjunct, error) {
	rs := []rune(word)
	if len(rs) != 3 || rs[0] != '\'' || rs[2] != '\'' {
		return grammar.ParsingAdjunct{}, fmt.Errorf("parsing adjunct: expected 'V', got %q", word)
	}
	s, ok := parsingAdjunctVowels[string(rs[1])]
	if !ok {
		return grammar.ParsingAdjunct{}, fmt.Errorf("parsing adjunct: vowel %q is not one of a/e/o/u", string(rs[1]))
	}
	return grammar.ParsingAdjunct{Stress: s}, nil
}
