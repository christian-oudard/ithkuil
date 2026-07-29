package phonology

import "fmt"

// A parsing adjunct (§4.8, §2.3 ¶5) is a one-syllable word of the
// shape 'V — a single vowel between two glottal stops — placed before
// another word to declare that word's stress.
//
// It is phonology rather than grammar. It says nothing about what the
// following word means, only how it is said, and the spec introduces
// it for the case where the ordinary cues are unavailable: "In unusual
// situations (e.g. singing a song) when pitch-accent is unavailable or
// undesirable as a means of parsing word boundaries". Its whole
// content is recoverable from the word it precedes, so a parser
// consumes it and it never becomes a word of its own.
//
// The vowel names the stress per §2.3 ¶5:
//
//	'a' → Monosyllabic
//	'e' → Ultimate
//	'o' → Penultimate
//	'u' → Antepenultimate

// parsingAdjunctVowels maps the four vowels §2.3 ¶5 recognizes to the
// stress each declares for the following word.
var parsingAdjunctVowels = map[rune]Stress{
	'a': Monosyllabic,
	'e': Ultimate,
	'o': Penultimate,
	'u': Antepenultimate,
}

// ParsingAdjunct decodes a word of the shape 'V and returns the stress
// it declares. ok is false for any other shape; this is a precise
// three-character word.
func ParsingAdjunct(word string) (Stress, bool) {
	rs := []rune(word)
	if len(rs) != 3 || rs[0] != '\'' || rs[2] != '\'' {
		return 0, false
	}
	s, ok := parsingAdjunctVowels[rs[1]]
	return s, ok
}

// DeclareStress applies the stress a parsing adjunct declared to the
// word that follows it.
//
// An unmarked word takes the declared stress, which is the case the
// adjunct exists for: §2.3 ¶5 offers it precisely when the diacritics
// are not being written. A word that does carry a mark must agree with
// the declaration, and a disagreement is an error rather than a
// silently preferred reading — the two cues are saying different
// things about one word, and neither of them is more authoritative.
func DeclareStress(word string, declared Stress) (string, error) {
	bare, marked := Strip(word)
	if marked == InvalidStress {
		return "", fmt.Errorf("parsing adjunct declares %v for %q, which is not marked legibly", declared, word)
	}
	if marked != declared {
		// Monosyllabic is not a placement, so a one-syllable word
		// cannot carry a contradicting mark; anything else can.
		if SyllableCount(bare) > 1 && marked != Penultimate {
			return "", fmt.Errorf(
				"parsing adjunct declares %v stress but %q is written with %v", declared, word, marked)
		}
	}
	return Apply(bare, declared), nil
}

// ParsingAdjunctFor returns the adjunct that declares the given
// stress, the inverse of ParsingAdjunct.
//
// This is what a stressless spelling needs: §2.3 makes pitch accent
// the means by which word boundaries are found, and §2.3 ¶5 offers the
// adjunct for when that channel is unavailable — singing, where the
// melody has taken the pitch and pausing between words is unrealistic.
// The adjunct restores both halves of what the contour carried: the
// glottal stops around it mark where a word begins, and the vowel says
// where its stress falls.
func ParsingAdjunctFor(s Stress) (string, bool) {
	for vowel, declared := range parsingAdjunctVowels {
		if declared == s {
			return "'" + string(vowel) + "'", true
		}
	}
	return "", false
}
