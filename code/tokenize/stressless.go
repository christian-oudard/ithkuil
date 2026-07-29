package tokenize

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
)

// RenderStressless writes a span with its stress carried by §4.8
// parsing adjuncts instead of by the acute and circumflex.
//
// §2.3 makes pitch accent "the means by which word boundaries may be
// parsed": every syllable before the stressed one is MID, and from the
// stressed syllable to the end of the word there is a single non-MID
// contour, so a listener hears where each word starts and stops. §2.3
// ¶5 gives the adjunct for when that channel is not available — its
// example is singing, where the melody has taken the pitch and pausing
// between every word is unrealistic. The adjunct replaces both halves
// of what the contour did: the glottal stops around it mark a word
// boundary audibly, and the vowel inside says where the stress falls.
//
// So this is not an alternative spelling of the same medium. It is the
// same grammar written for a channel that cannot carry stress, which
// is why it does not conflict with a Formative having one canonical
// surface.
//
// The two forms read back to the same words, which is what
// consumeParsingAdjuncts already does on the way in.
func RenderStressless(t g.Text) (string, error) {
	parts := make([]string, 0, len(t)*2)
	for _, w := range t {
		// A §3.1.7 chain is written as one hyphen-joined word but each
		// link carries its own stress, so one adjunct cannot declare
		// it. Whether a link counts as "any word to be parsed" in the
		// sense of §2.3 ¶5, and may take an adjunct of its own, the
		// source does not say — so refuse rather than invent a
		// spelling. See docs/reference/issues.md.
		if _, isChain := w.(*g.Chain); isChain {
			return "", fmt.Errorf(
				"a concatenation chain carries one stress per link, which a single parsing adjunct cannot declare")
		}
		rom, err := Render(w)
		if err != nil {
			return "", err
		}
		bare, stress := phonology.Strip(rom)
		adjunct, ok := phonology.ParsingAdjunctFor(stress)
		if !ok {
			// No adjunct names this stress, so the bare word is all
			// there is to write; InvalidStress is the only such value
			// and Render does not produce it.
			parts = append(parts, rom)
			continue
		}
		parts = append(parts, adjunct, bare)
	}
	return strings.Join(parts, " "), nil
}
