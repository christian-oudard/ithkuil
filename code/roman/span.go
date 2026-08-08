package roman

import (
	"strings"
	"unicode/utf8"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/slots"
)

// Spellings returns the legal romanizations of one word in preference
// order, the canonical one first. Only formatives have more than one;
// every other class writes a single form, so the slice has one entry.
//
// Word is the right call for a word on its own. This is for a caller
// that has context to bring, which today means Text.
func Spellings(w g.Word) ([]string, error) {
	f, ok := w.(g.Formative)
	if !ok || f.Concat != g.ConcatNone {
		s, err := Word(w)
		if err != nil {
			return nil, err
		}
		return []string{s}, nil
	}
	return slots.Spellings(f), nil
}

// segment is one word of a span together with what separates it from
// the one before. A §3.1.7 chain contributes one segment per link,
// joined by hyphens rather than spaces, because §1.5 reads across a
// link boundary exactly as it reads across a word boundary: §3.1.8
// calls the hyphen "a simple mnemonic indicator", so nothing is paused
// there and the links are said continuously.
type segment struct {
	cands []string
	sep   string
}

// Text writes a whole span, choosing each word's spelling in the
// context of the next.
//
// A word rendered alone takes the shortest spelling it has, which is
// what a dictionary wants and what Word gives. Running text wants
// something else, and §1.5 of the morphology says what:
//
//	When a word ending in a consonant-form [...] is followed in the
//	same breath-group by another word beginning with a consonant-form,
//	it is usually necessary to append a vowel either to the end of the
//	first word or the beginning of the second word, so as to avoid
//	confusion as to which word the word-final and/or word-initial
//	consonants belong to. This is accomplished by ensuring that
//	appropriate word-initial and/or word-final vocalic Slots (e.g.,
//	Slot II, Slot IX) are filled.
//
// §1.2 of the phonotactics makes every word consonant-initial, an
// unwritten glottal stop before a vowel that "must still be
// pronounced", so the antecedent holds at every junction after a
// consonant-final word. The rule therefore reduces to: a word with
// another word after it should end in a vowel if any of its spellings
// does. Quijada's own text bears that out, 15 consonant-final words at
// 386 clause-medial junctions.
//
// Which end takes the vowel is left open by "either ... or", and this
// fills the first word's Slot IX rather than the second word's Slot II.
// A speaker asked about both families of junction preferred it there
// both times, and it is also the end that can be filled without
// changing anything: the Slot IX default is the value an elision
// dropped, so writing it back cannot alter the reading, while Slot II
// governs Stem and Version.
//
// The last word of the span has no junction to repair and keeps its
// short form. It is still asked not to end in a bare -h, which is a
// fact about the word rather than the boundary; see
// phonology.EndsInBareH.
func Text(t g.Text) (string, error) {
	segs := make([]segment, 0, len(t))
	for _, w := range t {
		if chain, ok := w.(*g.Chain); ok {
			for i, f := range chain.Formatives() {
				sep := "-"
				if i == 0 {
					sep = " "
				}
				segs = append(segs, segment{slots.Spellings(f), sep})
			}
			continue
		}
		cands, err := Spellings(w)
		if err != nil {
			return "", err
		}
		segs = append(segs, segment{cands, " "})
	}

	var b strings.Builder
	for i, s := range segs {
		if i > 0 {
			b.WriteString(s.sep)
		}
		b.WriteString(pickInSpan(s.cands, i < len(segs)-1))
	}
	return b.String(), nil
}

// pickInSpan takes the first spelling that suits the position, falling
// back to the canonical one when none does. A word with nothing else to
// offer is written as it is: these are preferences between legal forms,
// never a reason to fail.
func pickInSpan(cands []string, more bool) string {
	for _, c := range cands {
		if more && !endsInVowel(c) {
			continue
		}
		if phonology.EndsInBareH(c) {
			continue
		}
		return c
	}
	return cands[0]
}

func endsInVowel(w string) bool {
	bare, _ := phonology.Strip(phonology.Normalize(w))
	r, _ := utf8.DecodeLastRuneInString(bare)
	return phonology.IsVowel(r)
}
