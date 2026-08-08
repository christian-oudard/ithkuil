package slots

import (
	"reflect"
	"sort"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
)

// Spellings returns every legal romanization of f that reads back as f,
// in the same preference order FromGrammar ranks by, so the first entry
// is the canonical spelling and the rest are the slack the sources
// leave.
//
// The slack is five decisions, and they are the whole of it: the §3.2
// Ca shortcut, the §3.8.1.2 C_N-into-Ca move, the §3.9.1 moved glottal,
// and whether the Slot II and Slot IX defaults are written or elided.
// FromGrammar chooses among the first three and elides the last two
// whenever it can, which is right for a word standing alone and wrong
// at a junction: §1.5 wants the word to end in a vowel when another
// word follows, and only the Slot IX default can supply one.
//
// That is what this exists for. A per-word renderer cannot see the next
// word, so it offers its spellings instead and roman.Text picks. Four
// corpus formatives in five have more than one.
//
// Every candidate is checked twice, because neither check implies the
// other: phonology.Legal that it can be said, and a parse back to an
// equal Formative that it still says the same thing. A shortening that
// changes the reading is a far worse failure than one that does not
// apply, so nothing is trusted here for being constructed correctly.
func Spellings(f g.Formative) []string {
	type candidate struct {
		text string
		cost cost
	}
	seen := map[string]bool{}
	var out []candidate
	for _, e := range allEncodings {
		base := layoutFor(f, e)
		// Restoring a default is only meaningful where layoutFor elided
		// one, so the two bits index the four combinations of "leave it"
		// and "put it back".
		for restore := 0; restore < 4; restore++ {
			l := base
			if restore&1 != 0 {
				r, ok := f.Root.(g.CrRoot)
				if !ok || l.Vv != "" {
					continue
				}
				l.Vv = parse.SlotIIToVv(g.SlotII{Stem: r.Stem, Version: r.Version})
			}
			if restore&2 != 0 {
				if l.Vc != "" {
					continue
				}
				l.Vc, l.Stress = slotIXFromFinal(f)
			}
			text := Render(l)
			if seen[text] || !phonology.Legal(text) {
				continue
			}
			back, err := Parse(text)
			if err != nil {
				continue
			}
			f2, err := ToGrammar(back)
			if err != nil || !reflect.DeepEqual(f, f2) {
				continue
			}
			seen[text] = true
			out = append(out, candidate{text, romanizationCost(l, e)})
		}
	}
	sort.SliceStable(out, func(i, j int) bool {
		return out[i].cost.better(out[j].cost)
	})
	texts := make([]string, len(out))
	for i, c := range out {
		texts[i] = c.text
	}
	return texts
}
