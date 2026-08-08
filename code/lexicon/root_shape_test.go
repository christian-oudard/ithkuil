package lexicon

import (
	"sort"
	"testing"

	"github.com/christian-oudard/ithkuil/phonology"
)

// §§8-11 of the phonotactics document enumerate the conjuncts that can
// be a root or an affix. The lexicon is the community spreadsheet
// rather than one of Quijada's documents, so the two can disagree, and
// they do: 453 of the 5,895 roots are shapes no row of §§9-11 admits.
//
// The disagreement is entirely above two consonants, which is the shape
// of a real finding rather than of a transcription slip. §8's grid is
// derived from the pair rules in §§2-7, so a root of one or two
// consonants that our validator accepts is one §8 accepts, and all 670
// of them pass. §§9-11 are independent tables that no rule generates,
// and they are where the lexicon departs.
//
// The departures look systematic rather than scattered, which is why
// this is filed and not fixed. -pf- takes fourteen initials the tables
// do not give it (ţpf, žpf, ḑpf, ẓpf, čpf and so on), and Cml/Cmr is
// another family of the same kind. Either the spreadsheet extended a
// series past what §9 sanctions, or §9's rows are incomplete; deciding
// that is upstream's call and not something to settle by making the
// parser refuse 8% of the vocabulary. See ERRATA.md §§9-11.
//
// Enforcement waits on that. What this pins is the size and shape of
// the gap, so a re-sync that changes it says so.
func TestRoots_AgainstTheRootShapeTables(t *testing.T) {
	lex, err := Load(dataPath("data.json"))
	if err != nil {
		t.Fatal(err)
	}

	byLength := map[int]int{}
	for cr := range lex.Roots {
		if !phonology.RootConjunctLegal(cr) {
			byLength[len([]rune(cr))]++
		}
	}
	want := map[int]int{3: 128, 4: 236, 5: 89}
	for n := 1; n <= 6; n++ {
		if byLength[n] != want[n] {
			t.Errorf("roots of %d consonants outside §§9-11: %d, want %d",
				n, byLength[n], want[n])
		}
	}

	// The affixes are all but clean, and the two that are not share a
	// final -ḑr, which no §9 row gives any initial.
	var affixes []string
	for cs := range lex.Affixes {
		if !phonology.RootConjunctLegal(cs) {
			affixes = append(affixes, cs)
		}
	}
	sort.Strings(affixes)
	if got, w := affixes, []string{"xḑr", "čḑr"}; len(got) != len(w) || (len(got) == 2 && (got[0] != w[0] || got[1] != w[1])) {
		t.Errorf("affixes outside §§9-11 = %v, want %v", got, w)
	}
}
