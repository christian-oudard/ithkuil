package slots

import (
	"fmt"
	"strings"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/inventory"
)

// layoutFields is the Layout as a name-to-written-form map, so a test
// can ask which slot a change landed in without naming each field.
func layoutFields(l Layout) map[string]string {
	return map[string]string{
		"Cc": l.Cc, "Vv": l.Vv, "Cr": l.Cr, "Vr": l.Vr,
		"Ca": l.Ca, "Vn": l.Vn, "Cn": l.Cn, "Vc": l.Vc,
		"SlotV":   fmt.Sprint(l.SlotV),
		"SlotVII": fmt.Sprint(l.SlotVII),
	}
}

// TestPositions_CategoriesLandInTheirSlot is what makes Positions a
// claim about the language rather than a comment. The inventory holds
// one minimal word per grammatical value, each differing from a fixed
// baseline in that value alone, so writing one must change the slot
// this file says carries it. If a table moves a category to another
// slot, the declaration stops matching and this fails.
//
// A sample may change more than the declared field: the verbal
// baseline differs from the nominal one in Slot IX and in stress, so a
// Mood sample moves those too. The claim is that the declared field is
// among the ones that moved, not that it is the only one.
func TestPositions_CategoriesLandInTheirSlot(t *testing.T) {
	// Category to the fields Positions says can carry it. Case groups
	// are named "Case/Transrelative" and the rest in search.Table, and
	// Positions names the family, so match on the prefix.
	want := map[string][]string{}
	for _, p := range Positions() {
		for _, c := range p.Categories {
			want[c] = append(want[c], p.Field)
		}
	}

	base := layoutFields(FromGrammar(g.MinimalFormative(inventory.Cr)))
	seen := map[string]bool{}
	for _, s := range inventory.Samples() {
		f, ok := s.Word.(g.Formative)
		if !ok {
			continue
		}
		family, _, _ := strings.Cut(s.Category, "/")
		fields, declared := want[family]
		if !declared {
			t.Errorf("category %q has samples but no position declares it", s.Category)
			continue
		}
		seen[family] = true

		// An unwritten or unmarked value is its category's default and
		// changes no letters, so there is no slot to find it in.
		if s.Unwritten || s.Unmarked {
			continue
		}
		got := layoutFields(FromGrammar(f))
		var moved []string
		for name, v := range got {
			if v != base[name] {
				moved = append(moved, name)
			}
		}
		// Slot X is carried by stress, so a relation change may move no
		// conjunct at all.
		if len(moved) == 0 && f.Final != nil {
			continue
		}
		hit := false
		for _, w := range fields {
			for _, m := range moved {
				if m == w {
					hit = true
				}
			}
		}
		if !hit {
			t.Errorf("%s/%s changed %v, but Positions says %s is written in %v",
				s.Category, s.Abbrev, moved, family, fields)
		}
	}

	for c := range want {
		if !seen[c] {
			t.Errorf("Positions declares category %q, which the inventory never exercises", c)
		}
	}
}

// TestPositions_Shape pins the frame a builder lays out against: every
// position names a slot, and every slot of the formative appears.
func TestPositions_Shape(t *testing.T) {
	slots := map[string]bool{}
	for _, p := range Positions() {
		if p.Slot == "" || p.Name == "" {
			t.Errorf("position with no slot or name: %+v", p)
		}
		if len(p.Categories) == 0 && p.Note == "" {
			t.Errorf("%s holds no category and says why: %+v", p.Slot, p)
		}
		slots[p.Slot] = true
	}
	for _, want := range []string{"I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"} {
		if !slots[want] {
			t.Errorf("no position for Slot %s", want)
		}
	}
}
