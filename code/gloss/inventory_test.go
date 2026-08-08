package gloss_test

import (
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/inventory"
	"github.com/christian-oudard/ithkuil/lexicon"
)

// The gloss arm is swept over the whole inventory the way roman is, and
// deliberately on its own. Nothing here reads or writes a romanization:
// if the two sweeps shared a path, a value both arms lost would pass
// both. See package inventory for how "every value" is kept honest.

func inventoryGlosser(t *testing.T) *gloss.Glosser {
	t.Helper()
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lexicon: %v", err)
	}
	return &gloss.Glosser{Lex: lex}
}

// TestInventory_GlossNamesTheValue requires the gloss of a marked value
// to contain that value's abbreviation. This is the property a reader
// actually relies on: a gloss is only useful if the label in it is the
// one the tables use, and search finds it.
//
// A default is exempt, and the exemption is the check. The gloss shows
// what was chosen, so a formative at THM in S1 and PRC glosses to its
// root alone. Which values are defaults is recorded in the sample, so a
// marked value that glosses to nothing fails here rather than being
// quietly waved through.
func TestInventory_GlossNamesTheValue(t *testing.T) {
	gl := inventoryGlosser(t)
	for _, s := range inventory.Samples() {
		out := gl.Word(s.Word, g.Text{s.Word}, 0)
		if s.Unmarked {
			continue
		}
		if !strings.Contains(out, s.Abbrev) {
			t.Errorf("%s/%s glosses to %q, which does not name it",
				s.Category, s.Abbrev, out)
		}
	}
}

// TestInventory_GlossReadsBack requires the canonical gloss to be an
// input to gloss.ParseWord that gives back the grammar it was written
// from. Comparing values rather than gloss strings is what makes it a
// test of both directions at once: a gloss that drops a value and a
// parser that invents the default agree on the string and disagree on
// the grammar.
func TestInventory_GlossReadsBack(t *testing.T) {
	gl := inventoryGlosser(t)
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lexicon: %v", err)
	}
	for _, s := range inventory.Samples() {
		out := gl.Word(s.Word, g.Text{s.Word}, 0)
		if out == "" {
			t.Errorf("%s/%s glosses to nothing", s.Category, s.Abbrev)
			continue
		}
		back, err := gloss.ParseWord(out, lex)
		if err != nil {
			t.Errorf("%s/%s -> %q: %v", s.Category, s.Abbrev, out, err)
			continue
		}
		if !reflect.DeepEqual(back, s.Word) {
			t.Errorf("%s/%s -> %q came back changed\n  sent %#v\n  got  %#v",
				s.Category, s.Abbrev, out, s.Word, back)
		}
	}
}

// TestInventory_GlossDistinctWithinCategory requires the values of one
// category to gloss differently from each other. Two values sharing a
// label is not something the round trip can report as such: whichever
// the parser resolves the label to satisfies one sample, and only the
// other looks broken, with no hint of what it collided with.
//
// The defaults are skipped rather than compared. Every category has at
// most one, and it glosses to the bare baseline along with every other
// category's default, so including them would report a collision that
// is the intended behaviour.
func TestInventory_GlossDistinctWithinCategory(t *testing.T) {
	gl := inventoryGlosser(t)
	seen := map[string]map[string]string{} // category -> gloss -> abbrev
	for _, s := range inventory.Samples() {
		if s.Unmarked {
			continue
		}
		out := gl.Word(s.Word, g.Text{s.Word}, 0)
		if seen[s.Category] == nil {
			seen[s.Category] = map[string]string{}
		}
		if prev, ok := seen[s.Category][out]; ok {
			t.Errorf("%s: %s and %s both gloss to %q", s.Category, prev, s.Abbrev, out)
			continue
		}
		seen[s.Category][out] = s.Abbrev
	}
}

// TestInventory_GlossPairsAreDistinct is the same property over
// combinations: the gloss has to be injective, so two formatives
// glossing to one line must be the same grammar.
//
// This is the sweep the Slot V against Slot VII collapse would have
// failed. The same affix means different things in the two slots — in
// Slot V it applies to the stem alone, in Slot VII it has scope over
// the whole C_A — and position relative to C_A was the only thing
// saying which. An all-default C_A glossed to nothing, so both wrote
// the same line and compose could not tell them apart on the way back.
// One value at a time never reaches it: the collision needs an affix
// and a default C_A at once, and AffixSlot is not a value in any
// published table.
func TestInventory_GlossPairsAreDistinct(t *testing.T) {
	gl := inventoryGlosser(t)
	seen := map[string]inventory.Combination{}
	for _, c := range inventory.Pairs() {
		out := gl.Word(c.Word, g.Text{c.Word}, 0)
		prev, ok := seen[out]
		if !ok {
			seen[out] = c
			continue
		}
		if !reflect.DeepEqual(prev.Word, c.Word) {
			t.Errorf("%q is the gloss of two different formatives\n  %s + %s\n  %s + %s",
				out, prev.A, prev.B, c.A, c.B)
		}
	}
	if len(seen) == 0 {
		t.Fatal("no combination was glossed; the test is not exercising anything")
	}
	t.Logf("%d distinct glosses", len(seen))
}
