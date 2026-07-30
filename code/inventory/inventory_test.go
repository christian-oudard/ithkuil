package inventory_test

import (
	"sort"
	"testing"

	"github.com/christian-oudard/ithkuil/inventory"
	"github.com/christian-oudard/ithkuil/search"
)

// TestSamples_CoverTheInventory is what lets the sweeps in roman and
// gloss say "every grammatical value" instead of "the ones we thought
// of". search.Table is the full inventory, and search's own tests hold
// it against data.json, so this closes the chain: a value in the
// published tables reaches the two arms, or one of these three tests
// fails.
//
// Both directions are checked. A missing sample is an untested value; a
// sample for something not in the inventory is a value invented here,
// which would pass a sweep while proving nothing.
func TestSamples_CoverTheInventory(t *testing.T) {
	got := map[string]bool{}
	for _, s := range inventory.Samples() {
		key := s.Category + "/" + s.Abbrev
		if got[key] {
			t.Errorf("two samples for %s", key)
		}
		got[key] = true
	}
	want := map[string]bool{}
	for _, e := range search.Table {
		want[e.Category+"/"+e.Abbrev] = true
	}
	var missing, extra []string
	for k := range want {
		if !got[k] {
			missing = append(missing, k)
		}
	}
	for k := range got {
		if !want[k] {
			extra = append(extra, k)
		}
	}
	sort.Strings(missing)
	sort.Strings(extra)
	if len(missing) > 0 {
		t.Errorf("no sample carries these (%d): %v", len(missing), missing)
	}
	if len(extra) > 0 {
		t.Errorf("samples for values not in the inventory (%d): %v", len(extra), extra)
	}
}

// TestSamples_WordIsSet guards against a sample whose carrier was never
// filled in, which would make a sweep pass on a nil word.
func TestSamples_WordIsSet(t *testing.T) {
	for _, s := range inventory.Samples() {
		if s.Word == nil {
			t.Errorf("%s/%s has no carrier word", s.Category, s.Abbrev)
		}
	}
}
