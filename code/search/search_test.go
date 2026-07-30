package search

import (
	"github.com/christian-oudard/ithkuil/lexicon"
	"strings"
	"testing"
)

// These moved here with the lookup they cover.

func TestCategories(t *testing.T) {
	cats := Categories()
	if len(cats) == 0 {
		t.Fatal("Categories returned empty")
	}
	// Expect at least a few known categories.
	want := map[string]bool{
		"Stem": false, "Version": false, "Function": false,
		"Specification": false, "Context": false,
		"Configuration": false, "Affiliation": false,
		"Case": false,
	}
	for _, c := range cats {
		want[c] = true
	}
	for k, found := range want {
		if !found {
			t.Errorf("Categories missing %q", k)
		}
	}
}

func TestFilter_Exact(t *testing.T) {
	got := Filter("", "ERG", true)
	if len(got) == 0 {
		t.Fatal("Filter(_, ERG, exact) returned no hits")
	}
	for _, e := range got {
		if e.Abbrev != "ERG" {
			t.Errorf("got abbrev %q in exact ERG filter", e.Abbrev)
		}
	}
}

func TestFilter_Fuzzy(t *testing.T) {
	got := Filter("Case", "ergative", false)
	if len(got) == 0 {
		t.Fatal("Filter(Case, ergative) returned no hits")
	}
	foundERG := false
	for _, e := range got {
		if e.Abbrev == "ERG" {
			foundERG = true
		}
	}
	if !foundERG {
		t.Error("Filter(Case, ergative) didn't include ERG")
	}
}

func TestFilter_CategoryOnly(t *testing.T) {
	got := Filter("Stem", "", false)
	if len(got) < 4 {
		t.Errorf("Filter(Stem, _) returned %d entries, want >= 4", len(got))
	}
}

func TestSearchGrammar_EmptyAndExact(t *testing.T) {
	// Empty query → returns the full table (no filter).
	out := SearchGrammar("")
	if len(out) == 0 {
		t.Error("SearchGrammar(empty) returned nothing")
	}
	// Exact match should rank ahead of fuzzy.
	exact := SearchGrammar("ERG")
	if len(exact) == 0 || !strings.EqualFold(exact[0].Abbrev, "ERG") {
		t.Errorf("SearchGrammar(ERG) first = %+v, want exact ERG", exact[0])
	}
}

func TestSearchRoots_EmptyQuery(t *testing.T) {
	hits := SearchRoots("", map[string]lexicon.RootEntry{
		"l": {Cr: "l", Stem1: "linguistic utterance"},
	})
	if len(hits) != 0 {
		t.Errorf("SearchRoots(empty) = %v, want no hits", hits)
	}
}

func TestSearchAffixes_EmptyQuery(t *testing.T) {
	hits := SearchAffixes("", map[string]lexicon.AffixEntry{
		"r": {Cs: "r", Abbrev: "REF"},
	})
	if len(hits) != 0 {
		t.Errorf("SearchAffixes(empty) = %v, want no hits", hits)
	}
}
