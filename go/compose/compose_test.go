package compose

import (
	"path/filepath"
	"testing"

	"github.com/coudard/ithkuil/go/lexicon"
)

func TestTable_NonEmpty(t *testing.T) {
	// Sanity: the table holds entries for every major category.
	wantCategories := []string{
		"Case/Transrelative", "Case/Appositive",
		"Stem", "Version", "Function", "Specification", "Context",
		"Configuration", "Affiliation", "Perspective", "Extension", "Essence",
		"Valence", "Phase", "Effect", "Level", "Aspect",
		"Mood", "CaseScope", "Illocution", "Validation",
		"Bias", "Register", "CarrierType",
	}
	seen := map[string]bool{}
	for _, e := range Table {
		seen[e.Category] = true
	}
	for _, c := range wantCategories {
		if !seen[c] {
			t.Errorf("category %q missing from Table", c)
		}
	}
}

func TestLookupGrammar(t *testing.T) {
	hits := LookupGrammar("THM")
	if len(hits) != 1 || hits[0].Abbrev != "THM" {
		t.Errorf("LookupGrammar(THM) = %v, want one THM hit", hits)
	}
	if hits[0].Form != "a" {
		t.Errorf("THM form = %q, want \"a\"", hits[0].Form)
	}
}

func TestLookupGrammar_CaseInsensitive(t *testing.T) {
	hits := LookupGrammar("thm")
	if len(hits) != 1 {
		t.Errorf("LookupGrammar(thm) = %v, want one hit", hits)
	}
}

func TestSearchGrammar_Fuzzy(t *testing.T) {
	hits := SearchGrammar("case")
	if len(hits) < 60 {
		// Cases (68) and Case/* category entries should all match.
		t.Errorf("SearchGrammar(case) hit %d, want ≥60", len(hits))
	}
}

func TestLookupForm(t *testing.T) {
	// "a" is THM case (Transrelative). Several other entries also map
	// to "a" (Valence MNO, Aspect RTR, Validation OBS) — they have
	// .Form unset though, since I didn't populate those.
	hits := LookupForm("a")
	if len(hits) < 1 {
		t.Errorf("LookupForm(a) returned %d hits", len(hits))
	}
	var sawTHM bool
	for _, h := range hits {
		if h.Abbrev == "THM" {
			sawTHM = true
		}
	}
	if !sawTHM {
		t.Errorf("LookupForm(a) should include THM; got %v", hits)
	}
}

func TestSearchRoots(t *testing.T) {
	roots, err := lexicon.LoadRoots(filepath.Join("..", "..", "data", "roots.json"))
	if err != nil {
		t.Fatalf("load roots: %v", err)
	}
	// "yellow" should find the root "ml".
	hits := SearchRoots("yellow", roots)
	if len(hits) == 0 {
		t.Fatal("SearchRoots(yellow): no hits")
	}
	var sawML bool
	for _, h := range hits {
		if h.Cr == "ml" {
			sawML = true
		}
	}
	if !sawML {
		t.Errorf("SearchRoots(yellow) should include 'ml'; got %v", hits[:min(3, len(hits))])
	}
}

func TestSearchRoots_DirectCr(t *testing.T) {
	roots, err := lexicon.LoadRoots(filepath.Join("..", "..", "data", "roots.json"))
	if err != nil {
		t.Fatalf("load roots: %v", err)
	}
	hits := SearchRoots("ml", roots)
	if len(hits) == 0 || hits[0].Cr != "ml" || hits[0].Score != 0 {
		t.Errorf("SearchRoots(ml): direct hit should rank first with score 0; got %v",
			hits[:min(3, len(hits))])
	}
}

func TestSearchAffixes(t *testing.T) {
	affixes, err := lexicon.LoadAffixes(filepath.Join("..", "..", "data", "affixes.json"))
	if err != nil {
		t.Fatalf("load affixes: %v", err)
	}
	// "r" affix is NEG (Negation degrees).
	hits := SearchAffixes("r", affixes)
	if len(hits) == 0 {
		t.Fatal("SearchAffixes(r): no hits")
	}
	var sawNEG bool
	for _, a := range hits {
		if a.Cs == "r" && a.Abbrev == "NEG" {
			sawNEG = true
		}
	}
	if !sawNEG {
		t.Errorf("SearchAffixes(r) should include NEG; got %d hits", len(hits))
	}
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
