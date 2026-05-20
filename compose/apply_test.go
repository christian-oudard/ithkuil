package compose

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

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

func TestApplyFlag_Stem(t *testing.T) {
	f := g.MinimalFormative("ml")
	if err := ApplyFlag(&f, "S2"); err != nil {
		t.Fatalf("ApplyFlag S2: %v", err)
	}
	cr, ok := f.Root.(g.CrRoot)
	if !ok || cr.Stem != g.S2 {
		t.Errorf("Stem = %v, want S2", cr.Stem)
	}
}

func TestApplyFlag_Version(t *testing.T) {
	f := g.MinimalFormative("ml")
	if err := ApplyFlag(&f, "CPT"); err != nil {
		t.Fatalf("ApplyFlag CPT: %v", err)
	}
	cr := f.Root.(g.CrRoot)
	if cr.Version != g.CPT {
		t.Errorf("Version = %v, want CPT", cr.Version)
	}
}

func TestApplyFlag_Case(t *testing.T) {
	f := g.MinimalFormative("ml")
	if err := ApplyFlag(&f, "ERG"); err != nil {
		t.Fatalf("ApplyFlag ERG: %v", err)
	}
	un, ok := f.Final.(g.UnframedNominal)
	if !ok || un.Case != g.ERG {
		t.Errorf("Final = %v, want UnframedNominal{ERG}", f.Final)
	}
}

func TestApplyFlag_Illocution(t *testing.T) {
	f := g.MinimalFormative("ml")
	if err := ApplyFlag(&f, "DIR"); err != nil {
		t.Fatalf("ApplyFlag DIR: %v", err)
	}
	uv, ok := f.Final.(g.UnframedVerbal)
	if !ok {
		t.Fatalf("Final = %v, want UnframedVerbal{DIR}", f.Final)
	}
	if _, ok := uv.Vk.(g.Directive); !ok {
		t.Errorf("Vk = %v, want Directive", uv.Vk)
	}
}

func TestApplyFlag_Stress(t *testing.T) {
	f := g.MinimalFormative("ml")
	// PEN is the default, so applying it should be a no-op error or noop.
	// ULT, ANT, MON change Final.
	if err := ApplyFlag(&f, "ULT"); err != nil {
		t.Fatalf("ApplyFlag ULT: %v", err)
	}
	if _, ok := f.Final.(g.UnframedVerbal); !ok {
		t.Errorf("ULT didn't produce UnframedVerbal: %v", f.Final)
	}
}

func TestApplyFlag_UnknownReturnsError(t *testing.T) {
	f := g.MinimalFormative("ml")
	if err := ApplyFlag(&f, "QQQ"); err == nil {
		t.Error("ApplyFlag(QQQ) returned nil error")
	}
}

func TestApplyFlag_StemOnNonCrErrors(t *testing.T) {
	// Build a CsRoot formative and try applying S2.
	f := g.MinimalFormative("ml")
	f.Root = g.CsRoot{Cs: "ml", Degree: 5, Version: g.PRC, Function: g.STA, Context: g.EXS}
	err := ApplyFlag(&f, "S2")
	if err == nil {
		t.Error("ApplyFlag S2 on CsRoot didn't error")
	}
}
