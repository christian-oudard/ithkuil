package concatenation

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

func cluster(f g.Formative) string {
	if cr, ok := f.Root.(g.CrRoot); ok {
		return cr.Cluster
	}
	return ""
}

func TestNewChain_HeadOnly(t *testing.T) {
	c := New(g.MinimalFormative("ml"))
	if c.Length() != 1 {
		t.Errorf("Length() = %d, want 1", c.Length())
	}
	if c.Semantics() != Compound {
		t.Errorf("Semantics() = %v, want Compound (head alone)", c.Semantics())
	}
}

func TestAddType1(t *testing.T) {
	c := New(g.MinimalFormative("ml")).AddType1(g.MinimalFormative("t"))
	if c.Length() != 2 {
		t.Errorf("Length() = %d, want 2", c.Length())
	}
	deps := c.Type1Dependents()
	if len(deps) != 1 || cluster(deps[0]) != "t" {
		t.Errorf("Type1Dependents() = %v, want one with Cr=t", deps)
	}
	if deps[0].Concat == nil || *deps[0].Concat != g.Type1 {
		t.Errorf("dependent Concat = %v, want Type1", deps[0].Concat)
	}
}

func TestAddType2(t *testing.T) {
	c := New(g.MinimalFormative("ml")).AddType2(g.MinimalFormative("t"))
	deps := c.Type2Dependents()
	if len(deps) != 1 || deps[0].Concat == nil || *deps[0].Concat != g.Type2 {
		t.Errorf("Type2 dependent didn't get Concat=Type2: %v", deps)
	}
}

func TestSemantics_Compound(t *testing.T) {
	c := New(g.MinimalFormative("ml")).AddType1(g.MinimalFormative("t"))
	if c.Semantics() != Compound {
		t.Errorf("all Type 1: Semantics() = %v, want Compound", c.Semantics())
	}
}

func TestSemantics_Coordinated(t *testing.T) {
	c := New(g.MinimalFormative("ml")).AddType2(g.MinimalFormative("t"))
	if c.Semantics() != Coordinated {
		t.Errorf("all Type 2: Semantics() = %v, want Coordinated", c.Semantics())
	}
}

func TestSemantics_Mixed(t *testing.T) {
	c := New(g.MinimalFormative("ml")).
		AddType1(g.MinimalFormative("t")).
		AddType2(g.MinimalFormative("k"))
	if c.Semantics() != Mixed {
		t.Errorf("mix: Semantics() = %v, want Mixed", c.Semantics())
	}
}

func TestFormatives_OrderAndCount(t *testing.T) {
	c := New(g.MinimalFormative("a")).
		AddType1(g.MinimalFormative("b")).
		AddType2(g.MinimalFormative("c"))
	all := c.Formatives()
	if len(all) != 3 {
		t.Fatalf("Formatives() = %d, want 3", len(all))
	}
	if cluster(all[0]) != "a" || cluster(all[1]) != "b" || cluster(all[2]) != "c" {
		t.Errorf("Formatives order: %s %s %s",
			cluster(all[0]), cluster(all[1]), cluster(all[2]))
	}
}

func TestConcatMarker(t *testing.T) {
	t1 := g.Type1
	t2 := g.Type2
	cases := []struct {
		in   *g.ConcatenationStatus
		want string
	}{
		{nil, ""},
		{&t1, "h"},
		{&t2, "hw"},
	}
	for _, c := range cases {
		if got := ConcatMarker(c.in); got != c.want {
			t.Errorf("ConcatMarker(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}
