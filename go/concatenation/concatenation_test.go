package concatenation

import (
	"testing"

	g "github.com/coudard/ithkuil/go/grammar"
)

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
	if len(deps) != 1 || deps[0].SlotIII != "t" {
		t.Errorf("Type1Dependents() = %v, want one with Cr=t", deps)
	}
	// SlotI of the dependent should be set to Type1.
	if deps[0].SlotI == nil || *deps[0].SlotI != g.Type1 {
		t.Errorf("dependent SlotI = %v, want Type1", deps[0].SlotI)
	}
}

func TestAddType2(t *testing.T) {
	c := New(g.MinimalFormative("ml")).AddType2(g.MinimalFormative("t"))
	deps := c.Type2Dependents()
	if len(deps) != 1 || deps[0].SlotI == nil || *deps[0].SlotI != g.Type2 {
		t.Errorf("Type2 dependent didn't get SlotI=Type2: %v", deps)
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
	if all[0].SlotIII != "a" || all[1].SlotIII != "b" || all[2].SlotIII != "c" {
		t.Errorf("Formatives order: %v %v %v",
			all[0].SlotIII, all[1].SlotIII, all[2].SlotIII)
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
