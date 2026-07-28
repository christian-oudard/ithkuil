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
}

func TestAddType1_SetsConcat(t *testing.T) {
	c := New(g.MinimalFormative("ml")).AddType1(g.MinimalFormative("t"))
	if c.Length() != 2 {
		t.Errorf("Length() = %d, want 2", c.Length())
	}
	dep := c.Tail[0]
	if cluster(dep) != "t" {
		t.Errorf("dependent Cr = %q, want %q", cluster(dep), "t")
	}
	if dep.Concat == g.ConcatNone || dep.Concat != g.Type1 {
		t.Errorf("dependent Concat = %v, want Type1", dep.Concat)
	}
}

func TestAddType2_SetsConcat(t *testing.T) {
	c := New(g.MinimalFormative("ml")).AddType2(g.MinimalFormative("t"))
	dep := c.Tail[0]
	if dep.Concat == g.ConcatNone || dep.Concat != g.Type2 {
		t.Errorf("Type2 dependent Concat = %v, want Type2", dep.Concat)
	}
}

func TestFormatives_OrderAndCount(t *testing.T) {
	// New() takes the parent; AddType1/AddType2 register the
	// concatenated dependents. Formatives() walks them in surface
	// order: dependents first, then the parent.
	c := New(g.MinimalFormative("a")).
		AddType1(g.MinimalFormative("b")).
		AddType2(g.MinimalFormative("c"))
	all := c.Formatives()
	if len(all) != 3 {
		t.Fatalf("Formatives() = %d, want 3", len(all))
	}
	if cluster(all[0]) != "b" || cluster(all[1]) != "c" || cluster(all[2]) != "a" {
		t.Errorf("Formatives order: %s %s %s",
			cluster(all[0]), cluster(all[1]), cluster(all[2]))
	}
}

func TestConcatMarker(t *testing.T) {
	cases := []struct {
		in   g.ConcatenationStatus
		want string
	}{
		{g.ConcatNone, ""},
		{g.Type1, "h"},
		{g.Type2, "hw"},
	}
	for _, c := range cases {
		if got := ConcatMarker(c.in); got != c.want {
			t.Errorf("ConcatMarker(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}
