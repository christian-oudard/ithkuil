// Package concatenation assembles multi-formative chains. Type 1
// concatenation forms a single compound concept whose dependents share
// the head formative's case frame; Type 2 concatenation coordinates
// formatives that each carry their own case.
package concatenation

import (
	g "github.com/christian-oudard/ithkuil/grammar"
)

// Chain is a list of concatenated dependents followed by the parent
// formative (§3.1.7). On the surface the parent comes LAST: the spec
// terms the leading formative(s) "concatenated" — each carries a Cc
// marker in Slot I — and the trailing one "parent" — which has no Cc.
// Head holds the parent; Tail holds the dependents in surface order.
// Each dependent's own Concat field tells Type-1 from Type-2 apart.
type Chain struct {
	Head g.Formative
	Tail []g.Formative
}

// New starts a chain with the given head formative.
func New(head g.Formative) *Chain {
	return &Chain{Head: head}
}

// AddType1 appends a Type-1 dependent. The dependent's SlotI is
// overwritten to Type1.
func (c *Chain) AddType1(f g.Formative) *Chain {
	t := g.Type1
	f.Concat = &t
	c.Tail = append(c.Tail, f)
	return c
}

// AddType2 appends a Type-2 dependent. The dependent's SlotI is
// overwritten to Type2.
func (c *Chain) AddType2(f g.Formative) *Chain {
	t := g.Type2
	f.Concat = &t
	c.Tail = append(c.Tail, f)
	return c
}

// Formatives returns every formative in the chain in surface order:
// the leading concatenated dependents first, then the parent.
func (c *Chain) Formatives() []g.Formative {
	out := make([]g.Formative, 0, 1+len(c.Tail))
	out = append(out, c.Tail...)
	out = append(out, c.Head)
	return out
}

// Length returns the total number of formatives in the chain.
func (c *Chain) Length() int { return 1 + len(c.Tail) }

// ConcatMarker returns the surface Slot I consonant for a
// ConcatenationStatus pointer, or "" if the formative isn't part of a
// chain. Mirrors the Haskell helper of the same name.
func ConcatMarker(s *g.ConcatenationStatus) string {
	if s == nil {
		return ""
	}
	switch *s {
	case g.Type1:
		return "h"
	case g.Type2:
		return "hw"
	}
	return ""
}
