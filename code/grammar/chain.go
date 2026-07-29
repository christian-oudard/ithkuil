package grammar

// A concatenation chain is a word (§3.1.7): two or more formatives
// written with hyphens between them and read as one unit. Type 1
// forms a single compound concept whose dependents share the head's
// case frame; Type 2 coordinates formatives that each carry their own
// case.

// Chain is a list of concatenated dependents followed by the parent
// formative (§3.1.7). On the romanization the parent comes LAST: the spec
// terms the leading formative(s) "concatenated" — each carries a Cc
// marker in Slot I — and the trailing one "parent" — which has no Cc.
// Head holds the parent; Tail holds the dependents in written order.
// Each dependent's own Concat field tells Type-1 from Type-2 apart.
type Chain struct {
	Head Formative
	Tail []Formative
}

// NewChain starts a chain with the given head formative.
func NewChain(head Formative) *Chain {
	return &Chain{Head: head}
}

// AddType1 appends a Type-1 dependent. The dependent's SlotI is
// overwritten to Type1.
func (c *Chain) AddType1(f Formative) *Chain {
	f.Concat = Type1
	c.Tail = append(c.Tail, f)
	return c
}

// AddType2 appends a Type-2 dependent. The dependent's SlotI is
// overwritten to Type2.
func (c *Chain) AddType2(f Formative) *Chain {
	f.Concat = Type2
	c.Tail = append(c.Tail, f)
	return c
}

// Formatives returns every formative in the chain in written order:
// the leading concatenated dependents first, then the parent.
func (c *Chain) Formatives() []Formative {
	out := make([]Formative, 0, 1+len(c.Tail))
	out = append(out, c.Tail...)
	out = append(out, c.Head)
	return out
}

// Length returns the total number of formatives in the chain.
func (c *Chain) Length() int { return 1 + len(c.Tail) }
