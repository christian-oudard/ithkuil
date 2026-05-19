// Package concatenation assembles multi-formative chains. Type 1
// concatenation forms a single compound concept whose dependents share
// the head formative's case frame; Type 2 concatenation coordinates
// formatives that each carry their own case.
package concatenation

import (
	g "github.com/coudard/ithkuil/go/grammar"
)

// Type marks how a dependent formative attaches to its parent.
type Type int

const (
	Type1Concat Type = iota // shares case with parent (compound concept)
	Type2Concat             // independent case frame (coordination)
)

func (t Type) String() string {
	return [...]string{"Type1Concat", "Type2Concat"}[t]
}

// Link pairs a dependent formative with its concatenation type.
type Link struct {
	Type      Type
	Formative g.Formative
}

// Chain is a head formative plus a list of dependents. Each dependent
// carries its concatenation type and a fully populated Formative
// (with SlotI set to match the link type).
type Chain struct {
	Head Formative
	Tail []Link
}

// Formative is exported as an alias so callers can spell types out of
// this package without dragging in grammar. The underlying type is the
// same.
type Formative = g.Formative

// New starts a chain with the given head formative.
func New(head g.Formative) *Chain {
	return &Chain{Head: head}
}

// AddType1 appends a Type-1 dependent. The dependent's SlotI is
// overwritten to Type1.
func (c *Chain) AddType1(f g.Formative) *Chain {
	t := g.Type1
	f.SlotI = &t
	c.Tail = append(c.Tail, Link{Type: Type1Concat, Formative: f})
	return c
}

// AddType2 appends a Type-2 dependent. The dependent's SlotI is
// overwritten to Type2.
func (c *Chain) AddType2(f g.Formative) *Chain {
	t := g.Type2
	f.SlotI = &t
	c.Tail = append(c.Tail, Link{Type: Type2Concat, Formative: f})
	return c
}

// Formatives returns every formative in the chain (head first).
func (c *Chain) Formatives() []g.Formative {
	out := make([]g.Formative, 0, 1+len(c.Tail))
	out = append(out, c.Head)
	for _, l := range c.Tail {
		out = append(out, l.Formative)
	}
	return out
}

// Length returns the total number of formatives in the chain.
func (c *Chain) Length() int { return 1 + len(c.Tail) }

// Type1Dependents returns just the Type-1 attached formatives.
func (c *Chain) Type1Dependents() []g.Formative {
	var out []g.Formative
	for _, l := range c.Tail {
		if l.Type == Type1Concat {
			out = append(out, l.Formative)
		}
	}
	return out
}

// Type2Dependents returns just the Type-2 attached formatives.
func (c *Chain) Type2Dependents() []g.Formative {
	var out []g.Formative
	for _, l := range c.Tail {
		if l.Type == Type2Concat {
			out = append(out, l.Formative)
		}
	}
	return out
}

// Semantics classifies the chain into one of three high-level shapes:
//   - Compound: head alone, or every dependent is Type 1 (single
//     compound concept).
//   - Coordinated: every dependent is Type 2 (coordinated concepts).
//   - Mixed: a mix of Type 1 and Type 2 dependents.
type Semantics int

const (
	Compound Semantics = iota
	Coordinated
	Mixed
)

func (s Semantics) String() string {
	return [...]string{"Compound", "Coordinated", "Mixed"}[s]
}

// Semantics returns the chain's classification.
func (c *Chain) Semantics() Semantics {
	if len(c.Tail) == 0 {
		return Compound
	}
	hasT1, hasT2 := false, false
	for _, l := range c.Tail {
		if l.Type == Type1Concat {
			hasT1 = true
		} else {
			hasT2 = true
		}
	}
	switch {
	case hasT1 && !hasT2:
		return Compound
	case hasT2 && !hasT1:
		return Coordinated
	default:
		return Mixed
	}
}

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
