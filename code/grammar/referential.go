package grammar

// A referential is a word class of its own (§4.6), not a kind of
// formative, so it gets its own type here rather than a variant of
// Root. Both shapes the section defines share a head, which is either
// a chain of personal references or a suppletive carrier standing in
// for one.

// RefHead is the sealed sum type for what a referential points at.
type RefHead interface {
	refHead()
}

// PersonalHead is a chain of one or more personal references, with the
// optional §4.6 category modifier that scopes the whole chain. Refs is
// never empty in a valid head.
type PersonalHead struct {
	Refs     []PersonalRef
	Category *RefCategory // nil when the chain carries no modifier
}

func (PersonalHead) refHead() {}

// SuppletiveHead is §4.6.3's carrier, quotative, naming or phrasal
// cluster in place of a personal reference, which lets those adjuncts
// take the Specification, affixes and case-stacking a referential can
// carry. The epenthetic vowel the section requires in front of it
// ("üo-" here, "a-" on a combination referential) exists only to keep
// the word from being read as a modular adjunct or a concatenated
// formative, so it is a rendering concern and does not appear here.
type SuppletiveHead struct {
	Type CarrierType
}

func (SuppletiveHead) refHead() {}

// Referential is the §4.6.1 single- or dual-referential word.
//
// The section's slot table makes Case mandatory and everything after
// it optional, so Case is a value rather than a pointer: a bare
// referential cluster with no case vowel is not a word.
type Referential struct {
	Head   RefHead
	Case   Case
	Second *SecondReferent // nil for a single referential
	// RpvEssence records ultimate stress, which per §4.6.1 gives the
	// whole word Representative rather than Normal Essence.
	RpvEssence bool
}

// SecondReferent is the optional [w/y V_C2 [C_2]] tail. §4.6.1 gives
// it two readings that share one shape: with Refs it is a second
// referent carrying its own case, and without Refs it stacks a second
// case onto the head.
type SecondReferent struct {
	Case Case
	Refs []PersonalRef // empty means the case stacks onto the head
}

// CombinationReferential is the §4.6.2 shape, which adds a
// Specification, affixes and a stacked case to a referential.
type CombinationReferential struct {
	Head       RefHead
	Case       Case
	Spec       Specification
	Affixes    []Affix
	Case2      *Case // nil when no second case is stacked
	RpvEssence bool
}

// Refs returns the personal references a head carries, and false for a
// suppletive head. It saves callers a type switch in the common case
// of wanting the chain when there is one.
func HeadRefs(h RefHead) ([]PersonalRef, bool) {
	p, ok := h.(PersonalHead)
	if !ok {
		return nil, false
	}
	return p.Refs, true
}
