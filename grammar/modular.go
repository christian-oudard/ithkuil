package grammar

// ModularAdjunct carries one or more SlotVIII-shaped Vn+Cn pairs as a
// stand-alone adjunct, scoping mood/aspect/etc. across an adjacent
// formative instead of being embedded in it.
//
// Structure: [w/y] (Vn Cn){0-3} V(final).
//
//	Prefix:  optional "w" or "y" scope marker; empty when absent.
//	Pairs:   zero or more (Vn, Cn) pairs (up to 3).
//	Final:   trailing vowel — aspect when Pairs is empty, otherwise a
//	         scope vowel.
//
// Vn and Cn are surface text so callers can run them through ParseVnCn
// + DisambiguateSlotVIII as needed.
type ModularAdjunct struct {
	Prefix string      // "w", "y", or ""
	Pairs  []VnCnPair  // 0-3 pairs
	Final  string      // trailing vowel
	// Vn and Cn are also exposed as a convenience for the common
	// single-pair case (Pairs has exactly one element).
	Vn string
	Cn string
}

// VnCnPair is a single (Vn, Cn) pair inside a modular adjunct.
type VnCnPair struct {
	Vn string
	Cn string
}
