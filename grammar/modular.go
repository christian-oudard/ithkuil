package grammar

// ModularScope is the application scope of a modular adjunct (§4.3
// Slot 1). The default scope applies to a standalone formative (and to
// both members of a concatenated pair); the Parent and Concat values
// restrict the adjunct to just one side of a concatenated pair.
type ModularScope int

const (
	ModularScopeDefault ModularScope = iota // no Slot-1 prefix
	ModularScopeParent                      // w- prefix: parent only
	ModularScopeConcat                      // y- prefix: concatenated only
)

func (s ModularScope) String() string {
	return [...]string{"default", "parent", "concat"}[s]
}

// ModularReach is the reach scope encoded by the Slot-4 V_H vowel
// when the modular adjunct carries ultimate stress (§4.3 Slot 4). The
// default reach (no V_H) is "none" — the adjunct's content applies to
// the formative's Slot VIII only.
type ModularReach int

const (
	ModularReachNone        ModularReach = iota // no V_H — default reach
	ModularReachCaseMood                        // V_H = e
	ModularReachCaseMoodIll                     // V_H = a (+ Validation/Illocution)
	ModularReachFormative                       // V_H = i/u
	ModularReachAdjacent                        // V_H = o
)

func (r ModularReach) String() string {
	return [...]string{"none", "case/mood", "case/mood/ill", "form.", "adj."}[r]
}

// ModularAdjunct carries one or more SlotVIII-shaped Vn+Cn pairs as a
// stand-alone adjunct, scoping mood/aspect/etc. across an adjacent
// formative instead of being embedded in it.
//
// Structure: [w/y] (Vn Cn){0-3} V(final).
//
//	Scope:   decoded application scope from the optional w-/y- prefix.
//	Pairs:   zero or more (Vn, Cn) pairs (up to 3).
//	Final:   trailing vowel — aspect when Pairs is empty, otherwise a
//	         scope vowel.
//
// Vn and Cn are surface text so callers can run them through ParseVnCn
// + DisambiguateSlotVIII as needed.
type ModularAdjunct struct {
	Scope ModularScope
	Reach ModularReach // decoded V_H reach scope (when ultimate stress)
	Pairs []VnCnPair   // 0-3 pairs
	Final string       // trailing vowel (raw — non-empty when not a V_H)
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
