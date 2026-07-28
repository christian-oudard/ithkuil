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

// ModularAdjunct carries one or more SlotVIII-shaped content slots as
// a stand-alone adjunct, scoping mood/aspect/etc. across an adjacent
// formative instead of being embedded in it. Surface encoding details
// (the w-/y- Slot-1 prefix, the Cn pattern alternation, the V_H Slot-4
// vowel) live in the Scope and Reach enums; Content holds the typed
// (Vn, Cn) pairs as SlotVIII values.
//
// Scope, Reach, and Content together carry the full grammatical
// content of §4.3 — no raw surface vowels remain in this struct.
type ModularAdjunct struct {
	Scope   ModularScope
	Reach   ModularReach
	Content []SlotVIII // 0-3 typed (Vn, Cn) pairs; lone-aspect modular = [VnCnAspect{...}]
}
