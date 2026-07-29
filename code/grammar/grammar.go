// Package grammar holds the morphological categories of Ithkuil V4
// (Stem, Version, Function, Case, etc.) and the slot tuples that combine
// them. Encoding from grammar values to written vowels/consonants lives
// here too; parsing in the reverse direction lives in package parse.
package grammar

// Stem is one of four lexical stems S1, S2, S3, S0.
type Stem int

const (
	S1 Stem = iota
	S2
	S3
	S0
)

func (s Stem) String() string {
	return [...]string{"S1", "S2", "S3", "S0"}[s]
}

// Version distinguishes Processual (ongoing) from Completive (achieved).
type Version int

const (
	PRC Version = iota
	CPT
)

func (v Version) String() string {
	return [...]string{"PRC", "CPT"}[v]
}

// SlotII = (Stem, Version), encoded as the Vv vowel.
type SlotII struct {
	Stem    Stem
	Version Version
}

var DefaultSlotII = SlotII{S1, PRC}

// Function distinguishes Stative (state) from Dynamic (action/event).
type Function int

const (
	STA Function = iota
	DYN
)

func (f Function) String() string {
	return [...]string{"STA", "DYN"}[f]
}

// Specification is one of four ways a root is realized: Basic,
// Contential, Constitutive, Objective.
type Specification int

const (
	BSC Specification = iota
	CTE
	CSV
	OBJ
)

func (s Specification) String() string {
	return [...]string{"BSC", "CTE", "CSV", "OBJ"}[s]
}

// Context places the formative on the Existential/Functional/
// Representational/Amalgamative scale (= series 1..4 of the Vr vowel).
type Context int

const (
	EXS Context = iota
	FNC
	RPS
	AMG
)

func (c Context) String() string {
	return [...]string{"EXS", "FNC", "RPS", "AMG"}[c]
}

// SlotIV = (Function, Specification, Context), encoded as the Vr vowel.
type SlotIV struct {
	Function      Function
	Specification Specification
	Context       Context
}

// DefaultSlotIV is the unmarked Vr: Stative, Basic, Existential.
var DefaultSlotIV = SlotIV{STA, BSC, EXS}
