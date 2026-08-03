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
	return enumName(s, "Stem", "S1", "S2", "S3", "S0")
}

// Version distinguishes Processual (ongoing) from Completive (achieved).
type Version int

const (
	PRC Version = iota
	CPT
)

func (v Version) String() string {
	return enumName(v, "Version", "PRC", "CPT")
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
	return enumName(f, "Function", "STA", "DYN")
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
	return enumName(s, "Specification", "BSC", "CTE", "CSV", "OBJ")
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
	return enumName(c, "Context", "EXS", "FNC", "RPS", "AMG")
}

// SlotIV = (Function, Specification, Context), encoded as the Vr vowel.
type SlotIV struct {
	Function      Function
	Specification Specification
	Context       Context
}

// DefaultSlotIV is the unmarked Vr: Stative, Basic, Existential.
var DefaultSlotIV = SlotIV{STA, BSC, EXS}
