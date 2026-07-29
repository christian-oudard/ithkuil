// Package grammar holds the morphological categories of Ithkuil V4
// (Stem, Version, Function, Case, etc.) and the slot tuples that combine
// them. Encoding from grammar values to written vowels/consonants lives
// here too; parsing in the reverse direction lives in package parse.
package grammar

import "github.com/christian-oudard/ithkuil/phonology"

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

// SlotIIToVv encodes a SlotII as its Vv vowel using the Series 1 row of
// the vowel form table. Form 5 ("ëi") is reserved for the Cs-root special
// and never produced from a regular SlotII.
func SlotIIToVv(s SlotII) string {
	return SlotIIToVvSeries(s, 1)
}

// SlotIIToVvSeries encodes a SlotII as its Vv vowel in the given series
// (1-4). Series 1 is the canonical non-shortcut form; series 2-4 are
// used in Cc-shortcut forms to encode the elided Slot VI Ca alongside
// the Vv.
func SlotIIToVvSeries(s SlotII, series int) string {
	return phonology.VowelForm(series, slotIIForm(s))
}

// slotIIForm returns the form number (1-9) corresponding to a SlotII.
// Form 5 is reserved for the Cs-root special and is never produced
// from a regular SlotII.
func slotIIForm(s SlotII) int {
	switch s {
	case SlotII{S1, PRC}:
		return 1
	case SlotII{S1, CPT}:
		return 2
	case SlotII{S2, PRC}:
		return 3
	case SlotII{S2, CPT}:
		return 4
	case SlotII{S0, CPT}:
		return 6
	case SlotII{S0, PRC}:
		return 7
	case SlotII{S3, CPT}:
		return 8
	case SlotII{S3, PRC}:
		return 9
	}
	panic("grammar: unreachable SlotII")
}

// DefaultSlotII is the unmarked Vv: Stem 1, Processual.
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

// SlotIVToVr encodes a SlotIV as its Vr vowel. The series is determined
// by Context (EXS=1, FNC=2, RPS=3, AMG=4) and the form by the
// Function×Specification combination. Form 5 is reserved for Cs-roots.
func SlotIVToVr(s SlotIV) string {
	series := int(s.Context) + 1
	var form int
	switch {
	case s.Function == STA && s.Specification == BSC:
		form = 1
	case s.Function == STA && s.Specification == CTE:
		form = 2
	case s.Function == STA && s.Specification == CSV:
		form = 3
	case s.Function == STA && s.Specification == OBJ:
		form = 4
	case s.Function == DYN && s.Specification == OBJ:
		form = 6
	case s.Function == DYN && s.Specification == CSV:
		form = 7
	case s.Function == DYN && s.Specification == CTE:
		form = 8
	case s.Function == DYN && s.Specification == BSC:
		form = 9
	default:
		panic("grammar: unreachable SlotIV")
	}
	return phonology.VowelForm(series, form)
}
