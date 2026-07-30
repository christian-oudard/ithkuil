// Package fault is the vocabulary for saying why a romanization could
// not be read. It sits below every reader so that all of them fail in
// the same shape, and carries no grammar of its own.
//
// A reader that returns a bare string has thrown away the two things a
// caller needs: which stage of reading failed, and which slot. The CLI
// then has to reconstruct them by re-parsing, and a program consuming
// the parser cannot act on the failure at all. So a fault carries the
// classification alongside the prose rather than instead of it.
package fault

import (
	"fmt"
	"strings"
)

// Stage is how far reading got before it failed. The four stages are
// ordered: each presupposes the one before, so a word that fails at
// Value was pronounceable and cut into slots cleanly, and a word that
// fails at Chars was never anything at all.
//
// The order is the contract. Word class is decided by trying each
// class in turn, and the attempt that reached the latest stage is the
// one with something useful to say.
type Stage int

const (
	// Chars: a character outside the V4 alphabet.
	Chars Stage = iota
	// Sound: legal letters that cannot be pronounced together, or a
	// stress mark that cannot be read.
	Sound
	// Shape: a pronounceable word that will not cut into slots.
	Shape
	// Value: a slot holding a form its table does not list.
	Value
)

func (s Stage) String() string {
	switch s {
	case Chars:
		return "characters"
	case Sound:
		return "sound"
	case Shape:
		return "shape"
	case Value:
		return "value"
	}
	return fmt.Sprintf("Stage(%d)", int(s))
}

// Fault is one reason a romanization could not be read.
//
// Code and Fix are both required and neither substitutes for the
// other. Code is what a program branches on: a §2 rule number at
// Sound, a slot name at Value. Fix is the sentence a reader acts on,
// and says what is admissible rather than guessing at what was meant —
// a parser that offers "did you mean" is inventing evidence it does
// not have.
type Fault struct {
	Stage Stage
	Code  string // rule identifier ("2.1", "chars") or slot name ("Ca")
	Found string // the offending text; empty when the whole word is the subject
	Fix   string // what would have to change
}

func (f Fault) Error() string {
	if f.Found == "" {
		return f.Code + ": " + f.Fix
	}
	return fmt.Sprintf("%s %q: %s", f.Code, f.Found, f.Fix)
}

// Faults is what a reader returns. It names the word because the layer
// that reports a failure is rarely the layer that raised it, and it
// holds every fault rather than the first: a reader fixing a word wants
// the whole list, and a word with two unlisted slots is a different
// problem from a word with one.
type Faults struct {
	Word string
	List []Fault
}

func (e Faults) Error() string {
	parts := make([]string, len(e.List))
	for i, f := range e.List {
		parts[i] = f.Error()
	}
	return e.Word + ": " + strings.Join(parts, "; ")
}

// Stage reports the latest stage any of the faults reached, which is
// how far the word got before it stopped. Comparing this across
// several failed readings picks the one worth showing.
func (e Faults) Stage() Stage {
	var s Stage
	for _, f := range e.List {
		if f.Stage > s {
			s = f.Stage
		}
	}
	return s
}

// One wraps a single fault as the error a reader returns.
func One(word string, f Fault) error {
	return Faults{Word: word, List: []Fault{f}}
}
