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
// other. Code is what a program branches on and where a reader is sent
// to read more: a cited rule at Sound ("phonotactics §2.1"), a slot
// name at Value ("Ca"). A cited rule names its document, because the
// grammar and the phonotactics number independently and collide — see
// docs/reference/READING.md. Fix is the sentence a reader acts on, and
// says what is admissible rather than guessing at what was meant — a
// parser that offers "did you mean" is inventing evidence it does not
// have. It carries §-citations of its own where the rule it rests on
// is not the one in Code, and there names the phonotactics document
// when it means that one: a bare § in a Fix is the Grammar Design
// document, which is what the sentence around it is already about.
// In is the enclosing unit a fault sits inside, when the thing being
// read is made of several: one link of a concatenation chain, one
// token of a gloss. Without it, a report of several faults is several
// sentences with nothing saying which part of the input each is
// about, and two slots of one kind raise the same sentence twice. It
// is empty when the fault is about the whole of what was read.
type Fault struct {
	Stage Stage
	Code  string // cited rule ("phonotactics §2.1", "chars") or slot name ("Ca")
	Found string // the offending text; empty when the whole word is the subject
	In    string // the chain link or gloss token this sits in; empty when the whole
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

// Error leads with the word, except where the word is already the
// subject of the only fault in it. A one-token failure otherwise
// prints its token twice in one line — "S3: stem \"S3\": …" — which
// reads as though the two mentions were different things.
// Error names the subject exactly once.
//
// A lone fault whose Found is the whole subject already names it, so
// it stands alone: "shape \"hlç\": …". Anything else leads with the
// subject and drops it from the individual faults that repeat it,
// since a line naming one word twice reads as though the two
// mentions were different things.
func (e Faults) Error() string {
	if len(e.List) == 1 && e.List[0].Found == e.Word {
		return e.List[0].Error()
	}
	parts := make([]string, len(e.List))
	for i, f := range e.List {
		if f.Found == e.Word {
			f.Found = ""
		}
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

// Plural writes a count with its noun, so a fix reads "1 conjunct"
// rather than "1 conjunct(s)".
//
// It lives here rather than in either caller because both callers
// were the same six lines, and both use it for one thing: the Fix
// sentence, which is English by definition.
func Plural(n int, noun string) string {
	if n == 1 {
		return fmt.Sprintf("%d %s", n, noun)
	}
	return fmt.Sprintf("%d %ss", n, noun)
}
