package gloss

import (
	"errors"
	"fmt"
	"strings"

	"github.com/christian-oudard/ithkuil/fault"
	"github.com/christian-oudard/ithkuil/phonology"
)

// The gloss is an authoring syntax as well as an output format, so a
// reader of these messages is mid-edit. The four stages of fault mean
// here what they mean for a romanization, because a gloss carries
// Ithkuil text inside it: a root cluster reaches phonology's own two
// stages, the punctuation grammar is the shape, and the inventories
// of names are the values.

// syntax reports a token that does not fit the punctuation grammar.
// tok is the whole token rather than the character at fault, because
// the gloss's marks each have one job and a token is the unit that
// claims a job.
func syntax(tok, admits string) error {
	return fault.One(tok, fault.Fault{
		Stage: fault.Shape, Code: "syntax", Found: tok, Fix: admits,
	})
}

// unlisted reports a name that is not in the inventory the token's
// shape sends it to. kind names that inventory — "affix", "case",
// "grammatical value" — which is the part a reader acts on: knowing
// that ZZZ was looked for among the affixes and not among the cases
// says where to go and look.
func unlisted(tok, kind, name string) error {
	return fault.One(tok, fault.Fault{
		Stage: fault.Value,
		Code:  kind,
		Found: name,
		Fix:   "no " + kind + " is named " + name,
	})
}

// value reports a name that is in an inventory but not admissible
// where it was written, which is a different problem from not being
// there at all and reads differently to someone holding the table.
func value(tok, code, found, admits string) error {
	return fault.One(tok, fault.Fault{
		Stage: fault.Value, Code: code, Found: found, Fix: admits,
	})
}

// clusterFault judges a Cr or Cs written inside a gloss as Ithkuil
// text, so it reaches the same stages a romanization does. Only two
// rules are applied; see validateCluster for why the rest are not.
func clusterFault(kind, cluster string) error {
	if v := phonology.CheckChars(cluster); len(v) > 0 {
		v[0].Code = kind
		v[0].Found = cluster
		return fault.Faults{Word: cluster, List: v}
	}
	if phonology.HasTripleConsonant(cluster) {
		return fault.One(cluster, fault.Fault{
			Stage: fault.Sound,
			Code:  kind,
			Found: cluster,
			Fix:   "1.7: no cluster holds three consonants in a row",
		})
	}
	return nil
}

// inToken names which token a fault came from, without flattening it
// back to a string. A gloss is a line of tokens and the one at fault
// has to be identifiable, but the stage and the slot have to survive
// for the caller that wants to mark it rather than print it.
//
// It relabels rather than appends. Faults already carries a Word and
// prints it in front, so a fault raised on part of a token — the
// degree of "DEV/99" — needs the token put there and nothing else. An
// appended "(in DEV/99)" named it twice in one line, which reads as
// though the two mentions were different things.
func inToken(tok string, err error) error {
	var fs fault.Faults
	if errors.As(err, &fs) {
		return fault.Faults{Word: tok, List: fs.List}
	}
	return fmt.Errorf("token %q: %w", tok, err)
}

// degreeAdmits is the sentence every degree-bearing shape shares. The
// nine degrees are the whole of §3.5's inventory, so a reader who
// wrote something else needs the range and nothing more.
func degreeAdmits(found string) string {
	return "an affix degree is one digit, 1 through 9, and " +
		strings.TrimSpace(found) + " is not"
}
