package gloss

import (
	"errors"
	"fmt"
	"strings"

	"github.com/christian-oudard/ithkuil/fault"
	g "github.com/christian-oudard/ithkuil/grammar"
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

// badValue reports a value-stage fault other than a name missing from
// its inventory: a degree outside 1-9, a case with no accessor
// increment, a name that exists but is not admissible where it was
// written. Each is a different problem from "no such name" and reads
// differently to someone holding the table, so unlisted stays
// separate rather than absorbing them.
func badValue(tok, code, found, admits string) error {
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
	if err == nil {
		return nil
	}
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

// assigned is the ledger of which grammatical categories a gloss has
// already set, and the value that set each one.
//
// A gloss assigns each category once. Accepting a second assignment
// let the last one win in silence, so "S2-S3-ml" composed as stem 3
// and said nothing about the S2 it had discarded — the one failure
// mode where a wrong answer comes back looking like a right one.
//
// The ledger is per scope, not per parse: a Ca stacked with "Ca:" is
// a second Ca complex rather than a second assignment to the Slot VI
// one, so it keeps its own.
type assigned map[string]string

func newAssigned() assigned { return assigned{} }

// apply sets one flag and records the category it assigned, refusing
// a category that is already spoken for. Both values are named: the
// one rejected says what to remove, and the one it collided with says
// why, which a reader scanning a long gloss cannot otherwise find.
func (a assigned) apply(f *g.Formative, flag string) error {
	cat, err := ApplyFlag(f, flag)
	if err != nil {
		return err
	}
	if prev, dup := a[cat]; dup {
		// The category is the Code rather than a generic "syntax":
		// which category collided is the thing a reader looks up, and
		// a caller marking a token wants to name the same field the
		// gloss does.
		return fault.One(flag, fault.Fault{
			Stage: fault.Shape,
			Code:  cat,
			Found: flag,
			Fix:   "a gloss sets " + cat + " once, and " + prev + " already set it",
		})
	}
	a[cat] = strings.ToUpper(flag)
	return nil
}

// plural writes a count with its noun, so a message reads "1
// dependent" rather than "1 dependent(s)".
func plural(n int, noun string) string {
	if n == 1 {
		return fmt.Sprintf("%d %s", n, noun)
	}
	return fmt.Sprintf("%d %ss", n, noun)
}

// collected gathers the faults of a whole gloss so a writer sees
// every bad token at once rather than the first one repeatedly.
//
// A fault keeps the token it came from as its Word, and Faults holds
// only one; flattening the list would lose which token each belonged
// to. So the token is folded into the Fix on the way in, where it is
// the one thing a writer needs to find it again.
type collected struct {
	list []fault.Fault
}

// add records a token's failure, if it failed. A nil error is the
// ordinary case and costs nothing, which is what lets the caller
// write the loop without a branch in it.
func (c *collected) add(err error) {
	if err == nil {
		return
	}
	var fs fault.Faults
	if !errors.As(err, &fs) {
		c.list = append(c.list, fault.Fault{
			Stage: fault.Shape, Code: "syntax", Fix: err.Error(),
		})
		return
	}
	for _, f := range fs.List {
		if fs.Word != "" && f.Found != fs.Word {
			f.Fix += " (in " + fs.Word + ")"
		}
		c.list = append(c.list, f)
	}
}

func (c *collected) err(subject string) error {
	if len(c.list) == 0 {
		return nil
	}
	return fault.Faults{Word: subject, List: c.list}
}
