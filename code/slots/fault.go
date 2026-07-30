package slots

import (
	"errors"
	"fmt"

	"github.com/christian-oudard/ithkuil/fault"
)

// faults collects the value-stage faults of one layout.
//
// A layout is already fully divided, so its slots decode independently
// and one unlisted slot is no reason to stop reading the rest. Stopping
// at the first told a reader to fix one thing and then handed them the
// next complaint once they had, which is the same diagnosis delivered
// one slot per attempt.
//
// The decoders substitute a default for a slot they could not read and
// carry on. The half-built Formative that results is never returned —
// a non-empty collector is an error — so the substitute only has to be
// good enough to let the remaining slots be judged.
type faults struct {
	list []fault.Fault
}

// add records that a slot holds a form its table does not list. slot
// is the name a reader sees in the phonetic table ("Ca", "Vx₂"), which
// is what makes the fault locatable; found is what was written there;
// admits says what the slot would accept.
func (f *faults) add(slot, found, admits string) {
	f.list = append(f.list, fault.Fault{
		Stage: fault.Value,
		Code:  slot,
		Found: found,
		Fix:   admits,
	})
}

func (f *faults) any() bool { return len(f.list) > 0 }

// shape reports that the word will not cut into slots. Unlike a value
// fault it ends the reading: there is nothing past the point where the
// cut stopped to form an opinion about, so a shape failure is always
// alone. It is returned as an error rather than collected for that
// reason.
func shape(slot, found, admits string) fault.Fault {
	return fault.Fault{Stage: fault.Shape, Code: slot, Found: found, Fix: admits}
}

// shapeErr names the word on whatever the cut raised. A fault that
// already carries its stage — phonology's, or one of the shape faults
// above — keeps it; anything else is a shape failure by virtue of
// where it was raised, since cutting a word into slots is all this
// layer does.
func shapeErr(word string, err error) error {
	var fs fault.Faults
	if errors.As(err, &fs) {
		return fs
	}
	var f fault.Fault
	if errors.As(err, &f) {
		return fault.One(word, f)
	}
	return fault.One(word, shape("shape", "", err.Error()))
}

// err returns the collected faults as the error ToGrammar hands back,
// or nil when every slot read.
func (f *faults) err(word string) error {
	if !f.any() {
		return nil
	}
	return fault.Faults{Word: word, List: f.list}
}

// subscript writes an affix's 1-based position the way the phonetic
// table does, so a fault's Code matches the row it belongs to.
func subscriptSlot(name string, i int) string {
	digits := []rune("₀₁₂₃₄₅₆₇₈₉")
	if i < 1 || i > 9 {
		return fmt.Sprintf("%s%d", name, i)
	}
	return name + string(digits[i])
}

// plural writes a count with its noun, so a message reads "1 conjunct"
// rather than "1 conjunct(s)".
func plural(n int, noun string) string {
	if n == 1 {
		return fmt.Sprintf("%d %s", n, noun)
	}
	return fmt.Sprintf("%d %ss", n, noun)
}
