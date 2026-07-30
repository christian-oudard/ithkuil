package fault_test

import (
	"errors"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/fault"
)

// A Fault is a machine-readable classification and a sentence for a
// reader, and the two are not interchangeable. These check that both
// survive, that neither is silently dropped when a fault is turned
// into an error string, and that a caller can recover the structure
// after it has travelled as an error.

func TestFault_ErrorNamesTheSlotAndTheFix(t *testing.T) {
	f := fault.Fault{
		Stage: fault.Value,
		Code:  "Ca",
		Found: "vẓ",
		Fix:   "no Ca complex is written vẓ",
	}
	msg := f.Error()
	for _, want := range []string{"Ca", "vẓ", "no Ca complex is written"} {
		if !strings.Contains(msg, want) {
			t.Errorf("Error() = %q, missing %q", msg, want)
		}
	}
}

// A whole-word fault has no Found text of its own — the word is the
// subject. It must not print an empty pair of quotes where the
// offending text would go.
func TestFault_ErrorOmitsAnEmptyFound(t *testing.T) {
	f := fault.Fault{
		Stage: fault.Chars,
		Code:  "chars",
		Fix:   "remove 'q' (U+0071)",
	}
	if got := f.Error(); strings.Contains(got, `""`) {
		t.Errorf("Error() = %q, printed an empty Found", got)
	}
}

// Faults is the error a reader returns. It has to name the word,
// since the caller reporting it may be several layers away from the
// text, and it has to list every fault rather than the first.
func TestFaults_ErrorListsEveryFault(t *testing.T) {
	err := fault.Faults{
		Word: "malëuţřaix",
		List: []fault.Fault{
			{Stage: fault.Value, Code: "Ca", Found: "vẓ", Fix: "no Ca complex is written vẓ"},
			{Stage: fault.Value, Code: "Vr", Found: "ou", Fix: "no Vr form is written ou"},
		},
	}
	msg := err.Error()
	for _, want := range []string{"malëuţřaix", "Ca", "Vr"} {
		if !strings.Contains(msg, want) {
			t.Errorf("Error() = %q, missing %q", msg, want)
		}
	}
}

// The structure has to survive being wrapped, because the layer that
// renders a fault is never the layer that raised it. A caller that
// only prints the string cannot mark the failing row of a slot table.
func TestFaults_SurvivesWrapping(t *testing.T) {
	inner := fault.Faults{
		Word: "mavẓorf",
		List: []fault.Fault{{Stage: fault.Value, Code: "Ca", Found: "vẓ", Fix: "no Ca complex is written vẓ"}},
	}
	var got fault.Faults
	if !errors.As(error(inner), &got) {
		t.Fatal("errors.As failed on an unwrapped Faults")
	}
	if got.List[0].Code != "Ca" {
		t.Errorf("Code = %q, want Ca", got.List[0].Code)
	}
}

// Stage orders the reading, and the order is the contract: a later
// stage presupposes every earlier one. Callers compare stages to
// decide which of several failed attempts read furthest, so the
// values must not be equal or reordered.
func TestStage_IsOrdered(t *testing.T) {
	stages := []fault.Stage{fault.Chars, fault.Sound, fault.Shape, fault.Value}
	for i := 1; i < len(stages); i++ {
		if !(stages[i-1] < stages[i]) {
			t.Errorf("%v is not before %v", stages[i-1], stages[i])
		}
	}
	for _, s := range stages {
		if s.String() == "" {
			t.Errorf("stage %d has no name", s)
		}
	}
}
