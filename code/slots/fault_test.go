package slots_test

import (
	"errors"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/fault"
	"github.com/christian-oudard/ithkuil/slots"
)

// A value-stage failure is the interesting one: the word is
// well-formed and cut into slots cleanly, and only its content is
// wrong. Reporting the first bad slot and stopping tells a reader to
// fix one thing, and hands them the same complaint again once they
// have. Every unlisted slot is reported at once.

func faultsOf(t *testing.T, l slots.Layout) fault.Faults {
	t.Helper()
	_, err := slots.ToGrammar(l)
	if err == nil {
		t.Fatal("ToGrammar accepted a layout with unlisted slots")
	}
	var fs fault.Faults
	if !errors.As(err, &fs) {
		t.Fatalf("error %v (%T) is not fault.Faults", err, err)
	}
	return fs
}

func codes(fs fault.Faults) []string {
	out := make([]string, len(fs.List))
	for i, f := range fs.List {
		out[i] = f.Code
	}
	return out
}

func has(fs fault.Faults, code string) *fault.Fault {
	for i := range fs.List {
		if fs.List[i].Code == code {
			return &fs.List[i]
		}
	}
	return nil
}

// Two slots wrong, two faults. The Vr and the Ca decode independently,
// so neither failure is a reason to stop looking at the other.
func TestToGrammar_ReportsEveryUnlistedSlot(t *testing.T) {
	fs := faultsOf(t, slots.Layout{
		Kind: slots.CrFormative,
		Cr:   "m",
		Vr:   "ëi", // not a Vr form
		Ca:   "vẓ", // not a Ca complex
	})
	if len(fs.List) != 2 {
		t.Fatalf("faults = %v, want one for Vr and one for Ca", codes(fs))
	}
	for _, code := range []string{"Vr", "Ca"} {
		f := has(fs, code)
		if f == nil {
			t.Fatalf("no fault for slot %s; got %v", code, codes(fs))
		}
		if f.Stage != fault.Value {
			t.Errorf("%s fault is stage %v, want value", code, f.Stage)
		}
		if f.Found == "" {
			t.Errorf("%s fault does not say what was written there", code)
		}
		if f.Fix == "" {
			t.Errorf("%s fault does not say what would be admissible", code)
		}
	}
}

// The fault names the slot in its Code, not only inside the prose, so
// a caller can mark the failing row of a slot table without matching
// on English.
func TestToGrammar_CodeIsTheSlotName(t *testing.T) {
	fs := faultsOf(t, slots.Layout{Kind: slots.CrFormative, Cr: "m", Vr: "a", Ca: "vẓ"})
	if got := codes(fs); len(got) != 1 || got[0] != "Ca" {
		t.Fatalf("codes = %v, want [Ca]", got)
	}
	if f := has(fs, "Ca"); f.Found != "vẓ" {
		t.Errorf("Found = %q, want the Ca as written", f.Found)
	}
}

// A slot that reads correctly raises nothing. Without this the
// "report everything" change could pass its own test by reporting
// every slot unconditionally.
func TestToGrammar_SaysNothingAboutSlotsThatRead(t *testing.T) {
	_, err := slots.ToGrammar(slots.Layout{Kind: slots.CrFormative, Cr: "m", Vr: "a", Ca: "l"})
	if err != nil {
		t.Fatalf("a well-formed layout raised %v", err)
	}
}

// An affix carries two conjuncts and either can be wrong, so its
// fault has to say which affix as well as which slot. "Slot V" alone
// does not locate anything in a word with three of them.
func TestToGrammar_AffixFaultNamesItsPosition(t *testing.T) {
	fs := faultsOf(t, slots.Layout{
		Kind: slots.CrFormative,
		Cr:   "m",
		Vr:   "a",
		Ca:   "l",
		SlotVII: []slots.AffixChunk{
			{Vx: "a", Cs: "ţř"},
			{Vx: "ïï", Cs: "t"},
		},
	})
	f := has(fs, "Vx₂")
	if f == nil {
		t.Fatalf("codes = %v, want the second affix named", codes(fs))
	}
	if !strings.Contains(f.Fix, "degree") && !strings.Contains(f.Fix, "Vx") {
		t.Errorf("Fix = %q, want it to say what a Vx admits", f.Fix)
	}
}
