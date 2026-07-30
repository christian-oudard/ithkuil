package gloss_test

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/fault"
	"github.com/christian-oudard/ithkuil/gloss"
)

// A gloss assigns each grammatical category once. Assigning one twice
// used to be accepted with the last write silently winning, so
// "S2-S3-ml" composed as stem 3 and said nothing about the S2 it had
// just discarded — the one failure mode where a wrong answer comes
// back looking like a right one.
//
// The rule is per category, not per token: two values of one category
// collide whether they are written in separate slots or joined by "."
// inside one.

func repeatFault(t *testing.T, expr string) fault.Faults {
	t.Helper()
	return glossFault(t, expr)
}

func TestParseFormative_ACategoryIsAssignedOnce(t *testing.T) {
	for _, c := range []struct {
		expr     string
		category string
		values   []string
	}{
		{"S2-S3-ml", "stem", []string{"S2", "S3"}},
		{"S2.S3-ml", "stem", []string{"S2", "S3"}},
		{"PRC-CPT-ml", "version", []string{"PRC", "CPT"}},
		{"ml-STA-DYN", "function", []string{"STA", "DYN"}},
		{"ml-BSC-OBJ", "specification", []string{"BSC", "OBJ"}},
		{"ml-ERG-ABS", "case", []string{"ERG", "ABS"}},
		{"ml-MSS-DPX", "configuration", []string{"MSS", "DPX"}},
	} {
		t.Run(c.expr, func(t *testing.T) {
			fs := repeatFault(t, c.expr)
			msg := fs.Error()
			if !strings.Contains(msg, c.category) {
				t.Errorf("does not name the category %q: %q", c.category, msg)
			}
			// Both values have to appear. Naming only the second says
			// what was rejected without saying what it collided with.
			for _, v := range c.values {
				if !strings.Contains(msg, v) {
					t.Errorf("does not name %q: %q", v, msg)
				}
			}
		})
	}
}

// Repeating one value is still a repeat. "S2-S2" says nothing the
// single "S2" does not, and treating it as harmless would mean the
// rule holds only where it changes the answer, which is not a rule a
// writer can keep in their head.
func TestParseFormative_TheSameValueTwiceIsStillARepeat(t *testing.T) {
	if fs := repeatFault(t, "S2-S2-ml"); !strings.Contains(fs.Error(), "stem") {
		t.Errorf("an identical repeat was not reported: %q", fs.Error())
	}
}

// The categories that share one written slot must collide, since only
// one of them survives into the word. Slot VIII holds an Aspect or a
// Valence or a Phase or an Effect or a Level, never two.
func TestParseFormative_OneSlotVIIIValue(t *testing.T) {
	for _, expr := range []string{"ml-RTR-PRG", "ml-RTR-RCP", "ml-CCN-CCA"} {
		t.Run(expr, func(t *testing.T) {
			if fs := repeatFault(t, expr); fs.Stage() != fault.Shape {
				t.Errorf("stage = %v, want shape: a repeat is a syntax error", fs.Stage())
			}
		})
	}
}

// Distinct categories that happen to be written in one slot compose
// as they always did. Over-applying the rule would reject the gloss
// syntax's own documented forms.
func TestParseFormative_DistinctCategoriesStillCompose(t *testing.T) {
	lex := testLexicon(t)
	for _, expr := range []string{
		"S2.CPT-ml-ERG",
		"S2.CPT-ml-DYN.OBJ-MSS.G-ERG",
		"ml-ASR.RPR",
		"ml-PEN-ERG",
		"ml-RTR-CCA",
		"ml-Ca:MSS.G-ERG",
	} {
		t.Run(expr, func(t *testing.T) {
			if _, err := gloss.ParseFormative(expr, lex.Affixes); err != nil {
				t.Errorf("ParseFormative(%q): %v", expr, err)
			}
		})
	}
}

// A stacked Ca is its own scope. "MSS" in the Slot VI Ca and "MSS" in
// a Ca stacked on it are two different Configurations, and reading
// the second as a repeat of the first would refuse a word the
// grammar allows.
func TestParseFormative_AStackedCaIsItsOwnScope(t *testing.T) {
	lex := testLexicon(t)
	if _, err := gloss.ParseFormative("ml-MSS-Ca:MSS-ERG", lex.Affixes); err != nil {
		t.Errorf("a stacked Ca repeating a Slot VI component was refused: %v", err)
	}
}
