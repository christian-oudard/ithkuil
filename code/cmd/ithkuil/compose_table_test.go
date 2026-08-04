package main

import (
	"strings"
	"testing"
)

// A compose failure shows the expression token by token. The tokens
// that read are half the diagnosis, the same way the slots that read
// are half of a parse failure: a list of complaints says what is
// wrong without saying how much was understood, and a writer looking
// at a long gloss cannot otherwise tell one bad token from a
// misread shape.
func TestCompose_MarksTheFailingTokenAndPassesTheRest(t *testing.T) {
	_, errOut, code := runCLI("-data", dataFile(), "compose", "S2.CPT-ml-DYN.OBJ-ZZZ-ERG")
	if code == 0 {
		t.Fatal("compose accepted a gloss with an unknown value")
	}
	for _, want := range []string{"TOKEN", "READS AS", "ok"} {
		if !strings.Contains(errOut, want) {
			t.Errorf("output missing %q; got %q", want, errOut)
		}
	}
	for _, line := range strings.Split(errOut, "\n") {
		// The header echoes the whole expression, so every token
		// appears in it; the rows are what these assertions are about.
		if strings.HasPrefix(line, "compose:") {
			continue
		}
		switch {
		case strings.Contains(line, "ZZZ"):
			if !strings.Contains(line, "✗") {
				t.Errorf("the failing token is not marked: %q", line)
			}
		case strings.Contains(line, "S2.CPT"), strings.Contains(line, "DYN.OBJ"):
			if strings.Contains(line, "✗") {
				t.Errorf("a token that read is marked as failing: %q", line)
			}
			if !strings.Contains(line, "ok") {
				t.Errorf("a token that read does not say so: %q", line)
			}
		}
	}
}

// Each bad token gets its own row, so two slots raising the same
// sentence are still told apart.
func TestCompose_EveryBadTokenGetsARow(t *testing.T) {
	_, errOut, _ := runCLI("-data", dataFile(), "compose", "S9-ZZZ-ml-QQQ")
	for _, want := range []string{"S9", "ZZZ", "QQQ"} {
		if !strings.Contains(errOut, want) {
			t.Errorf("output does not name %q; got %q", want, errOut)
		}
	}
	if n := strings.Count(errOut, "✗"); n != 3 {
		t.Errorf("marked %d tokens, want 3; got %q", n, errOut)
	}
}

// A one-token expression has no table to draw — there is nothing to
// contrast the failure with — so the fault prints on its own.
func TestCompose_ASingleTokenNeedsNoTable(t *testing.T) {
	_, errOut, _ := runCLI("-data", dataFile(), "compose", "Ml")
	if strings.Contains(errOut, "TOKEN") {
		t.Errorf("drew a table for one token: %q", errOut)
	}
	if !strings.Contains(errOut, "lowercase") {
		t.Errorf("lost the fault: %q", errOut)
	}
}
