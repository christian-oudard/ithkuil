package gloss_test

import (
	"testing"
)

// Reading a gloss is permissive: a token that fails records its fault
// and the rest are still read, so a writer with three problems fixes
// three rather than discovering them one attempt at a time. The
// formative path does this already. These are the other word classes,
// which each kept their own loop and stopped at the first.

func TestParseWord_ReferentialReportsEveryBadSlot(t *testing.T) {
	fs := tokenFault(t, "1m-ZZZ-XXX")
	if len(fs.List) < 2 {
		t.Fatalf("faults = %+v, want one per bad slot", fs.List)
	}
}

func TestParseWord_CombinationReferentialReportsEveryBadSlot(t *testing.T) {
	fs := tokenFault(t, "1m-THM-BSC-ZZZ-XXX")
	if len(fs.List) < 2 {
		t.Fatalf("faults = %+v, want one per bad slot", fs.List)
	}
}

// Every fault stays complete however many there are: collecting must
// not flatten them into a list of bare sentences.
func TestParseWord_CollectedFaultsAreComplete(t *testing.T) {
	for _, tok := range []string{"1m-ZZZ-XXX", "1m-THM-BSC-ZZZ-XXX"} {
		t.Run(tok, func(t *testing.T) {
			for _, f := range tokenFault(t, tok).List {
				if f.Code == "" {
					t.Errorf("fault has no code: %+v", f)
				}
				if f.Fix == "" {
					t.Errorf("fault has no fix: %+v", f)
				}
			}
		})
	}
}
