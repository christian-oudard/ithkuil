package gloss_test

import (
	"strings"
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

// A root that will not read does not end the reading. It used to,
// which left every other token unjudged — and a report that then
// showed them as fine would be claiming something the reader never
// checked.
func TestParseFormative_ABadRootDoesNotStopTheRest(t *testing.T) {
	fs := glossFault(t, "S9-ZZZ-Ml-QQQ")
	if len(fs.List) != 4 {
		t.Fatalf("faults = %+v, want one per bad token", fs.List)
	}
}

// A token consumed as a root is not offered to the slot loop as well.
// Reading it twice added a second, wrong complaint beside the real
// one — the fallthrough this syntax avoids everywhere else.
func TestParseFormative_ASecondRootIsNotAlsoReadAsAFlag(t *testing.T) {
	fs := glossFault(t, "ml-tpl-ERG")
	if len(fs.List) != 1 {
		t.Fatalf("faults = %+v, want only the duplicate-root complaint", fs.List)
	}
	if !strings.Contains(fs.List[0].Fix, "one root") {
		t.Errorf("wrong complaint kept: %q", fs.List[0].Fix)
	}
}
