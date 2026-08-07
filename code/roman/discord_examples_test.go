package roman

import (
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
)

// TestDiscordExamples checks the parser against the curated verdicts in
// the Discord word list. The verdicts are about the words; this asserts
// we agree with them.
//
// A correct word must classify as something, and an incorrect one must
// not. An entry marked with a leading "!" is one we already know we
// disagree with, so it is reported rather than failed. If such an entry
// starts agreeing, the test says so: the defect is fixed and the marker
// should come off.
//
// The list is a testing record kept outside the repo, so a checkout
// without it skips here rather than failing. See
// corpus.DiscordExamplesPath.
func TestDiscordExamples(t *testing.T) {
	examples, err := corpus.DiscordExamples()
	if err != nil {
		t.Fatal(err)
	}
	if len(examples) == 0 {
		t.Skipf("no curated word list at %s", corpus.DiscordExamplesPath())
	}
	counts := map[corpus.Verdict]int{}
	for _, ex := range examples {
		counts[ex.Verdict]++
		_, err := ParseWord(ex.Word)
		unreadable := err != nil
		agree := (ex.Verdict == corpus.Correct) != unreadable

		if ex.Defect {
			if agree {
				t.Errorf("%s: marked a known defect but we now agree it is %s; drop the \"!\"",
					ex.Word, ex.Verdict)
			} else {
				t.Logf("known defect: %s is %s (%s: %s)", ex.Word, ex.Verdict, ex.Rule, ex.Reason)
			}
			continue
		}
		if !agree {
			if ex.Verdict == corpus.Correct {
				t.Errorf("%s: correct (%s: %s) but we cannot read it", ex.Word, ex.Rule, ex.Reason)
			} else {
				t.Errorf("%s: incorrect (%s: %s) but we accept it", ex.Word, ex.Rule, ex.Reason)
			}
		}
	}
	if counts[corpus.Correct] == 0 || counts[corpus.Incorrect] == 0 {
		t.Fatalf("%s needs both verdicts; got %v", corpus.DiscordExamplesPath(), counts)
	}
	t.Logf("verdicts: %d correct, %d incorrect", counts[corpus.Correct], counts[corpus.Incorrect])
}
