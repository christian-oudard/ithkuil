package fullparse_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// TestJudged checks the parser against the curated verdicts in
// corpus/judged.txt. The verdicts are about the words; this asserts
// that we agree with them.
//
// An "ok" word must classify as something, and a "bad" word must not
// classify as the thing its rule forbids. "unsure" asserts nothing by
// design: those words are recorded so a later reader can see they were
// examined and left open, rather than silently counted as evidence
// either way.
func TestJudged(t *testing.T) {
	counts := map[corpus.Verdict]int{}
	for _, j := range corpus.Judged() {
		counts[j.Verdict]++
		tok := tokenize.ClassifyWord(j.Word)
		_, unknown := tok.(tokenize.UnknownWord)
		switch j.Verdict {
		case corpus.OK:
			if unknown {
				t.Errorf("%s: judged ok (%s: %s) but we cannot classify it",
					j.Word, j.Rule, j.Reason)
			}
		case corpus.Bad:
			if !unknown {
				t.Errorf("%s: judged bad (%s: %s) but we read it as %T",
					j.Word, j.Rule, j.Reason, tok)
			}
		}
	}
	if counts[corpus.OK] == 0 || counts[corpus.Bad] == 0 {
		t.Fatalf("judged.txt needs both ok and bad entries; got %v", counts)
	}
	t.Logf("verdicts: %d ok, %d bad, %d unsure",
		counts[corpus.OK], counts[corpus.Bad], counts[corpus.Unsure])
}
