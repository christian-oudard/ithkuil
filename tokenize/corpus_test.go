package tokenize_test

import (
	"sort"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// The words in the official example corpus that ClassifyWord does not
// yet claim. This is a drift guard: shrinking it is the point, so a
// failure here means either a regression or an improvement worth
// recording.
var corpusUnclassified = []string{
	// Not Ithkuil: a Spanish proper name standing alone in §3.8,
	// with no carrier adjunct in front of it to mark it as foreign.
	"espanya",

	// Concatenation chains that tryConcatenation rejects.
	"hakšilaölwie-addyëubzattuo",
	"hlaçköé-yeřdö'e",
	"hlurmiô-igulotruxröxḑuökfái",

	// Glottalized Vc, which selects the second case series. We decode
	// some glottalized vowel forms ("ukthili'a" gives LOC) but not
	// these: deleting the glottal makes every one of them parse, with
	// the first-series case.
	"iträlo'a",
	"mma'oxinļ",
	"přa'ölua",
	"pře'ilua",
	"při'olua",
	"wapšorco'a",
	"wupšersaryo'a",

	// Unexplained.
	"zëmse",   // §9.1, a three-referent referential
	"étkwö'e", // §6.2.2; still fails with the stress mark or the glottal removed
}

func TestCorpus_Classification(t *testing.T) {
	words := corpus.Words()
	if len(words) < 500 {
		t.Fatalf("corpus.Words() = %d words, expected the full corpus", len(words))
	}
	var got []string
	for _, w := range words {
		if _, unknown := tokenize.ClassifyWord(w).(tokenize.UnknownWord); unknown {
			got = append(got, w)
		}
	}
	sort.Strings(got)
	want := append([]string(nil), corpusUnclassified...)
	sort.Strings(want)
	if strings.Join(got, " ") != strings.Join(want, " ") {
		t.Errorf("unclassified corpus words drifted\n  got:  %v\n  want: %v", got, want)
	}
}
