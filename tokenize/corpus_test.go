package tokenize_test

import (
	"sort"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
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
	"hakšilaölwie-addyabzëuttuo",
	"hakšilaölwie-addyëubzattuo",
	"hlaçköé-yeřdö'e",
	"hlurmiô-igulotruxröxḑuökfái",
	"hrelu-azčojhaillöelyá",

	// A vowel in Vx position that isn't in the §3.5 Vx table. These
	// used to read as degree-0 affixes because the Vx lookup defaulted
	// silently; they now fail honestly.
	"itriloalö",
	"kšölaölwáu",
	"yamţröalwa'o",

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

// §5.6 forms WH-questions from the PVS referential plus the IVL1/4
// affix -inļ. The case vowel carries a glottal in "When?", which only
// resolves if the combination-referential path merges glottal vowels.
// Glossed without a lexicon, so the affix shows as its raw cluster.
func TestCorpus_WHQuestions(t *testing.T) {
	cases := []struct{ word, gloss string }{
		{"Mmiexinļ", "REF[PVS]-PUR.BSC-nļ/4"},     // Why?
		{"Mma'oxinļ", "REF[PVS]-CNR.BSC-nļ/4"},    // When?
		{"Nnioxinļ", "REF[PVS/BEN]-TRA.BSC-nļ/4"}, // To/for whose benefit?
		{"Ňňeöxinļ", "REF[PVS/DET]-RSL.BSC-nļ/4"}, // Resulting detrimentally in what?
		{"Mmauxinļ", "REF[PVS]-PRP.BSC-nļ/4"},     // Whose?
	}
	gl := &gloss.Glosser{}
	for _, c := range cases {
		tok := tokenize.ClassifyWord(c.word)
		if _, ok := tok.(tokenize.CombinationRefWord); !ok {
			t.Errorf("ClassifyWord(%s) = %T, want CombinationRefWord", c.word, tok)
			continue
		}
		if got := gl.Token(tok); got != c.gloss {
			t.Errorf("gloss(%s) = %q, want %q", c.word, got, c.gloss)
		}
	}
}

// TestCorpus_LoneConcatMarker covers §3.1.8: a C_C marker in Slot I
// means another formative follows, and the romanization joins the two
// with a hyphen. So a word with no hyphen cannot carry one.
//
// These thirteen come from the archive with no hyphen anywhere near
// them, and we read each as a chain dependent standing on its own.
// Every one begins with an h-cluster that ParseCc claims: "h" and "hw"
// are the bare Type-1 and Type-2 markers, and "hl"/"hm"/"hr"/"hn" pair
// a type with a Slot IV/VI shortcut.
//
// The fix is not simply to reject a lone C_C here. "hnas" is the case
// that shows why: tokenize.go documents the intent that it read as a
// Naming carrier, but ParseCarrier rejects it, because a §4.5 carrier
// adjunct is C_P plus a case vowel and "as" is not one. The formative
// reading is what isCarrierToken then leans on to scope a following
// foreign name. Rejecting the lone C_C without settling what these
// words are instead just moves the wrong answer somewhere else.
//
// serialize refuses to encode them: a lone C_C would make the decoder
// swallow the next token into a chain that never terminates.
func TestCorpus_LoneConcatMarker(t *testing.T) {
	t.Skip("known defect: a C_C marker outside a hyphenated chain")
	for _, w := range []string{
		"höňkoň", "hläxëinļa", "hrabtü", "hliöčmái", "hanļkakceilliu",
		"haeřalei", "hliamžá", "hmaxanļa", "hwëivholüpsao", "hafçäxeuppëu",
		"hoňkoň", "hlamëu", "hramëu", "hnas",
	} {
		tok := tokenize.ClassifyWord(w)
		fw, ok := tok.(tokenize.FormativeWord)
		if !ok {
			continue
		}
		if fw.Formative.Concat != g.ConcatNone {
			t.Errorf("ClassifyWord(%s) = lone formative with Concat %v", w, fw.Formative.Concat)
		}
	}
}
