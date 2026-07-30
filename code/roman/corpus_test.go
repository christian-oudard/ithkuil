package roman_test

import (
	"github.com/christian-oudard/ithkuil/roman"
	"sort"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
)

// The words in the official example corpus that ClassifyWord does not
// yet claim. This is a drift guard: shrinking it is the point, so a
// failure here means either a regression or an improvement worth
// recording.
var corpusUnclassified = []string{
	// Not Ithkuil: a Spanish proper name standing alone in §3.8,
	// with no carrier adjunct in front of it to mark it as foreign.
	"espanya",

	// §4.5.2, a garbled copy of the §4.4.7 sentence, which does
	// classify. Both carry the same English, and the dependent's Slot
	// IV V_R and its Slot V V_X are swapped, putting "ëu" in Slot IV
	// where no V_R is: Slot IV needs eight values, so it uses forms 1-4
	// and 6-9 and skips form 5. The case label moved too — the gloss
	// names TSP (uö) where the word ends in uo, which is CMM. §4.4.7 is
	// the sound copy.
	"hakšilaölwie-addyëubzattuo",

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
		if _, err := roman.ParseWord(w); err != nil {
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
		tok, _ := roman.ParseWord(c.word)
		if _, ok := tok.(g.CombinationReferential); !ok {
			t.Errorf("ParseWord(%s) = %T, want CombinationRefWord", c.word, tok)
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
// These fourteen come from the archive with no hyphen anywhere near
// them, and each used to read as a chain dependent standing on its
// own. Every one begins with an h-cluster that ParseCc claims: "h" and
// "hw" are the bare Type-1 and Type-2 markers, and "hl"/"hm"/"hr"/"hn"
// pair a type with a Slot IV/VI shortcut.
//
// They are now UnknownWord, which is the honest answer. Some are
// foreign names in Ithkuil letters ("hoňkoň", "höňkoň" — Hong Kong);
// some are a dependent someone quoted without its parent, which is
// discourse about a word rather than a word. What none of them is is a
// standalone formative, and serialize could not encode one anyway: a
// lone C_C would make the decoder swallow the next token into a chain
// that never terminates.
//
// The list is the words that exposed the defect; the corpus sweep
// below is the guard, since a lone C_C is a shape rather than a
// vocabulary.
func TestCorpus_LoneConcatMarker(t *testing.T) {
	words := []string{
		"höňkoň", "hläxëinļa", "hrabtü", "hliöčmái", "hanļkakceilliu",
		"haeřalei", "hliamžá", "hmaxanļa", "hwëivholüpsao", "hafçäxeuppëu",
		"hoňkoň", "hlamëu", "hramëu", "hnas",
	}
	words = append(words, corpus.Words()...)
	for _, w := range words {
		word, err := roman.ParseWord(w)
		if err != nil {
			continue
		}
		if fw, ok := word.(g.Formative); ok && fw.Concat != g.ConcatNone {
			t.Errorf("ParseWord(%s) = lone formative with Concat %v", w, fw.Concat)
		}
	}
}
