package gloss

import (
	"path/filepath"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// TestCorpusGloss_ComposesBack drives the arm SPEC claims, over the
// whole corpus rather than the formatives alone: the canonical gloss is
// both what Glosser.Word writes and what ParseWord reads, so every
// corpus word we can classify should survive the trip out and back.
//
// It is a drift guard on a known gap, not a clean pass. A concatenation
// chain glosses to its members separated by a space, and ParseWord takes
// one whitespace-delimited token, so a chain's gloss is not an input to
// it at all. Nineteen corpus words are chains. Until the gloss syntax
// gives chains a separator of their own, or ParseText learns to
// reassemble them, that number is the thing to watch: it must not grow,
// and nothing outside it may fail.
func TestCorpusGloss_ComposesBack(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	gl := &Glosser{Lex: lex, Canonical: true}
	var chains, ok int
	for _, w := range corpus.Words() {
		word, err := tokenize.ClassifyWord(w)
		if err != nil {
			continue
		}
		canonical := gl.Word(word, nil, 0)
		if strings.Contains(canonical, " ") {
			chains++
			continue
		}
		back, err := ParseWord(canonical, lex)
		if err != nil {
			t.Errorf("%q glosses to %q, which does not parse back: %v", w, canonical, err)
			continue
		}
		if again := gl.Word(back, nil, 0); again != canonical {
			t.Errorf("%q glosses to %q, which parses back to something else: %q", w, canonical, again)
			continue
		}
		ok++
	}
	if ok == 0 {
		t.Fatal("no corpus word round-tripped; the test is not exercising anything")
	}
	if chains != 19 {
		t.Errorf("%d words gloss with a space and cannot be parsed back, want 19", chains)
	}
	t.Logf("%d corpus words round-tripped through the canonical gloss", ok)
}
