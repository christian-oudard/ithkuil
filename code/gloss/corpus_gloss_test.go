package gloss

import (
	"path/filepath"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/roman"
)

// TestCorpusGloss_ComposesBack drives the arm SPEC claims, over the
// whole corpus rather than the formatives alone: the canonical gloss is
// both what Glosser.Word writes and what ParseWord reads, so every
// corpus word we can classify should survive the trip out and back.
//
// A concatenation chain glosses to its members separated by a space, so
// it is not one whitespace-delimited token and goes back through
// ParseText, which rejoins the members on the Slot I marker each
// dependent carries. Nineteen corpus words are chains, and the count is
// held so that path cannot quietly stop being exercised.
func TestCorpusGloss_ComposesBack(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	gl := &Glosser{Lex: lex}
	var chains, ok int
	for _, w := range corpus.Words() {
		word, err := roman.ParseWord(w)
		if err != nil {
			continue
		}
		canonical := gl.Word(word, nil, 0)
		var back g.Word
		if strings.Contains(canonical, " ") {
			chains++
			words, err := ParseText(canonical, lex)
			if err != nil {
				t.Errorf("%q glosses to %q, which does not parse back: %v", w, canonical, err)
				continue
			}
			if len(words) != 1 {
				t.Errorf("%q glosses to %q, which parses back as %d words rather than one chain",
					w, canonical, len(words))
				continue
			}
			back = words[0]
		} else {
			back, err = ParseWord(canonical, lex)
			if err != nil {
				t.Errorf("%q glosses to %q, which does not parse back: %v", w, canonical, err)
				continue
			}
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
		t.Errorf("%d corpus words are chains, want 19", chains)
	}
	t.Logf("%d corpus words round-tripped through the canonical gloss", ok)
}
