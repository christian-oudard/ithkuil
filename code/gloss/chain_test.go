package gloss_test

import (
	"path/filepath"
	"testing"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/roman"
)

// A chain is one word written with hyphens, and its canonical gloss is
// its members separated by a space, so ParseWord is handed one member
// at a time and never sees the chain. Every other word class round-
// tripped through gloss and this one came back as loose formatives.
func TestParseText_RejoinsAChain(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Skip("no lexicon")
	}
	gl := gloss.Glosser{Lex: lex, Canonical: true}
	for _, word := range []string{
		"hakšiţé-alcialu'a",
		"hlešvie-galoktähá",
	} {
		tok, err := roman.ParseWord(word)
		if err != nil {
			t.Errorf("ClassifyWord(%s): %v", word, err)
			continue
		}
		if _, ok := tok.(*g.Chain); !ok {
			t.Fatalf("%s classifies as %T, so this test is not exercising a chain", word, tok)
		}
		words, err := gloss.ParseText(gl.Token(tok), lex)
		if err != nil {
			t.Errorf("%s: ParseText: %v", word, err)
			continue
		}
		if len(words) != 1 {
			t.Errorf("%s composed to %d words, want the one chain", word, len(words))
			continue
		}
		chain, ok := words[0].(*g.Chain)
		if !ok {
			t.Errorf("%s composed to %T, want *g.Chain", word, words[0])
			continue
		}
		if got, want := gl.Token(chain), gl.Token(tok); got != want {
			t.Errorf("%s composed to a different chain\n  want: %s\n  got:  %s", word, want, got)
		}
	}
}

// A dependent carries a Slot I marker and the parent carries none, so
// dependents with nothing to attach to are not a sentence.
func TestParseText_ChainWithNoParent(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Skip("no lexicon")
	}
	if _, err := gloss.ParseText("T1-ksq-STA.OBJ.EXS-MDS-COR", lex); err == nil {
		t.Error("a lone concatenated formative composed into a text without complaint")
	}
}
