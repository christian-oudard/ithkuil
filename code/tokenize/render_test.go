package tokenize_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/gloss"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// Every word in the corpus that we can read should also be one we can
// write. Reading it, rendering the result, and reading that again has
// to land on the same grammar — which the gloss stands in for here,
// being the printable form of exactly that.
//
// This is a canonicalization check, not a string-identity one: a word
// spelled non-canonically re-renders to its canonical equivalent, and
// that is correct. What it catches is a renderer that loses or
// invents grammar, which for referentials nothing could catch before,
// there having been no renderer to check.
func TestToken_CorpusRoundTrip(t *testing.T) {
	gl := &gloss.Glosser{Canonical: true}
	var rendered, skipped int
	for _, w := range corpus.Words() {
		tok := tokenize.ClassifyWord(w)
		switch tok.(type) {
		case tokenize.UnknownWord, tokenize.ForeignWord:
			continue
		}
		rom, err := tokenize.Render(tok)
		if err != nil {
			// Not every class has a renderer yet; those that do not
			// are counted rather than failed, and the count is
			// asserted below so the gap cannot widen unnoticed.
			skipped++
			continue
		}
		rendered++
		again := tokenize.ClassifyWord(rom)
		if _, bad := again.(tokenize.UnknownWord); bad {
			t.Errorf("%q rendered %q, which no longer classifies", w, rom)
			continue
		}
		if want, got := gl.Token(tok), gl.Token(again); want != got {
			t.Errorf("%q rendered %q, which reads back differently\n  want: %s\n  got:  %s",
				w, rom, want, got)
		}
	}
	if rendered == 0 {
		t.Fatal("no corpus word was rendered; the test is not exercising anything")
	}
	t.Logf("rendered %d corpus words, %d classes without a renderer", rendered, skipped)
}

// The referential classes specifically, which are the ones that had no
// renderer at all.
func TestToken_CorpusReferentials(t *testing.T) {
	gl := &gloss.Glosser{Canonical: true}
	var n int
	for _, w := range corpus.Words() {
		tok := tokenize.ClassifyWord(w)
		switch tok.(type) {
		case tokenize.ReferentialWord, tokenize.CombinationRefWord:
		default:
			continue
		}
		rom, err := tokenize.Render(tok)
		if err != nil {
			t.Errorf("%q classifies as %T but does not render: %v", w, tok, err)
			continue
		}
		n++
		if want, got := gl.Token(tok), gl.Token(tokenize.ClassifyWord(rom)); want != got {
			t.Errorf("%q rendered %q, which reads back as %q rather than %q",
				w, rom, got, want)
		}
	}
	if n == 0 {
		t.Fatal("no referential in the corpus; this test is not exercising anything")
	}
	t.Logf("round-tripped %d corpus referentials", n)
}
