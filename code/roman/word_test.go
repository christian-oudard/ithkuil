package roman_test

import (
	"github.com/christian-oudard/ithkuil/roman"
	"reflect"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
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
	gl := &gloss.Glosser{}
	var rendered, skipped int
	for _, w := range corpus.Words() {
		tok, err := roman.ParseWord(w)
		if err != nil {
			continue
		}
		if _, foreign := tok.(g.Foreign); foreign {
			continue
		}
		rom, err := roman.Word(tok)
		if err != nil {
			// Every class the corpus exercises has a renderer now, so
			// this is a failure rather than the tallied gap it used to
			// be. The count below asserts the gap stays closed.
			t.Errorf("%q classified as %T, which has no renderer: %v", w, tok, err)
			skipped++
			continue
		}
		rendered++
		again, err := roman.ParseWord(rom)
		if err != nil {
			t.Errorf("%q rendered %q, which no longer reads: %v", w, rom, err)
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
	if skipped != 0 {
		t.Errorf("%d corpus words have no renderer, want 0", skipped)
	}
	t.Logf("rendered %d corpus words", rendered)
}

// The referential classes specifically, which are the ones that had no
// renderer at all.
func TestToken_CorpusReferentials(t *testing.T) {
	gl := &gloss.Glosser{}
	var n int
	for _, w := range corpus.Words() {
		tok, err := roman.ParseWord(w)
		if err != nil {
			continue
		}
		switch tok.(type) {
		case g.Referential, g.CombinationReferential:
		default:
			continue
		}
		rom, err := roman.Word(tok)
		if err != nil {
			t.Errorf("%q reads as %T but does not render: %v", w, tok, err)
			continue
		}
		n++
		back, err := roman.ParseWord(rom)
		if err != nil {
			t.Errorf("%q rendered %q, which no longer reads: %v", w, rom, err)
			continue
		}
		if want, got := gl.Token(tok), gl.Token(back); want != got {
			t.Errorf("%q rendered %q, which reads back as %q rather than %q",
				w, rom, got, want)
		}
	}
	if n == 0 {
		t.Fatal("no referential in the corpus; this test is not exercising anything")
	}
	t.Logf("round-tripped %d corpus referentials", n)
}

// TestRender_EverySumVariant walks the grammar.Word sum itself rather
// than the corpus, which exercises only the classes the corpus happens
// to use. Each case is a real word of its class, read and written back.
func TestRender_EverySumVariant(t *testing.T) {
	for _, tc := range []struct {
		word string
		want g.Word
	}{
		{"mlala", g.Formative{}},
		{"hlamröé-mlala", &g.Chain{}},
		{"řřx", g.Bias(0)},
		{"ha", g.RegisterMarker{}},
		{"hai", g.RegisterMarker{}},
		{"uhlaini", g.ModularAdjunct{}},
		{"ač", g.SingleAffixAdjunct{}},
		{"dohast", g.MultipleAffixAdjunct{}},
		{"hmo", g.CarrierAdjunct{}},
		{"lo", g.Referential{}},
		{"slex", g.CombinationReferential{}},
	} {
		w, err := roman.ParseWord(tc.word)
		if err != nil {
			t.Errorf("%q does not classify: %v", tc.word, err)
			continue
		}
		if reflect.TypeOf(w) != reflect.TypeOf(tc.want) {
			t.Errorf("%q classified as %T, want %T", tc.word, w, tc.want)
			continue
		}
		if _, err := roman.Word(w); err != nil {
			t.Errorf("%q (%T) does not render: %v", tc.word, w, err)
		}
	}
}
