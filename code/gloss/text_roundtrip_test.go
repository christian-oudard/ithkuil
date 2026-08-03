package gloss_test

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/roman"
	"github.com/christian-oudard/ithkuil/store"
)

// The word-level round trip was covered from the start, but the span
// was not, because the gloss arm had no writer for one: ParseText read
// a whole span and nothing wrote one back. Glosser.Sentence looks like
// the inverse and is not — it takes a romanization, not a g.Text, and
// returns one string per word rather than the span.
//
// That gap is exactly where the space-separated constructs live. A
// chain glosses to two tokens and a foreign word to a quoted one, so
// both are invisible to a per-word test.
//
//	romanization ─roman.ParseText→ g.Text ─gloss.Text→ gloss
//	  ─gloss.ParseText→ g.Text ─roman.Text→ romanization
//
// Each span is given in its canonical romanization, so the trip is an
// identity rather than a canonicalization.
func TestText_RoundTripsThroughGloss(t *testing.T) {
	lex := loadTextLex(t)
	gl := &gloss.Glosser{Lex: lex}

	for _, span := range []string{
		"mlala",                     // one word, the degenerate span
		"mlala lo",                  // two formatives
		"mlala hla",                 // formative then carrier adjunct
		"hamlal-mlala",              // a chain, which is one word of two tokens
		"hamlal-mlala mlala",        // a chain that does not run to the end
		"mlala hamlal-mlala",        // and one that does not start at it
		"hamlal-mlala hamlal-mlala", // two chains in a row
		"lo mlala lo",               // three words
	} {
		words, err := roman.ParseText(span)
		if err != nil {
			t.Errorf("%q does not parse: %v", span, err)
			continue
		}
		text := gl.Text(words)

		back, err := gloss.ParseText(text, lex)
		if err != nil {
			t.Errorf("%q glossed to %q, which does not parse back: %v", span, text, err)
			continue
		}
		if len(back) != len(words) {
			t.Errorf("%q is %d words, glossed to %q, which is %d",
				span, len(words), text, len(back))
			continue
		}
		got, err := roman.Text(back)
		if err != nil {
			t.Errorf("%q glossed to %q, which does not write back: %v", span, text, err)
			continue
		}
		if got != span {
			t.Errorf("%q glossed to %q, which writes back as %q", span, text, got)
		}
	}
}

// A carrier adjunct makes the word after it foreign, so the span is
// where the quote mark is reachable at all: roman.Tokenize only marks
// a word foreign by looking at its neighbour. Kept apart from the
// table above because the round trip cannot be an identity on the
// romanization — "John" is not Ithkuil and has no canonical spelling.
func TestText_RoundTripsForeignWord(t *testing.T) {
	lex := loadTextLex(t)
	gl := &gloss.Glosser{Lex: lex}

	const span = "hla John"
	words, err := roman.ParseText(span)
	if err != nil {
		t.Fatalf("%q does not parse: %v", span, err)
	}
	text := gl.Text(words)
	if !strings.Contains(text, `"John"`) {
		t.Errorf("%q glossed to %q, want the foreign word quoted", span, text)
	}

	back, err := gloss.ParseText(text, lex)
	if err != nil {
		t.Fatalf("%q glossed to %q, which does not parse back: %v", span, text, err)
	}
	if len(back) != 2 {
		t.Fatalf("%q glossed to %q, which is %d words, want 2", span, text, len(back))
	}
	f, ok := back[1].(g.Foreign)
	if !ok {
		t.Fatalf("word 1 = %T, want g.Foreign", back[1])
	}
	if f.Text != "John" {
		t.Errorf("Foreign.Text = %q, want %q", f.Text, "John")
	}
}

// A foreign word is one word. A loanword is adapted to Ithkuil
// phonology when it is borrowed, so a foreign name arrives already
// spelled as a single Ithkuil-shaped word: the corpus writes Spanish
// as "espanya", not as a word of Spanish. That is why splitting a
// gloss span on plain whitespace is enough, and why the quote wraps
// only what one token can already hold.
//
// This test exists because the parser was briefly taught to keep
// quoted runs together across spaces, for a Foreign holding "John
// Smith" that nothing can produce and the language would not borrow.
func TestText_ForeignWordIsOneWord(t *testing.T) {
	lex := loadTextLex(t)
	gl := &gloss.Glosser{Lex: lex}

	const span = "hla espanya"
	words, err := roman.ParseText(span)
	if err != nil {
		t.Fatalf("%q does not parse: %v", span, err)
	}
	text := gl.Text(words)
	if text != `[CAR] "espanya"` {
		t.Fatalf("gloss = %q, want %q", text, `[CAR] "espanya"`)
	}
	if len(strings.Fields(text)) != 2 {
		t.Errorf("gloss %q is not two whitespace-delimited tokens", text)
	}
	back, err := gloss.ParseText(text, lex)
	if err != nil {
		t.Fatalf("%q does not parse back: %v", text, err)
	}
	if f, ok := back[1].(g.Foreign); !ok || f.Text != "espanya" {
		t.Errorf("word 1 = %#v, want Foreign{espanya}", back[1])
	}
	got, err := roman.Text(back)
	if err != nil {
		t.Fatalf("%q does not write back: %v", text, err)
	}
	if got != span {
		t.Errorf("%q writes back as %q", span, got)
	}
}

// Text is the inverse of ParseText, so a gloss span written by hand
// must survive the trip out to grammar and back to the same gloss.
// This is the direction a person authors in.
func TestText_GlossSpanIsAFixedPoint(t *testing.T) {
	lex := loadTextLex(t)
	gl := &gloss.Glosser{Lex: lex}

	for _, text := range []string{
		"ml",
		"ml ml",
		"T1-ml ml",
		"T1-ml ml ml",
		"S2.CPT-ml-ERG 1m-ERG",
		"[CAR] \"John\"",
		"DSV ml DSV_END",
	} {
		words, err := gloss.ParseText(text, lex)
		if err != nil {
			t.Errorf("%q does not parse: %v", text, err)
			continue
		}
		if got := gl.Text(words); got != text {
			t.Errorf("%q re-glossed as %q", text, got)
		}
	}
}

func loadTextLex(t *testing.T) *lexicon.Lexicon {
	t.Helper()
	st, err := store.Open(store.DefaultPath())
	if err != nil {
		t.Skip("no data store; run tools/build_db.py")
	}
	lex, err := store.LoadLexicon(st)
	if err != nil {
		t.Fatal(err)
	}
	return lex
}
