package gloss_test

import (
	"testing"
	"unicode"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/gloss"
	"github.com/christian-oudard/ithkuil/roman"
	"github.com/christian-oudard/ithkuil/store"
)

// The gloss is an authoring syntax, not only an output format:
// gloss.ParseFormative reads back what gloss.Formative writes. So every
// character in it has to be typable on an ordinary keyboard, which is
// why the root is spelled in ASCII digraphs and the affix type is "_2"
// rather than a Unicode subscript.
//
// Two designs have been proposed that broke this, "Ø" for a zero value
// and "§" for a sentence-starter marker. Both read as natural notation
// and neither can be typed. It also slipped through in the code: an
// affix Cs outside the lexicon fell back to the raw cluster, so any
// gloss naming one printed š, č or ţ.
//
// Both configurations are checked. Without a lexicon every affix takes
// the raw-cluster path, which is the one that was wrong; with a
// lexicon only the affixes the lexicon does not name take it.
func TestCanonicalGlossIsASCII(t *testing.T) {
	st, err := store.Open(store.DefaultPath())
	if err != nil {
		t.Skip("no data store; run tools/build_db.py")
	}
	lex, err := store.LoadLexicon(st)
	if err != nil {
		t.Fatal(err)
	}
	for _, tc := range []struct {
		name string
		gl   *gloss.Glosser
	}{
		{"no lexicon", &gloss.Glosser{}},
		{"with lexicon", &gloss.Glosser{Lex: lex}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			for _, w := range corpus.Words() {
				tok, err := roman.ParseWord(w)
				if err != nil {
					continue
				}
				g := tc.gl.Token(tok)
				for _, r := range g {
					if r > unicode.MaxASCII {
						t.Errorf("canonical gloss of %q is %q, which holds non-ASCII %q (U+%04X)",
							w, g, r, r)
						break
					}
				}
			}
		})
	}
}
