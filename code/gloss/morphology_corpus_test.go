package gloss

import (
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/roman"
	"path/filepath"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/lexicon"
)

// TestFullDistance_MorphologyCorpus extends the gloss ↔ compose
// round-trip check to every spec-worked-example word that classifies
// as a FormativeWord. Each word in corpus/morphology_examples.txt
// that successfully classifies and parses goes through:
//
//	romanization ─tokenize→ FormativeWord
//	         ─fullparse→ grammar.Formative
//	         ─gloss(Canonical)→ G1
//	         ─ParseFormative→ Formative'
//	         ─gloss(Canonical)→ G2
//
// G1 must equal G2 — compose is the inverse of canonical gloss on
// every spec example we can parse. Non-formative tokens (adjuncts,
// referentials, unknowns) are silently skipped: those have their own
// classification tests, and compose Phase 3 only covers CrRoot,
// CsRoot, and RefRoot formatives.
func TestFullDistance_MorphologyCorpus(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	gl := &Glosser{Lex: lex, Canonical: true}

	for _, w := range corpus.MorphologyWords() {
		t.Run(w, func(t *testing.T) {
			tok, err := roman.ParseWord(w)
			if err != nil {
				t.Skipf("not readable: %v", err)
			}
			if _, ok := tok.(g.Formative); !ok {
				t.Skipf("not a formative: %T", tok)
			}
			f, err := roman.ParseFormative(w)
			if err != nil {
				t.Skipf("fullparse rejects %q: %v", w, err)
			}
			s1 := gl.Formative(f)
			f2, err := ParseFormative(s1, lex.Affixes)
			if err != nil {
				t.Fatalf("ParseFormative(%q): %v\n  formative: %+v", s1, err, f)
			}
			s2 := gl.Formative(f2)
			if s1 != s2 {
				t.Errorf("gloss round-trip mismatch\n  romanization: %s\n  first:   %s\n  second:  %s",
					w, s1, s2)
			}
		})
	}
}
