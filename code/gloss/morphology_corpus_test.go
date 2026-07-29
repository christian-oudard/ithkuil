package gloss

import (
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/roman"
	"path/filepath"
	"testing"

	"github.com/christian-oudard/ithkuil/lexicon"
)

// TestFullDistance_MorphologyCorpus extends the gloss ↔ compose
// round-trip check to every spec-worked-example word that classifies
// as a FormativeWord. Each word in tokenize/morphology_corpus_test.go
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

	for _, w := range morphologyCorpusWords {
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

// morphologyCorpusWords mirrors the spec-worked-example romanizations in
// tokenize/morphology_corpus_test.go. Kept here as a flat list so the
// compose-level test doesn't reach into another package's test code.
// Update when the source corpus grows.
//
// Spellings come from Quijada's own documents, not from the markdown,
// which had lost diacritics the PDF carries; that file's header
// explains the check and what was dropped for having no source. The
// per-section comments that used to break this list up are gone with
// the flattening — the sections are in that file.
var morphologyCorpusWords = []string{
	"to", "zua", "laiwe", "ëpgói", "ëztewim", "zëmse", "smoyút", "triwejvë",
	"smlo", "püwüp", "zäwiez", "slex", "poxtanz", "ëtkexpa", "ëlsuoxxéd",
	"Byalá", "pa", "Byulá", "pu", "Vvralá", "mi", "wurçpëi", "urçpulëi",
	"Tladatřá", "çkava", "Tludatřá", "Txadá", "ku", "Txudá", "Waltlá", "wele",
	"lo", "Altlúl", "Malá", "welu", "wiosaḑcä", "espanya", "Mulá", "Weňayá",
	"kšilo", "äpçólöwa", "lu", "eňtyarkena", "thaxač", "li", "kšilenëi",
	"Erčädókh", "elavöte", "kšivëi", "Erčuláfs", "elaţwe", "kšivöto", "Yuřká",
	"kšila", "Umňälöřdá", "ẓúlikti", "kšilëi", "welene", "Umňälá", "kšivu",
	"thu", "Muliuţmá", "mu", "hlu", "hma", "ažxíp", "Wanzvihá", "Weilüsve",
	"erčuléi", "utxoléi", "akftyäloë", "willyothoilyá", "utplaliör",
	"Kšölaölwáu", "Etxulá", "welacu", "Itxulá", "Enulá", "laleco",
	"welacülwu", "etxulie", "welacurzu", "welacärzülwu", "welacärzu",
	"Etxulärzá", "welecu", "welecärzu", "Aḑçulëuhá", "Aḑçulärzëuhá",
	"ellyuhrú", "lalacu", "ellyila", "ellyahrú", "Ellyulá", "ro",
	"Ellyulerzá", "Ellyalerzá", "wuksärsëirwa", "wuksärsëirwiasta",
	"wustarsëirwiaza", "wullärsurya", "ksalirsa", "gzalui", "walẓärs",
	"cpalärsa", "wapcui", "wansorsë'i", "cpalörs", "wallärsa",
}
