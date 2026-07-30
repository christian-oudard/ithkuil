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
var morphologyCorpusWords = []string{
	// §3.5
	"wal", "walurx", "waluirx",
	// §4.6.1
	"to", "zua", "laiwe", "ëpgói", "ëztewim", "zëmse", "smoyút",
	"triwejvë", "sme'e", "ka'u", "fo'we'is",
	"smlo", "püwüp", "zäwiez",
	// §4.6.2
	"slex", "poxtanz", "ëtkexpa", "ëlsuoxxéd",
	// §5.3
	"Byala", "pa", "Byula", "pu",
	"Vvrala", "mi", "wurcpei", "urcpulei",
	"Tladatra", "cskava", "Tludatra",
	"Txada", "ku", "Txuda",
	"Waltla", "wele", "lo", "Altlul",
	"Mala", "welu", "wiosadca", "espanya", "Mula",
	// §5.4
	"Wenaya", "ksilo", "apcolowa", "lu", "entyarkena",
	"apcoloyu", "thaxac",
	"Zala", "li", "ksilenei", "malihu",
	"Ercadokh", "elavote", "zzjaduu", "ksivei",
	"Erculafs", "elatwe", "ainsaida", "ksivoto",
	"Yurka", "warrnernei", "ksila", "hluu",
	"Umnalorda", "zulikti", "ksilei", "welene",
	"Umnala", "ksivu", "thu",
	// §5.5
	"Muliutma", "mu", "hlu", "hma", "azxip", "hlie",
	"Wanzviha", "welei", "welie", "thie",
	// §5.6
	"Weilusve", "erculei",
	"Lalutikusvu", "utxolei", "akftyaloe",
	// §5.7
	"willyothoilya", "utplalior", "Ksolaolwau",
	// §5.8 CHC examples
	"Etxula", "welacu", "Itxula",
	"Enula", "laleco", "welaculwu", "etxulie",
	"welacurzu", "welacarzulwu", "welacarzu",
	"Etxularza",
	"Adcsuloeuha", "welecu", "welecarzu",
	"Adcsuleuha", "Adcsularzeuha",
	"ellyuhru", "lalacu", "ellyila",
	"ellyahru", "ellyuleeihru",
	"Ellyula", "ro", "Ellyulerza", "Ellyalerza",
	// §6.0 SPT
	"wuksarseirwa", "wuksarseirwiasta",
	"wustarseirwiaza", "walzarsao", "walzorsurwei",
	"wucpirwao", "wucpirwoltao",
	"wullarsurya", "wupsersaryoa",
	// §8.3 spoken numbers
	"ksalirsa", "gzalui", "walẓärs",
	"cpalärsa", "wapcui", "wansorsë'i", "cpalörs",
	"wallärsa",
}
