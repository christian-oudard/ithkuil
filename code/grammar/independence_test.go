package grammar_test

import (
	"go/build"
	"strings"
	"testing"
)

// grammar is the centre every other package converts through, and it
// is meant to be ignorant of how Ithkuil is written down. A Case is a
// Case whether or not anyone ever says it.
//
// The package used to import phonology and hold five tables of
// romanized forms — CaseToVc, BiasForm, CarrierTypeForm and the two
// register forms — so the phoneme-independent centre knew what every
// value was spelled as. Those live in parse now, each beside the
// decoder that was already there.
//
// A dependency here is not a compile error, so nothing would catch the
// next one. This does.
func TestGrammarImportsNothingInternal(t *testing.T) {
	pkg, err := build.ImportDir(".", 0)
	if err != nil {
		t.Fatalf("reading the package: %v", err)
	}
	const self = "github.com/christian-oudard/ithkuil"
	for _, imp := range pkg.Imports {
		if strings.HasPrefix(imp, self) {
			t.Errorf("grammar imports %s; the centre should know nothing "+
				"about how words are written or parsed", imp)
		}
	}
}
