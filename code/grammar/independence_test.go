package grammar_test

import (
	"github.com/christian-oudard/ithkuil/grammar"
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

// A §4.6.4 personal-reference root names a referent chain — §4.6.4
// calls the Slot III form "a combination Referential affix" — so
// grammar.RefRoot holds the chain rather than the letters that spell it.
//
// It used to hold the cluster as a string, which meant every reader
// decoded it again and grammar.RefRoot{C1: "xh"} was constructible: it glossed
// as "(xh)" and spelled as "aexhal", a word built on a cluster that
// decodes to no referents at all, reported as success by both arms.
// gloss even carried a fallback branch for printing such a cluster
// raw, which existed only because the type permitted it.
func TestRefRootHoldsAChainNotACluster(t *testing.T) {
	r := grammar.RefRoot{Refs: []grammar.PersonalRef{{Referent: grammar.R1m}}}
	if len(r.Refs) != 1 || r.Refs[0].Referent != grammar.R1m {
		t.Errorf("Refs = %v, want [1m]", r.Refs)
	}
}
