package roman_test

import (
	"github.com/christian-oudard/ithkuil/roman"
	"testing"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/store"
)

// The example sentences were transcribed and glossed by a third party,
// not taken verbatim from Quijada, and two of them name a root their
// form does not contain. Both mean 'child', which is C_R "l" at stem 2;
// both were written with a neighbouring consonant instead.
//
// The glosses are otherwise dependable. Checking every quoted root
// meaning against the lexicon entry for the C_R the parser finds, 151
// of 153 agree, including where the wording diverges (bšt glossed
// 'priest' against the lexicon's "religious leader", tr 'approach'
// against "linear motion"). These two are the only cases where the
// consonant itself is wrong.
//
// Pinned rather than corrected: examples.txt reproduces the document as
// received, so the defect stays and this test fails if it is ever
// repaired, or if our reading of either form changes.
var misglossedChildRoots = []struct {
	section string
	word    string // as the corpus has it
	cr      string // the root it actually contains
	meant   string // the form its gloss describes
	meantAs string // and how that form glosses
}{
	// §4.8.4 and §6.1.3 carry the same sentence, glossed
	// 'child'-G-VOC. G Perspective under a w- shortcut also needs a
	// series-2 V_V, so ei- rather than e-.
	{"4.8.4", "Weru'i", "r", "Weilu'i", "S2.PRC-l-G-VOC"},

	// Glossed [default CA]-stem2/prc-'child'-DPX-IND, which contradicts
	// itself as well: DPX is a Configuration, so a formative carrying
	// it does not have a default C_A. Without the shortcut the word
	// needs its Slot IV V_R, hence -a- rather than nothing.
	{"5.1.6", "wesu", "s", "elasu", "S2.PRC-l-DPX-IND"},
}

func TestCorpusMisglossedChildRoots(t *testing.T) {
	st, err := store.Open(store.DefaultPath())
	if err != nil {
		t.Skip("no data store; run tools/build_db.py")
	}
	lex, err := lexicon.LoadFromStore(st)
	if err != nil {
		t.Fatal(err)
	}
	gl := &gloss.Glosser{Lex: lex}

	for _, c := range misglossedChildRoots {
		f, err := roman.ParseFormative(c.word)
		if err != nil {
			t.Errorf("§%s %s: %v", c.section, c.word, err)
			continue
		}
		root, ok := f.Root.(g.CrRoot)
		if !ok {
			t.Errorf("§%s %s: root is %T, want a C_R root", c.section, c.word, f.Root)
			continue
		}
		if root.Cluster != c.cr {
			t.Errorf("§%s %s: C_R = %q, want %q", c.section, c.word, root.Cluster, c.cr)
		}
		if root.Cluster == "l" {
			t.Errorf("§%s %s: now contains the 'child' root; the corpus has been "+
				"corrected and this entry can go", c.section, c.word)
		}

		meant, err := roman.ParseFormative(c.meant)
		if err != nil {
			t.Errorf("§%s %s: %v", c.section, c.meant, err)
			continue
		}
		if got := gl.Formative(meant); got != c.meantAs {
			t.Errorf("§%s %s glosses %q, want %q", c.section, c.meant, got, c.meantAs)
		}
	}
}
