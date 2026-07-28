package compose

import (
	"path/filepath"
	"testing"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	rf "github.com/christian-oudard/ithkuil/referentials"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// §4.6's referent categories (Agglomerative, Nomic, Abstract) attach to
// a referential cluster as a prefix or suffix, and the gloss tags the
// referent list with them: "NOM:1m-ERG".
//
// These are ordinary words — "lxa", "lxo", "tļla", "çla" all classify
// as referentials — but the tag had no parser, so every one of them
// failed the gloss-to-compose trip. The gloss syntax rule says ":"
// introduces a tagged body, and this is the second construct that uses
// it after "Ca:"; it was missed when that rule was written down.
func TestReferentialCategory_RoundTrip(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatal(err)
	}
	gl := &gloss.Glosser{Lex: lex, Canonical: true}

	for _, w := range []string{
		"lxa",  // NOM suffix, THM
		"lxo",  // NOM suffix, ERG
		"tļla", // AGM prefix
		"çla",  // NOM prefix
		"la",   // no category at all, the control
	} {
		tok := tokenize.ClassifyWord(w)
		if _, ok := tok.(tokenize.ReferentialWord); !ok {
			t.Errorf("%q no longer classifies as a referential: %T", w, tok)
			continue
		}
		s := gl.Token(tok)
		back, err := ParseToken(s, lex)
		if err != nil {
			t.Errorf("%q glosses to %q, which does not parse back: %v", w, s, err)
			continue
		}
		if again := gl.Token(back); again != s {
			t.Errorf("%q round-tripped %q -> %q", w, s, again)
		}
	}
}

// Every category must survive the trip, not just the two that happen
// to appear in short words.
func TestReferentialCategory_EveryCategory(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatal(err)
	}
	gl := &gloss.Glosser{Lex: lex, Canonical: true}
	erg := g.ERG

	for _, cat := range []rf.Category{rf.Agglomerative, rf.Nomic, rf.Abstract} {
		c := cat
		want := tokenize.ReferentialWord{
			Refs:     []rf.PersonalRef{{Referent: rf.R1m}},
			Category: &c,
			Case:     &erg,
		}
		s := gl.Token(want)
		back, err := ParseToken(s, lex)
		if err != nil {
			t.Errorf("%v: gloss %q does not parse: %v", cat, s, err)
			continue
		}
		got, ok := back.(tokenize.ReferentialWord)
		if !ok {
			t.Errorf("%v: gloss %q parsed as %T", cat, s, back)
			continue
		}
		if got.Category == nil || *got.Category != cat {
			t.Errorf("%v: gloss %q lost the category (got %v)", cat, s, got.Category)
		}
	}
}

// A combination referential carrying affixes loses them on the way
// back: the gloss writes "1m-ERG-CTE-NEG/3", and parseReferentialToken
// stops after the Specification, so re-glossing yields "1m-ERG-CTE".
//
// This is silent loss rather than an error, which is the part that
// matters — a lossy round trip that reports success is worse than one
// that fails. The fix is to keep reading the tail after the Spec slot,
// where affixes and a second case may both appear; it is left undone
// because the tail grammar there (§4.6.2) is shared with the bracketed
// path in buildRefFromTail, and the two should grow one parser rather
// than a second copy.
func TestCombinationReferential_AffixesAreLost(t *testing.T) {
	t.Skip("§4.6.2 combination-referential affix tail is not parsed; see the comment above")

	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatal(err)
	}
	gl := &gloss.Glosser{Lex: lex, Canonical: true}

	want := tokenize.CombinationRefWord{
		Refs:    []rf.PersonalRef{{Referent: rf.R1m}},
		Case:    g.ERG,
		Spec:    g.CTE,
		Affixes: []g.Affix{{Type: g.Type1Affix, Degree: 3, Consonant: "r"}},
	}
	s := gl.Token(want)
	back, err := ParseToken(s, lex)
	if err != nil {
		t.Fatalf("gloss %q does not parse: %v", s, err)
	}
	if again := gl.Token(back); again != s {
		t.Errorf("round trip dropped part of %q, came back as %q", s, again)
	}
}
