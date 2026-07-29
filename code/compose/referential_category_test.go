package compose

import (
	"path/filepath"
	"testing"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
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

	// §4.6 attaches the category affix on whichever side is
	// "phonotactically permissible", so which words exist is decided by
	// the cluster rules: "çla" and "xla" are both sayable, while the
	// suffixed "lça" and "lxa" are not clusters Ithkuil allows to open
	// a word.
	for _, w := range []string{
		"xla",  // NOM prefix, x form
		"sxa",  // NOM suffix, the only legal side on 2m
		"tļla", // AGM prefix
		"çla",  // NOM prefix
		"lwa",  // ABS, which §4.6 writes as a suffix only
		"la",   // no category at all, the control
	} {
		tok := readWord(t, w)
		if _, ok := tok.(g.Referential); !ok {
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

	for _, cat := range []g.RefCategory{g.Agglomerative, g.Nomic, g.Abstract} {
		c := cat
		want := g.Referential{
			Head: g.PersonalHead{
				Refs:     []g.PersonalRef{{Referent: g.R1m}},
				Category: &c,
			},
			Case: erg,
		}
		s := gl.Token(want)
		back, err := ParseToken(s, lex)
		if err != nil {
			t.Errorf("%v: gloss %q does not parse: %v", cat, s, err)
			continue
		}
		got, ok := back.(g.Referential)
		if !ok {
			t.Errorf("%v: gloss %q parsed as %T", cat, s, back)
			continue
		}
		head, ok := got.Head.(g.PersonalHead)
		if !ok || head.Category == nil || *head.Category != cat {
			t.Errorf("%v: gloss %q lost the category (got %+v)", cat, s, got.Head)
		}
	}
}

// A combination referential used to lose its affixes on the way back:
// the gloss wrote "1m-ERG-CTE-NEG/3" and the parser stopped after the
// Specification, so re-glossing gave "1m-ERG-CTE". Silent loss is the
// part that mattered, a lossy round trip reporting success being worse
// than one that fails.
//
// The cause was two copies of the §4.6.2 tail grammar, one for
// bracketed heads and one for bare ones, only the first of which read
// past the Spec slot. They are now one parser.
func TestCombinationReferential_KeepsItsTail(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatal(err)
	}
	gl := &gloss.Glosser{Lex: lex, Canonical: true}
	dat := g.DAT

	for _, want := range []g.CombinationReferential{
		// A bare head, which used to take the lossy path.
		g.CombinationReferential{
			Head:    g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m}}},
			Case:    g.ERG,
			Spec:    g.CTE,
			Affixes: []g.Affix{{Type: g.Type1Affix, Degree: 3, Consonant: "r"}},
		},
		// Affixes plus a stacked case, the full §4.6.2 tail.
		g.CombinationReferential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R2m, Effect: g.BEN}}},
			Case: g.ERG,
			Spec: g.OBJ,
			Affixes: []g.Affix{
				{Type: g.Type1Affix, Degree: 3, Consonant: "r"},
				{Type: g.Type2Affix, Degree: 5, Consonant: "kt"},
			},
			Case2: &dat,
		},
		// A bracketed multi-referent head reaches the same parser.
		g.CombinationReferential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{
				{Referent: g.R1m}, {Referent: g.R2p},
			}},
			Case:    g.THM,
			Spec:    g.BSC,
			Affixes: []g.Affix{{Type: g.Type1Affix, Degree: 3, Consonant: "r"}},
		},
	} {
		s := gl.Token(want)
		back, err := ParseToken(s, lex)
		if err != nil {
			t.Errorf("gloss %q does not parse: %v", s, err)
			continue
		}
		if again := gl.Token(back); again != s {
			t.Errorf("round trip dropped part of %q, came back as %q", s, again)
		}
	}
}
