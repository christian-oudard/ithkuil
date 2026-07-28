package fullparse_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/fullparse"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/render"
	"github.com/christian-oudard/ithkuil/store"
	"github.com/christian-oudard/ithkuil/validation"
)

// Four roots in the community lexicon cannot be pronounced. The spec
// names two of them outright: §2.13 gives \*mps and \*mpš in its own
// list of prohibited nasal-stop-sibilant conjuncts, and §2.16 bars ň
// before k and before y. No formative built on these can be a legal
// word, whatever else it holds, so they are excluded from the sweep
// rather than allowed to bury a real regression.
//
// mps and mpš are daggered upstream, the sheet's mark for a retired
// word, and every other §2.13 violator in the lexicon is daggered too.
// ňkhw and řẓňy are not, so those two read as typos rather than as
// deliberate exceptions. Removing a root from this map is the test that
// the lexicon has been repaired.
var unpronounceableRoots = map[string]bool{
	"mps":  true, // §2.13, named in the spec as \*mps; retired but still shipped
	"mpš":  true, // §2.13, named in the spec as \*mpš; retired but still shipped
	"ňkhw": true, // §2.16, ň before k
	"řẓňy": true, // §2.16, ň before y
}

// Every root and every affix in the shipped lexicon, built into a
// formative, rendered, validated, and parsed back.
//
// This is the complement of the fuzz, which varies the slots against a
// handful of hand-picked roots. Here the slots are fixed and the
// lexicon is what varies, so a cluster that only some Cr or Cs shapes
// can produce has somewhere to show up. The stressed-diphthong and
// word-final-approximant defects were both of that kind: reachable
// from a root shape the fuzz never drew.
func TestLexiconSweep(t *testing.T) {
	st, err := store.Open(store.DefaultPath())
	if err != nil {
		t.Skip("no data store; run tools/build_db.py")
	}
	lex, err := lexicon.LoadFromStore(st)
	if err != nil {
		t.Fatal(err)
	}
	if len(lex.Roots) < 1000 || len(lex.Affixes) < 100 {
		t.Fatalf("lexicon looks truncated: %d roots, %d affixes",
			len(lex.Roots), len(lex.Affixes))
	}

	// A handful of endings rather than one: the Final decides the
	// stress, and stress placement is what several past defects turned
	// on.
	finals := []g.Final{
		g.UnframedNominal{Case: g.THM},
		g.UnframedNominal{Case: g.ERG},
		g.FramedVerbal{Case: g.ERG},
		g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}},
	}

	t.Run("roots", func(t *testing.T) {
		for cluster := range lex.Roots {
			if unpronounceableRoots[cluster] {
				continue
			}
			for i, final := range finals {
				f := g.MinimalFormative(cluster)
				f.Final = final
				sweep(t, f, cluster, i)
			}
		}
		for cluster := range unpronounceableRoots {
			if _, ok := lex.Roots[cluster]; !ok {
				t.Errorf("%q is no longer in the lexicon; drop it from unpronounceableRoots", cluster)
				continue
			}
			f := g.MinimalFormative(cluster)
			if r := validation.ValidateWord(render.Formative(f)); r.Valid {
				t.Errorf("%q now validates; drop it from unpronounceableRoots", cluster)
			}
		}
	})

	t.Run("affixes", func(t *testing.T) {
		for cs := range lex.Affixes {
			for degree := 1; degree <= 9; degree++ {
				f := g.MinimalFormative("ml")
				f.SlotVII = []g.Affix{{Type: g.Type1Affix, Degree: degree, Consonant: cs}}
				sweep(t, f, cs, degree)
			}
		}
	})
}

// sweep renders one formative and asserts the three properties that
// hold of every well-formed word: it is legal by our own phonotactics,
// it parses back, and re-rendering the parse is a fixed point.
func sweep(t *testing.T, f g.Formative, label string, variant int) {
	t.Helper()
	defer func() {
		if r := recover(); r != nil {
			t.Errorf("%s/%d: render panicked: %v", label, variant, r)
		}
	}()

	w := render.Formative(f)
	if r := validation.ValidateWord(w); !r.Valid {
		t.Errorf("%s/%d renders to %q, which our own validator rejects: %v",
			label, variant, w, r.Errors)
		return
	}
	parsed, err := fullparse.Formative(w)
	if err != nil {
		t.Errorf("%s/%d renders to %q, which we then cannot parse: %v",
			label, variant, w, err)
		return
	}
	if again := render.Formative(parsed); again != w {
		t.Errorf("%s/%d: canonicalization is not a fixed point: %q -> %q",
			label, variant, w, again)
	}
}
