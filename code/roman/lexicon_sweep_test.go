package roman

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/store"
)

// Two roots in the shipped lexicon cannot be pronounced: §2.16 bars ň
// before k and before y. No formative built on either can be a legal
// word, whatever else it holds, so they are excluded from the sweep
// rather than allowed to bury a real regression.
//
// Every other rule-violating root upstream is retired — \*mps and
// \*mpš, which §2.13 names in its own list of prohibited
// nasal-stop-sibilant conjuncts, and mpm, mpn, mpx with them — and
// retired entries no longer reach the store. These two are not marked
// retired, so they read as typos rather than as deliberate exceptions.
var unpronounceableRoots = map[string]bool{
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
			if err := phonology.CheckText(Formative(f)); err == nil {
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

	w := Formative(f)
	if err := phonology.CheckText(w); err != nil {
		t.Errorf("%s/%d renders to %q, which our own validator rejects: %v",
			label, variant, w, err)
		return
	}
	parsed, err := ParseFormative(w)
	if err != nil {
		t.Errorf("%s/%d renders to %q, which we then cannot parse: %v",
			label, variant, w, err)
		return
	}
	if again := Formative(parsed); again != w {
		t.Errorf("%s/%d: canonicalization is not a fixed point: %q -> %q",
			label, variant, w, again)
	}
}
