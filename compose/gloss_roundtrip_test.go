package compose

import (
	"math/rand"
	"path/filepath"
	"testing"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
)

// TestFuzz_GlossComposeRoundTrip asserts compose ∘ gloss is the
// identity at the canonical (meaning-suppressed) gloss level:
//
//	random Formative
//	  → gloss (Canonical: true)
//	  → compose.Formative
//	  → re-gloss (Canonical: true)
//	must equal the first gloss.
//
// The Canonical flag strips the quoted ' meaning' that the default
// gloss adds for the root cluster; the quoted form is informational
// only and would otherwise need re-stripping before compose can
// re-ingest. Everything else in the gloss output is already compose-
// parseable: slot hyphens, "/" sub-fields, dotted Ca complexes, and
// the "ABBREV:CODE" form for category-valued affixes.
//
// This is the strongest invariant we can assert about the round-trip
// without comparing two Formatives structurally, which is fragile
// against alternate but-equivalent encodings (e.g. shortcut vs long
// form). Comparing glosses normalizes those away.
func TestFuzz_GlossComposeRoundTrip(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	gl := &gloss.Glosser{Lex: lex, Canonical: true}

	const iterations = 1000
	rng := rand.New(rand.NewSource(2026_05_21))
	for i := 0; i < iterations; i++ {
		f := randomFormative(rng, lex)
		s1 := gl.Formative(f)
		f2, err := Formative(s1, lex.Affixes)
		if err != nil {
			t.Errorf("iter %d: compose.Formative(%q): %v\n  formative: %+v",
				i, s1, err, f)
			continue
		}
		s2 := gl.Formative(f2)
		if s1 != s2 {
			t.Errorf("iter %d: gloss round-trip mismatch\n  first:  %s\n  second: %s",
				i, s1, s2)
		}
	}
}

// randomFormative builds a valid Formative biased toward default
// values. Local copy of the generator used by fullparse/fuzz_test.go;
// kept independent so the two fuzz suites can drift on their own
// without coupling.
func randomFormative(rng *rand.Rand, lex *lexicon.Lexicon) g.Formative {
	f := g.MinimalFormative("ml")
	cr := f.Root.(g.CrRoot)
	if rng.Intn(10) < 3 {
		cr.Stem = []g.Stem{g.S0, g.S1, g.S2, g.S3}[rng.Intn(4)]
	}
	if rng.Intn(10) < 3 {
		cr.Version = g.CPT
	}
	if rng.Intn(10) < 3 {
		cr.SlotIV.Function = g.DYN
	}
	if rng.Intn(10) < 3 {
		cr.SlotIV.Specification = []g.Specification{g.BSC, g.CTE, g.CSV, g.OBJ}[rng.Intn(4)]
	}
	if rng.Intn(10) < 3 {
		cr.SlotIV.Context = []g.Context{g.EXS, g.FNC, g.RPS, g.AMG}[rng.Intn(4)]
	}
	f.Root = cr
	if rng.Intn(10) < 4 {
		f.SlotVI = g.SlotVI{
			Configuration: g.AllConfigurations[rng.Intn(len(g.AllConfigurations))],
			Affiliation:   g.AllAffiliations[rng.Intn(len(g.AllAffiliations))],
			Perspective:   g.AllPerspectives[rng.Intn(len(g.AllPerspectives))],
			Extension:     g.AllExtensions[rng.Intn(len(g.AllExtensions))],
			Essence:       g.AllEssences[rng.Intn(len(g.AllEssences))],
		}
	}
	if rng.Intn(10) < 3 {
		f.SlotV = []g.Affix{randomAffix(rng)}
	}
	if rng.Intn(10) < 3 {
		f.SlotVII = []g.Affix{randomAffix(rng)}
	}
	if rng.Intn(10) < 3 {
		val := g.AllValences[rng.Intn(len(g.AllValences))]
		mood := g.FAC
		if rng.Intn(10) < 3 {
			mood = g.AllMoods[rng.Intn(len(g.AllMoods))]
		}
		f.SlotVIII = g.VnCnValence{Valence: val, MoodScope: mood}
	}
	switch rng.Intn(10) {
	case 0, 1, 2, 3, 4, 5, 6:
		f.Final = g.UnframedNominal{Case: g.AllCases[rng.Intn(len(g.AllCases))]}
	case 7, 8:
		f.Final = g.FramedVerbal{Case: g.AllCases[rng.Intn(len(g.AllCases))]}
	default:
		f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
	}
	return f
}

// randomAffix picks from a small set of attested Cs clusters so the
// generated affixes stay phonotactically valid.
func randomAffix(rng *rand.Rand) g.Affix {
	cs := []string{"b", "r", "t", "kt", "rf", "lk", "tk"}[rng.Intn(7)]
	atype := []g.AffixType{g.Type1Affix, g.Type2Affix, g.Type3Affix}[rng.Intn(3)]
	return g.Affix{Type: atype, Degree: rng.Intn(9) + 1, Consonant: cs}
}
