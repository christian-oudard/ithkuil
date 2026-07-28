package compose

import (
	"math/rand"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"testing"

	"github.com/christian-oudard/ithkuil/allomorph"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/parse"
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
// The Canonical flag strips the display-only annotations: the quoted
// ' meaning' after the root cluster, and the category code that
// replaces the degree on affixes like MCS. Both are informational, and
// what remains is exactly the authoring syntax — slot hyphens, dotted
// category groups, "head/argument" affixes, and the "Ca:" tag.
//
// This is the strongest invariant we can assert about the round-trip
// without comparing two Formatives structurally, which is fragile
// against alternate but-equivalent encodings (e.g. shortcut vs long
// form). Comparing glosses normalizes those away.
func TestFuzz_GlossComposeRoundTrip(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
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
		f.SlotV = []g.Affix{randomAffix(rng, lex)}
	}
	if rng.Intn(10) < 3 {
		f.SlotVII = []g.Affix{randomAffix(rng, lex)}
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

// randomAffix draws from every affix construct the canonical gloss
// gives a distinct shape to, not just the ordinary "ABBREV/degree"
// one. The notation's central claim is that a token's kind follows
// from its shape, and that claim is only tested where the shapes
// actually meet.
//
// This generator used to draw seven hardcoded clusters, all ordinary
// Type-1/2/3 affixes. That blind spot is why it did not catch the
// collision between a category-valued affix and a §3.9.2 accessor:
// neither construct was ever generated, so the round trip it asserts
// was never asserted over them.
func randomAffix(rng *rand.Rand, lex *lexicon.Lexicon) g.Affix {
	switch rng.Intn(10) {
	case 0:
		// §3.5/§3.7 Ca-stacking: "Ca:MSS.G", or "Ca:{Ca}" all-default.
		return g.Affix{Type: g.CaStackAffix, Consonant: allomorph.ConstructCa(randomSlotVI(rng))}
	case 1, 2:
		// §3.9.2 accessor family: "ACC/INS", "IAC/PRP_3", "CST/ERG".
		c := g.AllCases[rng.Intn(len(g.AllCases))]
		series, degree, high, ok := g.AccessorVx(c)
		if !ok {
			break
		}
		atype, ok := g.SeriesAffixType(series)
		if !ok {
			break
		}
		kind := g.AllAccessorKinds[rng.Intn(len(g.AllAccessorKinds))]
		return g.Affix{Type: atype, Degree: degree, Consonant: g.AccessorCs(kind, high)}
	case 3:
		// §4.6.5 Column-4 referential: "(1m)/AFF".
		return g.Affix{
			Type:      g.Column4Affix,
			Degree:    rng.Intn(9) + 1,
			Consonant: randomRefCluster(rng),
		}
	case 4:
		// §4.6.5 Type-3 referential shortcut: "(1m+2p/BEN)/3".
		return g.Affix{
			Type:      g.Type3Affix,
			Degree:    rng.Intn(9) + 1,
			Consonant: randomRefCluster(rng),
		}
	case 5:
		// A category-valued affix (MCS, PHS, LVL, VAL, IVL, AP1, AP2).
		// Drawn from its own pool rather than from the lexicon at large:
		// there are seven of them among 528, so a uniform draw would
		// land on one about once per run and the guard would be luck.
		if pool := categoryValuedCs(lex); len(pool) > 0 {
			return g.Affix{
				Type:      []g.AffixType{g.Type1Affix, g.Type2Affix, g.Type3Affix}[rng.Intn(3)],
				Degree:    rng.Intn(9) + 1,
				Consonant: pool[rng.Intn(len(pool))],
			}
		}
	case 6:
		// Any lexicon affix, for the ordinary path.
		if pool := lexiconCs(lex); len(pool) > 0 {
			return g.Affix{
				Type:      []g.AffixType{g.Type1Affix, g.Type2Affix, g.Type3Affix}[rng.Intn(3)],
				Degree:    rng.Intn(9) + 1,
				Consonant: pool[rng.Intn(len(pool))],
			}
		}
	}
	cs := []string{"b", "r", "t", "kt", "rf", "lk", "tk"}[rng.Intn(7)]
	atype := []g.AffixType{g.Type1Affix, g.Type2Affix, g.Type3Affix}[rng.Intn(3)]
	return g.Affix{Type: atype, Degree: rng.Intn(9) + 1, Consonant: cs}
}

func randomSlotVI(rng *rand.Rand) g.SlotVI {
	return g.SlotVI{
		Configuration: g.AllConfigurations[rng.Intn(len(g.AllConfigurations))],
		Affiliation:   g.AllAffiliations[rng.Intn(len(g.AllAffiliations))],
		Perspective:   g.AllPerspectives[rng.Intn(len(g.AllPerspectives))],
		Extension:     g.AllExtensions[rng.Intn(len(g.AllExtensions))],
		Essence:       g.AllEssences[rng.Intn(len(g.AllEssences))],
	}
}

// randomRefCluster builds a one- or two-referent cluster of the kind
// §4.6.5's shortcuts put in an affix slot.
func randomRefCluster(rng *rand.Rand) string {
	n := 1
	if rng.Intn(4) == 0 {
		n = 2
	}
	var b strings.Builder
	for i := 0; i < n; i++ {
		b.WriteString(parse.RefC1(g.PersonalRef{
			Referent: g.AllReferents[rng.Intn(len(g.AllReferents))],
			Effect:   g.AllRefEffects[rng.Intn(len(g.AllRefEffects))],
		}))
	}
	return b.String()
}

// lexiconCs and categoryValuedCs are the two affix pools the fuzz
// draws from, sorted because map order is unspecified and the fuzz has
// to stay reproducible from its seed. Both are computed once.
var (
	lexiconCsOnce       sync.Once
	lexiconCsAll        []string
	lexiconCsCategoried []string
)

func buildCsPools(lex *lexicon.Lexicon) {
	lexiconCsOnce.Do(func() {
		for cs, e := range lex.Affixes {
			lexiconCsAll = append(lexiconCsAll, cs)
			// A category-valued affix answers with a code for some
			// (degree, type); which one does not matter here.
			for degree := 1; degree <= 9; degree++ {
				if e.CategoryValue(degree, 1) != "" {
					lexiconCsCategoried = append(lexiconCsCategoried, cs)
					break
				}
			}
		}
		sort.Strings(lexiconCsAll)
		sort.Strings(lexiconCsCategoried)
	})
}

func lexiconCs(lex *lexicon.Lexicon) []string {
	buildCsPools(lex)
	return lexiconCsAll
}

func categoryValuedCs(lex *lexicon.Lexicon) []string {
	buildCsPools(lex)
	return lexiconCsCategoried
}
