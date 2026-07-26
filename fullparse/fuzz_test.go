package fullparse

import (
	"math/rand"
	"reflect"
	"testing"

	"github.com/christian-oudard/ithkuil/allomorph"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/render"
	"github.com/christian-oudard/ithkuil/validation"
)

// TestFuzz_FormativeRoundTrip generates pseudo-random Formatives and
// asserts render → parse round-trips. Seeded for reproducibility:
// failures are deterministic given a fixed seed and iteration count.
//
// Coverage: CrRoot formatives with random Slot II/IV/VI/VIII fields,
// random Slot VII single-affix loadouts, and a random Final variant.
// Deliberately limited to one Slot VII affix and no Slot V affixes so
// the test stays focused on the slot-grammar layer without piling on
// affix-stack interactions (those have their own targeted tests).
//
// Bias toward defaults (~70% of the time per field) catches the
// elision logic alongside non-default carriers.
func TestFuzz_FormativeRoundTrip(t *testing.T) {
	const iterations = 5000
	rng := rand.New(rand.NewSource(2026_05_21))
	for i := 0; i < iterations; i++ {
		f := randomFormative(rng)
		surface := render.Formative(f)
		// Anything we emit has to be a word by our own rules. A
		// round-trip alone can't see this: a surface form both halves
		// mis-handle the same way still comes back equal.
		if r := validation.ValidateWord(surface); !r.Valid && !allomorph.UnresolvedCa(surface) {
			t.Errorf("iter %d: render produced %q, which our own validator rejects: %v\n  formative: %+v",
				i, surface, r.Errors, f)
		}
		parsed, err := Formative(surface)
		if err != nil {
			t.Errorf("iter %d: Formative(%q): %v\n  formative: %+v",
				i, surface, err, f)
			continue
		}
		if !formativesEquivalent(parsed, f) {
			t.Errorf("iter %d: round-trip mismatch (surface %q)\n  want: %+v\n  got:  %+v",
				i, surface, f, parsed)
		}
	}
}

// randomFormative builds a valid Formative biased toward default-value
// fields. The root cluster is always "ml" (a safe, attested Cr).
// fuzzRoots vary the shape of the root, which decides what sits at the
// two ends of the word. "ml" alone never put an approximant last, so
// the elision that stranded the w of "waňtyá" was unreachable. "ţt"
// and "rkw" also open with clusters that block the leading-Vv elision.
var fuzzRoots = []string{"ml", "m", "rkw", "ňty", "ţt", "ļgw"}

func randomFormative(rng *rand.Rand) g.Formative {
	f := g.MinimalFormative(fuzzRoots[rng.Intn(len(fuzzRoots))])
	cr := f.Root.(g.CrRoot)

	if rng.Intn(10) < 3 { // 30% non-default stem
		cr.Stem = pickStem(rng)
	}
	if rng.Intn(10) < 3 {
		cr.Version = g.CPT
	}
	if rng.Intn(10) < 3 {
		cr.SlotIV.Function = g.DYN
	}
	if rng.Intn(10) < 3 {
		cr.SlotIV.Specification = pickSpec(rng)
	}
	if rng.Intn(10) < 3 {
		cr.SlotIV.Context = pickContext(rng)
	}
	f.Root = cr

	if rng.Intn(10) < 4 { // 40% non-default Ca
		f.SlotVI = randomSlotVI(rng)
	}

	if rng.Intn(10) < 3 { // 30% has one Slot VII affix
		f.SlotVII = []g.Affix{randomAffix(rng)}
	}

	if rng.Intn(10) < 3 { // 30% non-default Slot VIII
		f.SlotVIII = randomSlotVIII(rng)
	}

	f.Final = randomFinal(rng)
	return f
}

func pickStem(rng *rand.Rand) g.Stem {
	return []g.Stem{g.S0, g.S1, g.S2, g.S3}[rng.Intn(4)]
}

func pickSpec(rng *rand.Rand) g.Specification {
	return []g.Specification{g.BSC, g.CTE, g.CSV, g.OBJ}[rng.Intn(4)]
}

func pickContext(rng *rand.Rand) g.Context {
	return []g.Context{g.EXS, g.FNC, g.RPS, g.AMG}[rng.Intn(4)]
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

// affixCs picks from a small attested set of Slot VII Cs clusters
// known to encode cleanly. Drawing from the full affix lexicon would
// require depending on lexicon/ from a parse test; this set keeps the
// test self-contained.
var fuzzAffixCs = []string{"b", "r", "t", "kt", "rf", "lk", "tk", "rs"}

func randomAffix(rng *rand.Rand) g.Affix {
	atype := g.Type1Affix
	if rng.Intn(10) < 3 {
		atype = g.Type2Affix
	}
	return g.Affix{
		Type:      atype,
		Degree:    rng.Intn(9) + 1,
		Consonant: fuzzAffixCs[rng.Intn(len(fuzzAffixCs))],
	}
}

func randomSlotVIII(rng *rand.Rand) g.SlotVIII {
	// Stay within Valence-only Slot VIII for now; Aspect/Phase/etc.
	// follow the same Vn+Cn encoding path so this still exercises
	// the moodscope-mood interaction.
	val := g.AllValences[rng.Intn(len(g.AllValences))]
	mood := g.FAC
	if rng.Intn(10) < 3 {
		mood = g.AllMoods[rng.Intn(len(g.AllMoods))]
	}
	return g.VnCnValence{Valence: val, MoodScope: mood}
}

func randomFinal(rng *rand.Rand) g.Final {
	switch rng.Intn(10) {
	case 0, 1, 2, 3, 4, 5, 6: // 70% nominal
		return g.UnframedNominal{Case: g.AllCases[rng.Intn(len(g.AllCases))]}
	case 7, 8:
		// Framed verbal with a case.
		return g.FramedVerbal{Case: g.AllCases[rng.Intn(len(g.AllCases))]}
	default:
		// Unframed verbal. Draw across all nine illocutions and, for
		// ASR, all nine validations: the Vk vowel varies with both, and
		// ultimate stress marks whichever one lands, so pinning this to
		// a single value leaves most of the stressed endings unreached.
		vk := g.AllVk[rng.Intn(len(g.AllVk))]
		if _, ok := vk.(g.Assertive); ok {
			vk = g.Assertive{Validation: g.AllValidations[rng.Intn(len(g.AllValidations))]}
		}
		return g.UnframedVerbal{Vk: vk}
	}
}

// formativesEquivalent compares two Formatives for round-trip
// equality. It mirrors assertRoundTrip's checks but returns bool so
// the fuzz driver can decide how to report.
func formativesEquivalent(got, want g.Formative) bool {
	if !reflect.DeepEqual(got.Root, want.Root) {
		return false
	}
	if got.SlotVI != want.SlotVI {
		return false
	}
	if !reflect.DeepEqual(got.SlotV, want.SlotV) {
		return false
	}
	if !reflect.DeepEqual(got.SlotVII, want.SlotVII) {
		return false
	}
	if !reflect.DeepEqual(got.SlotVIII, want.SlotVIII) {
		return false
	}
	if !reflect.DeepEqual(got.Final, want.Final) {
		return false
	}
	return got.Concat == want.Concat
}
