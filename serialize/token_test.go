package serialize

import (
	"bytes"
	"math/rand"
	"reflect"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/referentials"
	"github.com/christian-oudard/ithkuil/surface"
	"github.com/christian-oudard/ithkuil/tokenize"
)

func TestPhoneme_RoundTrip(t *testing.T) {
	for s, b := range phonemeToByte {
		got, err := DecodePhoneme(b)
		if err != nil {
			t.Errorf("DecodePhoneme(%d): %v", b, err)
			continue
		}
		if got != s {
			t.Errorf("phoneme %q encoded to %d, decoded back to %q", s, b, got)
		}
	}
}

func TestCluster_RoundTrip(t *testing.T) {
	clusters := []string{"m", "ml", "kpt", "ţř", "rcp", "tk", "ksp"}
	for _, c := range clusters {
		b, err := EncodeCluster(c)
		if err != nil {
			t.Fatalf("EncodeCluster(%q): %v", c, err)
		}
		got, n, err := DecodeCluster(b)
		if err != nil {
			t.Fatalf("DecodeCluster(%q): %v", c, err)
		}
		if n != len(b) {
			t.Errorf("cluster %q consumed %d of %d bytes", c, n, len(b))
		}
		if got != c {
			t.Errorf("cluster round-trip %q → %q", c, got)
		}
	}
}

func TestMarshalWord_AllTokenTypes(t *testing.T) {
	thm := g.THM
	erg := g.ERG
	dat := g.DAT
	carrType := g.Quotative
	cases := []tokenize.WordToken{
		// Formative — minimal.
		tokenize.FormativeWord{Formative: g.MinimalFormative("m")},
		// Bias.
		tokenize.BiasWord{Bias: g.DOL},
		// Register start.
		tokenize.RegisterStartWord{Register: g.DSV},
		// Register end.
		tokenize.RegisterEndWord{Register: g.DSV},
		// Parsing adjunct.
		tokenize.ParsingAdjunctWord{Adjunct: g.ParsingAdjunct{Stress: surface.Ultimate}},
		// Carrier.
		tokenize.CarrierWord{Carrier: g.CarrierAdjunct{Type: g.Carrier, Case: g.ERG}},
		// Modular — default.
		tokenize.ModularWord{Modular: g.ModularAdjunct{}},
		// Modular — with content + scope + reach.
		tokenize.ModularWord{Modular: g.ModularAdjunct{
			Scope:   g.ModularScopeParent,
			Reach:   g.ModularReachFormative,
			Content: []g.SlotVIII{g.VnCnValence{Valence: g.MNO, MoodScope: g.FAC}},
		}},
		// Single affix.
		tokenize.SingleAffixWord{Affix: g.SingleAffixAdjunct{
			Affix: g.Affix{Type: g.Type2Affix, Degree: 5, Consonant: "kt"},
			Scope: g.ScopeVIIDom,
		}},
		// Multi-affix.
		tokenize.MultipleAffixWord{Affixes: g.MultipleAffixAdjunct{
			First:      g.Affix{Type: g.Type1Affix, Degree: 3, Consonant: "r"},
			Rest:       []g.Affix{{Type: g.Type2Affix, Degree: 5, Consonant: "kt"}},
			FirstScope: g.ScopeVSub,
			RestScope:  g.ScopeVIIDom,
		}},
		// Referential — single ref.
		tokenize.ReferentialWord{
			Refs: []referentials.PersonalRef{{Referent: referentials.R1m, Effect: referentials.NEU}},
			Case: &thm,
		},
		// Referential — full shape (carrier head + case2 + RpvEssence).
		tokenize.ReferentialWord{
			Carrier:    &carrType,
			Case:       &erg,
			Case2:      &dat,
			RpvEssence: true,
		},
		// Combination ref.
		tokenize.CombinationRefWord{
			Refs: []referentials.PersonalRef{{Referent: referentials.R2m, Effect: referentials.BEN}},
			Case: g.ERG,
			Spec: g.BSC,
		},
		// Combination ref with affixes + case2.
		tokenize.CombinationRefWord{
			Refs:    []referentials.PersonalRef{{Referent: referentials.R1m, Effect: referentials.NEU}},
			Case:    g.ERG,
			Spec:    g.OBJ,
			Affixes: []g.Affix{{Type: g.Type1Affix, Degree: 1, Consonant: "r"}},
			Case2:   &dat,
		},
	}
	for i, want := range cases {
		b, err := MarshalWord(want)
		if err != nil {
			t.Errorf("case %d %T: marshal: %v", i, want, err)
			continue
		}
		got, n, err := UnmarshalWord(b)
		if err != nil {
			t.Errorf("case %d %T: unmarshal: %v (bytes: %x)", i, want, err, b)
			continue
		}
		if n != len(b) {
			t.Errorf("case %d %T: consumed %d of %d bytes", i, want, n, len(b))
		}
		if !equalTokens(want, got) {
			t.Errorf("case %d %T mismatch\n  want: %+v\n  got:  %+v", i, want, want, got)
		}
	}
}

func TestMarshalSentence_RoundTrip(t *testing.T) {
	thm := g.THM
	sentence := []tokenize.WordToken{
		tokenize.BiasWord{Bias: g.DOL},
		tokenize.FormativeWord{Formative: g.MinimalFormative("m")},
		tokenize.ReferentialWord{
			Refs: []referentials.PersonalRef{{Referent: referentials.R1m, Effect: referentials.NEU}},
			Case: &thm,
		},
	}
	b, err := MarshalSentence(sentence)
	if err != nil {
		t.Fatalf("MarshalSentence: %v", err)
	}
	got, err := UnmarshalSentence(b)
	if err != nil {
		t.Fatalf("UnmarshalSentence: %v", err)
	}
	if len(got) != len(sentence) {
		t.Fatalf("sentence len: got %d, want %d", len(got), len(sentence))
	}
	for i := range sentence {
		if !equalTokens(sentence[i], got[i]) {
			t.Errorf("sentence token %d mismatch\n  want: %+v\n  got:  %+v", i, sentence[i], got[i])
		}
	}
}

// TestFuzz_BinaryRoundTrip generates random Formatives and asserts
// that Marshal followed by Unmarshal preserves byte equality on
// re-marshal — the strongest invariant for a binary codec.
func TestFuzz_BinaryRoundTrip(t *testing.T) {
	lex, err := lexicon.LoadDefault()
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	_ = lex
	rng := rand.New(rand.NewSource(2026_05_21))
	const iterations = 500
	for i := 0; i < iterations; i++ {
		f := randomFormative(rng)
		t1 := tokenize.FormativeWord{Formative: f}
		b1, err := MarshalWord(t1)
		if err != nil {
			t.Errorf("iter %d: marshal: %v\n  f: %+v", i, err, f)
			continue
		}
		got, _, err := UnmarshalWord(b1)
		if err != nil {
			t.Errorf("iter %d: unmarshal: %v\n  bytes: %x", i, err, b1)
			continue
		}
		b2, err := MarshalWord(got)
		if err != nil {
			t.Errorf("iter %d: re-marshal: %v", i, err)
			continue
		}
		if !bytes.Equal(b1, b2) {
			t.Errorf("iter %d: byte mismatch\n  first:  %x\n  second: %x", i, b1, b2)
		}
	}
}

func randomFormative(rng *rand.Rand) g.Formative {
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
		cs := []string{"b", "r", "t", "kt", "rf", "lk", "tk"}[rng.Intn(7)]
		atype := []g.AffixType{g.Type1Affix, g.Type2Affix, g.Type3Affix}[rng.Intn(3)]
		f.SlotVII = []g.Affix{{
			Type: atype, Degree: rng.Intn(9) + 1, Consonant: cs,
		}}
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

// equalTokens compares two WordTokens for structural equality. Uses
// reflect.DeepEqual for the convenience of working through interface
// fields and slices.
func equalTokens(a, b tokenize.WordToken) bool {
	return reflect.DeepEqual(a, b)
}
