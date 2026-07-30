package serialize

import (
	"bytes"
	"math/rand"
	"reflect"
	"strings"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/roman"
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
	nomic := g.Nomic
	dat := g.DAT
	cases := []g.Word{
		// Formative — minimal.
		g.MinimalFormative("m"),
		// Bias.
		g.DOL,
		// Register start.
		g.RegisterMarker{Register: g.DSV},
		// Register end.
		g.RegisterMarker{Register: g.DSV},
		// Parsing adjunct.
		// Carrier.
		g.CarrierAdjunct{Type: g.Carrier, Case: g.ERG},
		// Modular — default.
		g.ModularAdjunct{},
		// Modular — with content + scope + reach.
		g.ModularAdjunct{
			Scope:   g.ModularScopeParent,
			Reach:   g.ModularReachFormative,
			Content: []g.SlotVIII{g.VnCnValence{Valence: g.MNO, MoodScope: g.FAC}},
		},
		// Single affix.
		g.SingleAffixAdjunct{
			Affix: g.Affix{Type: g.Type2Affix, Degree: 5, Consonant: "kt"},
			Scope: g.ScopeVIIDom,
		},
		// Multi-affix.
		g.MultipleAffixAdjunct{
			First:      g.Affix{Type: g.Type1Affix, Degree: 3, Consonant: "r"},
			Rest:       []g.Affix{{Type: g.Type2Affix, Degree: 5, Consonant: "kt"}},
			FirstScope: g.ScopeVSub,
			RestScope:  g.ScopeVIIDom,
		},
		// Referential — single ref.
		g.Referential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
			Case: g.THM,
		},
		// Referential — full shape: suppletive head, a second referent
		// carrying its own case, and RPV essence.
		g.Referential{
			Head: g.SuppletiveHead{Type: g.Quotative},
			Case: g.ERG,
			Second: &g.SecondReferent{
				Case: g.DAT,
				Refs: []g.PersonalRef{{Referent: g.R2p, Effect: g.DET}},
			},
			RpvEssence: true,
		},
		// Combination ref.
		g.CombinationReferential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R2m, Effect: g.BEN}}},
			Case: g.ERG,
			Spec: g.BSC,
		},
		// Multi-affix with no Rest — the run holds only the head, so
		// Rest must come back nil rather than an empty slice.
		g.MultipleAffixAdjunct{
			First:      g.Affix{Type: g.Type1Affix, Degree: 3, Consonant: "r"},
			FirstScope: g.ScopeVSub,
			RestScope:  g.ScopeVIIDom,
		},
		// Carrier at its default case, which the encoding elides.
		g.CarrierAdjunct{Type: g.Naming, Case: g.THM},
		// Modular carrying the maximum three Vn/Cn pairs.
		g.ModularAdjunct{
			Scope: g.ModularScopeConcat,
			Reach: g.ModularReachAdjacent,
			Content: []g.SlotVIII{
				g.VnCnValence{Valence: g.PRL, MoodScope: g.FAC},
				g.VnCnAspect{Aspect: g.SQN, MoodScope: g.FAC},
				g.VnCnLevel{Level: g.MAX, MoodScope: g.FAC, Absolute: true},
			},
		},
		// A second case with no referent of its own, which §4.6.1
		// stacks onto the head instead.
		g.Referential{
			Head:   g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.Rma, Effect: g.NEU}}},
			Case:   g.THM,
			Second: &g.SecondReferent{Case: g.ABS},
		},
		// Combination ref at its default Case and Spec, both elided.
		g.CombinationReferential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.Rpvs, Effect: g.DET}}},
			Case: g.THM,
			Spec: g.BSC,
		},
		// Combination ref with no Refs, which the corpus contains and
		// which used to write zero ref bytes but read one back,
		// desynchronising every token after it in the stream.
		g.CombinationReferential{
			Head: g.SuppletiveHead{Type: g.Quotative},
			Case: g.ERG,
			Spec: g.CTE,
		},
		// A category modifier on the head, which rides along with the
		// referent chain rather than in a slot of its own.
		g.Referential{
			Head: g.PersonalHead{
				Refs:     []g.PersonalRef{{Referent: g.Rmi, Effect: g.NEU}},
				Category: &nomic,
			},
			Case: g.ERG,
		},
		// Combination ref with affixes + case2.
		g.CombinationReferential{
			Head:    g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
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

func TestMarshalTokens_RoundTrip(t *testing.T) {
	tokens := []g.Word{
		g.DOL,
		g.MinimalFormative("m"),
		g.Referential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
			Case: g.THM,
		},
	}
	b, err := MarshalTokens(tokens)
	if err != nil {
		t.Fatalf("MarshalTokens: %v", err)
	}
	got, err := UnmarshalTokens(b)
	if err != nil {
		t.Fatalf("UnmarshalTokens: %v", err)
	}
	if len(got) != len(tokens) {
		t.Fatalf("token-stream len: got %d, want %d", len(got), len(tokens))
	}
	for i := range tokens {
		if !equalTokens(tokens[i], got[i]) {
			t.Errorf("token %d mismatch\n  want: %+v\n  got:  %+v", i, tokens[i], got[i])
		}
	}
}

// TestFuzz_BinaryRoundTrip generates random Formatives and asserts
// that Marshal followed by Unmarshal preserves byte equality on
// re-marshal — the strongest invariant for a binary codec.
func TestFuzz_BinaryRoundTrip(t *testing.T) {
	rng := rand.New(rand.NewSource(2026_05_21))
	const iterations = 500
	for i := 0; i < iterations; i++ {
		f := randomFormative(rng)
		t1 := f
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

// TestFinal_AllValues covers the flat Final byte, which packs three
// sum-type variants into one byte with no variant tag. Every case in
// both framings and every illocution must survive.
func TestFinal_AllValues(t *testing.T) {
	var finals []g.Final
	for _, c := range g.AllCases {
		finals = append(finals, g.UnframedNominal{Case: c}, g.FramedVerbal{Case: c})
	}
	for _, v := range g.AllValidations {
		finals = append(finals, g.UnframedVerbal{Vk: g.Assertive{Validation: v}})
	}
	for _, k := range leafVk {
		finals = append(finals, g.UnframedVerbal{Vk: k})
	}
	for _, want := range finals {
		b, err := putFinal(nil, want)
		if err != nil {
			t.Errorf("putFinal(%+v): %v", want, err)
			continue
		}
		if len(b) != 1 {
			t.Errorf("putFinal(%+v) wrote %d bytes, want 1", want, len(b))
		}
		got, err := getFinal(b[0])
		if err != nil {
			t.Errorf("getFinal(%d): %v", b[0], err)
			continue
		}
		if !reflect.DeepEqual(got, want) {
			t.Errorf("Final round-trip: %+v → %+v", want, got)
		}
	}
}

// TestSlotVIII_AllValues covers every Vn variant against every Mood,
// including the Aspect values above 31 and the Level Absolute flag —
// the three cases that leave the one-byte form.
func TestSlotVIII_AllValues(t *testing.T) {
	var slots []g.SlotVIII
	for _, m := range g.AllMoods {
		for _, v := range g.AllValences {
			slots = append(slots, g.VnCnValence{Valence: v, MoodScope: m})
		}
		for _, p := range g.AllPhases {
			slots = append(slots, g.VnCnPhase{Phase: p, MoodScope: m})
		}
		for _, e := range g.AllEffects {
			slots = append(slots, g.VnCnEffect{Effect: e, MoodScope: m})
		}
		for _, l := range g.AllLevels {
			slots = append(slots,
				g.VnCnLevel{Level: l, MoodScope: m, Absolute: false},
				g.VnCnLevel{Level: l, MoodScope: m, Absolute: true})
		}
		for _, a := range g.AllAspects {
			slots = append(slots, g.VnCnAspect{Aspect: a, MoodScope: m})
		}
	}
	for _, want := range slots {
		b, err := putSlotVIII(nil, want)
		if err != nil {
			t.Errorf("putSlotVIII(%+v): %v", want, err)
			continue
		}
		got, n, err := getSlotVIII(b)
		if err != nil {
			t.Errorf("getSlotVIII(%+v): %v", want, err)
			continue
		}
		if n != len(b) {
			t.Errorf("%+v: consumed %d of %d bytes", want, n, len(b))
		}
		if !reflect.DeepEqual(got, want) {
			t.Errorf("SlotVIII round-trip: %+v → %+v", want, got)
		}
	}
}

// TestCa_AllValues covers the Slot VI selector byte and its
// mixed-radix escape across the whole 3840-value space.
func TestCa_AllValues(t *testing.T) {
	for _, cfg := range g.AllConfigurations {
		for _, aff := range g.AllAffiliations {
			for _, per := range g.AllPerspectives {
				for _, ext := range g.AllExtensions {
					for _, ess := range g.AllEssences {
						want := g.SlotVI{
							Configuration: cfg, Affiliation: aff,
							Perspective: per, Extension: ext, Essence: ess,
						}
						b := putCa(nil, want)
						got, n, err := getCa(b)
						if err != nil {
							t.Fatalf("getCa(%+v): %v", want, err)
						}
						if n != len(b) {
							t.Errorf("%+v: consumed %d of %d bytes", want, n, len(b))
						}
						if got != want {
							t.Errorf("Ca round-trip: %+v → %+v", want, got)
						}
					}
				}
			}
		}
	}
}

// TestNonFormativeEscape pins the invariant that lets a formative go
// untagged: no formative may encode to the two-byte prefix that
// introduces every other token. The near miss is a concatenated
// formative with a plain root, which shares the first byte.
func TestNonFormativeEscape(t *testing.T) {
	for _, c := range []g.ConcatenationStatus{g.Type1, g.Type2} {
		f := g.MinimalFormative("ml")
		f.Concat = c
		b, err := putFormative(nil, f)
		if err != nil {
			t.Fatal(err)
		}
		if b[0] != nonFormative[0] {
			t.Fatalf("expected a first-byte collision to test, got %x", b[0])
		}
		if b[1] == nonFormative[1] {
			t.Errorf("Concat %v encoded to the non-formative prefix %x", c, b[:2])
		}
	}
	// A stream mixing both must survive, since it is the escape that
	// keeps the decoder's dispatch unambiguous.
	dep := g.MinimalFormative("ml")
	dep.Concat = g.Type1
	tokens := []g.Word{
		&g.Chain{
			Head: g.MinimalFormative("l"),
			Tail: []g.Formative{dep},
		},
		g.DOL,
		g.MinimalFormative("m"),
	}
	b, err := MarshalTokens(tokens)
	if err != nil {
		t.Fatal(err)
	}
	got, err := UnmarshalTokens(b)
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(got, tokens) {
		t.Errorf("mixed stream\n  want: %+v\n  got:  %+v", tokens, got)
	}
}

// TestDefaultElision pins the compaction the layout exists for: a
// formative with everything at its grammatical default must cost only
// the header byte plus its root cluster.
func TestDefaultElision(t *testing.T) {
	b, err := putFormative(nil, g.MinimalFormative("ml"))
	if err != nil {
		t.Fatal(err)
	}
	if len(b) != 3 {
		t.Errorf("minimal formative encoded to %d bytes (%x), want 3", len(b), b)
	}
}

// TestChain_FreeFraming pins the property the chain layout is built
// on: a chain costs exactly the sum of its formatives. The Cc marker
// each dependent already carries is what delimits the run, so there
// is no tag, no count, and no terminator to pay for.
func TestChain_FreeFraming(t *testing.T) {
	dep := g.MinimalFormative("ml")
	dep.Concat = g.Type1
	head := g.MinimalFormative("l")

	depBytes, err := putFormative(nil, dep)
	if err != nil {
		t.Fatal(err)
	}
	headBytes, err := putFormative(nil, head)
	if err != nil {
		t.Fatal(err)
	}
	chain, err := MarshalWord(&g.Chain{Head: head, Tail: []g.Formative{dep}})
	if err != nil {
		t.Fatal(err)
	}
	if want := len(depBytes) + len(headBytes); len(chain) != want {
		t.Errorf("chain encoded to %d bytes, want %d (the parts, with no framing)", len(chain), want)
	}
}

// TestChain_RejectsUngrammatical covers the states the romanization cannot
// express either: a Cc on a lone formative, a chain whose parent
// carries a Cc, and a dependent that carries none.
func TestChain_RejectsUngrammatical(t *testing.T) {
	withConcat := func(cluster string, c g.ConcatenationStatus) g.Formative {
		f := g.MinimalFormative(cluster)
		f.Concat = c
		return f
	}
	for _, tc := range []struct {
		name  string
		token g.Word
	}{
		{"lone formative with a Cc", withConcat("ml", g.Type1)},
		{"parent with a Cc", &g.Chain{
			Head: withConcat("l", g.Type2),
			Tail: []g.Formative{withConcat("ml", g.Type1)},
		}},
		{"dependent without a Cc", &g.Chain{
			Head: g.MinimalFormative("l"),
			Tail: []g.Formative{g.MinimalFormative("ml")},
		}},
		{"chain of one", &g.Chain{Head: g.MinimalFormative("l")}},
	} {
		if _, err := MarshalWord(tc.token); err == nil {
			t.Errorf("%s: encoded without error, want a rejection", tc.name)
		}
	}
}

// TestFuzz_ChainRoundTrip runs random chains through the codec, mixed
// into a stream so a chain that over- or under-read would corrupt the
// tokens after it.
func TestFuzz_ChainRoundTrip(t *testing.T) {
	rng := rand.New(rand.NewSource(2026_07_26))
	for i := 0; i < 200; i++ {
		tokens := []g.Word{
			randomChain(rng),
			g.DOL,
			randomFormative(rng),
		}
		b, err := MarshalTokens(tokens)
		if err != nil {
			t.Fatalf("iter %d: marshal: %v", i, err)
		}
		got, err := UnmarshalTokens(b)
		if err != nil {
			t.Fatalf("iter %d: unmarshal: %v", i, err)
		}
		if len(got) != len(tokens) {
			t.Fatalf("iter %d: got %d tokens, want %d", i, len(got), len(tokens))
		}
		b2, err := MarshalTokens(got)
		if err != nil {
			t.Fatalf("iter %d: re-marshal: %v", i, err)
		}
		if !reflect.DeepEqual(b, b2) {
			t.Fatalf("iter %d: bytes differ after a round trip\n  %x\n  %x", i, b, b2)
		}
	}
}

// randomChain builds a chain of one to three dependents plus a parent.
func randomChain(rng *rand.Rand) *g.Chain {
	c := &g.Chain{Head: randomFormative(rng)}
	for n := 1 + rng.Intn(3); n > 0; n-- {
		dep := randomFormative(rng)
		dep.Concat = []g.ConcatenationStatus{g.Type1, g.Type2}[rng.Intn(2)]
		c.Tail = append(c.Tail, dep)
	}
	return c
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
	switch rng.Intn(10) {
	case 8:
		f.Root = g.CsRoot{
			Cs: "kt", Degree: rng.Intn(10), Version: cr.Version,
			Function: cr.SlotIV.Function, Context: cr.SlotIV.Context,
		}
	case 9:
		f.Root = g.RefRoot{Refs: []g.PersonalRef{{Referent: g.R1m}}, Version: cr.Version, SlotIV: cr.SlotIV}
	}
	// Concat is deliberately left at its default: it is only legal on
	// a chain dependent, so randomChain sets it.
	if rng.Intn(10) < 3 {
		f.SlotV = randomAffixes(rng)
	}
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
		f.SlotVII = randomAffixes(rng)
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

// randomAffixes builds a run of one to three affixes, exercising the
// continuation bit that lets an affix run carry no count prefix.
func randomAffixes(rng *rand.Rand) []g.Affix {
	n := rng.Intn(3) + 1
	out := make([]g.Affix, n)
	for i := range out {
		out[i] = g.Affix{
			Type:      []g.AffixType{g.Type1Affix, g.Type2Affix, g.Type3Affix}[rng.Intn(3)],
			Degree:    rng.Intn(9) + 1,
			Consonant: []string{"b", "r", "t", "kt", "rf", "lk", "tk"}[rng.Intn(7)],
		}
	}
	return out
}

// equalTokens compares two WordTokens for structural equality. Uses
// reflect.DeepEqual for the convenience of working through interface
// fields and slices.
func equalTokens(a, b g.Word) bool {
	return reflect.DeepEqual(a, b)
}

// TestForeignWord_RoundTrip covers the one token whose meaning is its
// text. A carrier scopes a name or quotation that is deliberately not
// Ithkuil, so the letters go out verbatim, including case, which the
// rest of the pipeline normalizes away.
func TestForeignWord_RoundTrip(t *testing.T) {
	for _, s := range []string{
		"John", "", "Ithkuil", "Ⅳ", "naïve café", "日本語",
		strings.Repeat("x", 200), // past the one-byte uvarint length
	} {
		tokens := []g.Word{
			g.CarrierAdjunct{Type: g.Naming},
			g.Foreign{Text: s},
			g.MinimalFormative("ml"),
		}
		b, err := MarshalTokens(tokens)
		if err != nil {
			t.Fatalf("%q: marshal: %v", s, err)
		}
		got, err := UnmarshalTokens(b)
		if err != nil {
			t.Fatalf("%q: unmarshal: %v", s, err)
		}
		if !reflect.DeepEqual(got, tokens) {
			t.Errorf("%q\n  want: %+v\n  got:  %+v", s, tokens, got)
		}
	}
}

// TestModularMood_Restored covers the one field the codec derives
// rather than stores. MarksMood says whether the next formative is
// verbal, which decides whether the adjunct's Cn reads as Mood or as
// Case-Scope, and it comes off the neighbouring tokens. Storing it
// would be storing a fact already present in the stream, so the
// decoder recomputes it instead.
func TestModularMood_Restored(t *testing.T) {
	verbal := g.MinimalFormative("ml")
	verbal.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
	nominal := g.MinimalFormative("ml")

	for _, tc := range []struct {
		name string
		next g.Formative
		want bool
	}{
		{"before a verbal formative", verbal, true},
		{"before a nominal formative", nominal, false},
	} {
		tokens := []g.Word{
			g.ModularAdjunct{
				Content: []g.SlotVIII{g.VnCnAspect{Aspect: g.RTR, MoodScope: g.SUB}},
			},
			tc.next,
		}
		b, err := MarshalTokens(tokens)
		if err != nil {
			t.Fatalf("%s: marshal: %v", tc.name, err)
		}
		got, err := UnmarshalTokens(b)
		if err != nil {
			t.Fatalf("%s: unmarshal: %v", tc.name, err)
		}
		// Whether the adjunct's neighbour is verbal is asked of the
		// span, not stored, so it survives the trip by being derived
		// from the words either side rather than by being encoded.
		if verbal, found := roman.ModularIsVerbal(got, 0); !found || verbal != tc.want {
			t.Errorf("%s: verbal = %v (found %v), want %v", tc.name, verbal, found, tc.want)
		}
		if !reflect.DeepEqual(got, tokens) {
			t.Errorf("%s\n  want: %+v\n  got:  %+v", tc.name, tokens, got)
		}
	}
}

// TestCluster_Sizes pins the cluster layout's whole point: a consonant
// is five bits, and a cluster of three fits in two bytes rather than
// three. It also pins where packing stops, since lengths one and two
// gain nothing from it and keep a byte per consonant so the byte's
// value stays the consonant's own index.
func TestCluster_Sizes(t *testing.T) {
	for _, tc := range []struct {
		cluster string
		want    int
	}{
		{"m", 1},
		{"ml", 2},
		{"kpt", 2},  // 1 bit of framing + 3x5 = 16 bits exactly
		{"kptm", 3}, // 3 + 20 = 23 bits
	} {
		b, err := EncodeCluster(tc.cluster)
		if err != nil {
			t.Fatalf("%s: %v", tc.cluster, err)
		}
		if len(b) != tc.want {
			t.Errorf("%q encoded to %d bytes (%x), want %d", tc.cluster, len(b), b, tc.want)
		}
		got, n, err := DecodeCluster(b)
		if err != nil || got != tc.cluster || n != len(b) {
			t.Errorf("%q round-trip: got %q, %d bytes, err %v", tc.cluster, got, n, err)
		}
	}
	// An unpacked cluster's later consonants keep their plain index, so
	// the same consonant is the same byte wherever it lands.
	b, _ := EncodeCluster("ml")
	l, _ := EncodePhoneme("l")
	if b[1] != l {
		t.Errorf("second consonant of \"ml\" is byte %d, want its phoneme index %d", b[1], l)
	}
}

// TestCluster_RejectsVowel pins the assumption the five-bit alphabet
// rests on: clusters are consonants only, so a vowel has no encoding
// and must fail loudly rather than silently truncate to five bits.
func TestCluster_RejectsVowel(t *testing.T) {
	if _, err := EncodeCluster("ma"); err == nil {
		t.Error("encoded a cluster containing a vowel, want a rejection")
	}
}

// TestCluster_AllLengths round-trips every length the code can express,
// including the escape past four.
func TestCluster_AllLengths(t *testing.T) {
	cs := []string{"m", "l", "k", "p", "t", "r", "s", "n", "z", "v", "f", "x"}
	for n := 1; n <= len(cs); n++ {
		want := strings.Join(cs[:n], "")
		b, err := EncodeCluster(want)
		if err != nil {
			t.Fatalf("len %d: %v", n, err)
		}
		got, used, err := DecodeCluster(b)
		if err != nil {
			t.Fatalf("len %d: decode: %v", n, err)
		}
		if got != want || used != len(b) {
			t.Errorf("len %d: got %q using %d of %d bytes, want %q", n, got, used, len(b), want)
		}
	}
	if _, err := EncodeCluster(strings.Repeat("m", maxCluster+1)); err == nil {
		t.Error("encoded a cluster past the length code's range, want a rejection")
	}
}
