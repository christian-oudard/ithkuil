package fullparse

import (
	"reflect"
	"testing"

	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/render"
)

// assertRoundTrip renders f, parses the result, and asserts that the
// parsed Formative matches f field-by-field (except SlotI/SlotIShortcut
// pointers, which are compared by dereference).
func assertRoundTrip(t *testing.T, name string, f g.Formative) {
	t.Helper()
	surface := render.Formative(f)
	parsed, err := ParseFormative(surface)
	if err != nil {
		t.Errorf("%s: ParseFormative(%q): %v", name, surface, err)
		return
	}
	if parsed.Stress != f.Stress {
		t.Errorf("%s: Stress: got %v, want %v (surface %q)", name, parsed.Stress, f.Stress, surface)
	}
	if parsed.SlotII != f.SlotII {
		t.Errorf("%s: SlotII: got %v, want %v (surface %q)", name, parsed.SlotII, f.SlotII, surface)
	}
	if parsed.SlotIII != f.SlotIII {
		t.Errorf("%s: SlotIII: got %q, want %q (surface %q)", name, parsed.SlotIII, f.SlotIII, surface)
	}
	if parsed.SlotIV != f.SlotIV {
		t.Errorf("%s: SlotIV: got %v, want %v (surface %q)", name, parsed.SlotIV, f.SlotIV, surface)
	}
	if parsed.SlotVI != f.SlotVI {
		t.Errorf("%s: SlotVI: got %v, want %v (surface %q)", name, parsed.SlotVI, f.SlotVI, surface)
	}
	if !reflect.DeepEqual(parsed.SlotVII, f.SlotVII) {
		t.Errorf("%s: SlotVII: got %v, want %v (surface %q)", name, parsed.SlotVII, f.SlotVII, surface)
	}
	if !reflect.DeepEqual(parsed.SlotVIII, f.SlotVIII) {
		t.Errorf("%s: SlotVIII: got %v, want %v (surface %q)", name, parsed.SlotVIII, f.SlotVIII, surface)
	}
	if !reflect.DeepEqual(parsed.SlotIX, f.SlotIX) {
		t.Errorf("%s: SlotIX: got %v, want %v (surface %q)", name, parsed.SlotIX, f.SlotIX, surface)
	}
	gotCc := derefConcat(parsed.SlotI)
	wantCc := derefConcat(f.SlotI)
	if gotCc != wantCc {
		t.Errorf("%s: SlotI: got %v, want %v (surface %q)", name, gotCc, wantCc, surface)
	}
	gotSc := derefShortcut(parsed.SlotIShortcut)
	wantSc := derefShortcut(f.SlotIShortcut)
	if gotSc != wantSc {
		t.Errorf("%s: SlotIShortcut: got %v, want %v (surface %q)", name, gotSc, wantSc, surface)
	}
}

func derefConcat(p *g.ConcatenationStatus) interface{} {
	if p == nil {
		return nil
	}
	return *p
}

func derefShortcut(p *g.CcShortcut) interface{} {
	if p == nil {
		return nil
	}
	return *p
}

// TestRoundTrip_Stress asserts parse(render(F)).Stress == F.Stress for
// every reasonable combination of stress + Slot IX shape. This is the
// guard rail that catches accidental category shifts where the rendered
// surface form's default stress disagrees with the Formative's intended
// stress.
func TestRoundTrip_Stress(t *testing.T) {
	mkFormative := func(stress g.Stress, slotIX g.SlotIX) g.Formative {
		f := g.MinimalFormative("ml")
		f.Stress = stress
		f.SlotIX = slotIX
		return f
	}
	cases := []struct {
		name string
		f    g.Formative
	}{
		{
			"nominal-penultimate",
			mkFormative(g.Penultimate, g.CaseSlot{Case: g.THM}),
		},
		{
			"nominal-penultimate-erg",
			mkFormative(g.Penultimate, g.CaseSlot{Case: g.ERG}),
		},
		{
			"verbal-ultimate-asr-inf",
			mkFormative(g.Ultimate, g.Assertive{Validation: g.INF}),
		},
		{
			"verbal-ultimate-dir",
			mkFormative(g.Ultimate, g.Directive{}),
		},
		{
			"framed-antepenultimate",
			mkFormative(g.Antepenultimate, g.CaseSlot{Case: g.THM}),
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			surface := render.Formative(tc.f)
			parsed, err := ParseFormative(surface)
			if err != nil {
				t.Fatalf("ParseFormative(%q): %v", surface, err)
			}
			if parsed.Stress != tc.f.Stress {
				t.Errorf("Stress: got %v, want %v (surface: %q)", parsed.Stress, tc.f.Stress, surface)
			}
		})
	}
}

// TestRoundTrip_Formative_Equality asserts the broader property that
// every Formative field survives render+parse. Stress is the
// load-bearing addition, but the other slots should also be stable.
func TestRoundTrip_Formative_Equality(t *testing.T) {
	cases := []struct {
		name string
		f    g.Formative
	}{
		{"minimal", g.MinimalFormative("ml")},
		{"non-default-Vv", func() g.Formative {
			f := g.MinimalFormative("ml")
			f.SlotII = g.SlotII{Stem: g.S2, Version: g.PRC}
			return f
		}()},
		{"non-default-Vr", func() g.Formative {
			f := g.MinimalFormative("ml")
			f.SlotIV = g.SlotIV{Function: g.DYN, Specification: g.OBJ, Context: g.EXS}
			return f
		}()},
		{"erg-case", func() g.Formative {
			f := g.MinimalFormative("ml")
			f.SlotIX = g.CaseSlot{Case: g.ERG}
			return f
		}()},
		{"verbal-ultimate", func() g.Formative {
			f := g.MinimalFormative("ml")
			f.Stress = g.Ultimate
			f.SlotIX = g.Assertive{Validation: g.INF}
			return f
		}()},
		{"framed-antepenult", func() g.Formative {
			f := g.MinimalFormative("ml")
			f.Stress = g.Antepenultimate
			return f
		}()},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			assertRoundTrip(t, tc.name, tc.f)
		})
	}
}

// TestRoundTrip_AllSlotII exercises every Stem × Version combination.
func TestRoundTrip_AllSlotII(t *testing.T) {
	for _, stem := range []g.Stem{g.S0, g.S1, g.S2, g.S3} {
		for _, ver := range []g.Version{g.PRC, g.CPT} {
			f := g.MinimalFormative("ml")
			f.SlotII = g.SlotII{Stem: stem, Version: ver}
			name := stem.String() + "/" + ver.String()
			t.Run(name, func(t *testing.T) {
				assertRoundTrip(t, name, f)
			})
		}
	}
}

// TestRoundTrip_AllStress exercises every stress value with a 4-syllable
// long-form so all four can fit without padding.
func TestRoundTrip_AllStress(t *testing.T) {
	cases := []struct {
		name   string
		stress g.Stress
		slotIX g.SlotIX
	}{
		{"penultimate", g.Penultimate, g.CaseSlot{Case: g.THM}},
		{"ultimate", g.Ultimate, g.Assertive{Validation: g.OBS}},
		{"antepenultimate", g.Antepenultimate, g.CaseSlot{Case: g.THM}},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.Stress = c.stress
		f.SlotIX = c.slotIX
		// Add a Slot VII affix to give the body more syllables, ensuring
		// every stress can land.
		f.SlotVII = []g.Affix{{Vowel: "a", Consonant: "r", Type: g.Type1Affix}}
		t.Run(c.name, func(t *testing.T) {
			assertRoundTrip(t, c.name, f)
		})
	}
}

// TestRoundTrip_SlotVII covers a few representative affix shapes.
func TestRoundTrip_SlotVII(t *testing.T) {
	cases := []struct {
		name    string
		affixes []g.Affix
	}{
		{"single-type1", []g.Affix{{Vowel: "a", Consonant: "r", Type: g.Type1Affix}}},
		{"single-type2", []g.Affix{{Vowel: "ai", Consonant: "t", Type: g.Type2Affix}}},
		{"two-affixes", []g.Affix{
			{Vowel: "ëu", Consonant: "ţř", Type: g.Type2Affix},
			{Vowel: "ai", Consonant: "t", Type: g.Type2Affix},
		}},
	}
	for _, c := range cases {
		f := g.MinimalFormative("m")
		f.SlotVII = c.affixes
		t.Run(c.name, func(t *testing.T) {
			assertRoundTrip(t, c.name, f)
		})
	}
}

// TestRoundTrip_SlotVIII_AllVariants exercises every SlotVIII sum-type
// variant (Valence, Phase, Effect, Level, Aspect). Pattern-1 Cn consonants
// share Mood/CaseScope semantics; the parser picks Mood under ultimate
// stress and CaseScope otherwise. Each variant is tested in the stress
// regime that makes its MS reading grammatical.
func TestRoundTrip_SlotVIII_AllVariants(t *testing.T) {
	scope := g.CaseScopeVal{CaseScope: g.CCA}
	cases := []struct {
		name string
		s8   g.SlotVIII
	}{
		{"valence", g.VnCnValence{Valence: g.PRL, MS: scope}},
		{"phase", g.VnCnPhase{Phase: g.PCT, MS: scope}},
		{"effect", g.VnCnEffect{Effect: g.BEN1, MS: scope}},
		{"level", g.VnCnLevel{Level: g.MIN, MS: scope}},
		{"aspect", g.VnCnAspect{Aspect: g.RTR, MS: scope}},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.SlotVIII = c.s8
		t.Run(c.name, func(t *testing.T) {
			assertRoundTrip(t, c.name, f)
		})
	}
}

// TestRoundTrip_SlotVIII_VerbalMood pairs ultimate stress (verbal
// formative) with MoodVal so Pattern-1 Cn is read as Mood, not
// CaseScope. Aspect uses Pattern-2 Cn so it round-trips either way.
func TestRoundTrip_SlotVIII_VerbalMood(t *testing.T) {
	mood := g.MoodVal{Mood: g.SUB}
	cases := []struct {
		name string
		s8   g.SlotVIII
	}{
		{"valence", g.VnCnValence{Valence: g.PRL, MS: mood}},
		{"phase", g.VnCnPhase{Phase: g.PCT, MS: mood}},
		{"effect", g.VnCnEffect{Effect: g.BEN1, MS: mood}},
		{"level", g.VnCnLevel{Level: g.MIN, MS: mood}},
		{"aspect", g.VnCnAspect{Aspect: g.RTR, MS: mood}},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.Stress = g.Ultimate
		f.SlotIX = g.Assertive{Validation: g.OBS}
		f.SlotVIII = c.s8
		t.Run(c.name, func(t *testing.T) {
			assertRoundTrip(t, c.name, f)
		})
	}
}

// TestRoundTrip_SlotIShortcuts exercises every Cc shortcut combination
// (W/Y × no-concat/Type1/Type2 = 6 forms) with each of the 4 Vv series.
func TestRoundTrip_SlotIShortcuts(t *testing.T) {
	wDefault := g.DefaultSlotVI
	wG := g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}
	wN := g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.N_, Extension: g.DEL, Essence: g.NRM}
	wGR := g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.RPV}
	yPRX := g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}
	yRPV := g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.RPV}
	yA := g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.A_, Extension: g.DEL, Essence: g.NRM}
	yBoth := g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.RPV}

	t1 := g.Type1
	t2 := g.Type2
	sw := g.ShortcutW
	sy := g.ShortcutY

	cases := []struct {
		name   string
		slotI  *g.ConcatenationStatus
		sc     *g.CcShortcut
		slotVI g.SlotVI
	}{
		{"w-series1", nil, &sw, wDefault},
		{"w-series2", nil, &sw, wG},
		{"w-series3", nil, &sw, wN},
		{"w-series4", nil, &sw, wGR},
		{"y-series1", nil, &sy, yPRX},
		{"y-series2", nil, &sy, yRPV},
		{"y-series3", nil, &sy, yA},
		{"y-series4", nil, &sy, yBoth},
		{"hl-type1+w", &t1, &sw, wDefault},
		{"hm-type1+y", &t1, &sy, yPRX},
		{"hr-type2+w", &t2, &sw, wDefault},
		{"hn-type2+y", &t2, &sy, yPRX},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.SlotI = c.slotI
		f.SlotIShortcut = c.sc
		f.SlotVI = c.slotVI
		t.Run(c.name, func(t *testing.T) {
			assertRoundTrip(t, c.name, f)
		})
	}
}

// TestRoundTrip_Grid_AllCases walks all 68 cases as the nominal V_C
// under penultimate stress.
func TestRoundTrip_Grid_AllCases(t *testing.T) {
	for _, c := range g.AllCases {
		f := g.MinimalFormative("ml")
		f.SlotIX = g.CaseSlot{Case: c}
		name := c.String()
		t.Run(name, func(t *testing.T) {
			assertRoundTrip(t, name, f)
		})
	}
}

// TestRoundTrip_Grid_AllVk walks every Vk variant under ultimate
// stress: 9 Assertive sub-cases (one per Validation) plus the 8
// dedicated non-ASR illocutions, for 17 cells total. Non-ASR variants
// have no Validation field per §3.9.3.2.
func TestRoundTrip_Grid_AllVk(t *testing.T) {
	for _, val := range g.AllValidations {
		f := g.MinimalFormative("ml")
		f.Stress = g.Ultimate
		f.SlotIX = g.Assertive{Validation: val}
		name := "ASR-" + val.String()
		t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
	}
	// Skip index 0 (Assertive); the rest are leaf illocutions.
	for _, v := range g.AllIllocutionVariants[1:] {
		f := g.MinimalFormative("ml")
		f.Stress = g.Ultimate
		f.SlotIX = v
		name := v.Tag()
		t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
	}
}

// TestRoundTrip_Grid_MoodVerbal exhaustively walks every Mood × every
// Slot VIII content under ultimate stress (verbal formative). Pattern-1
// Cn is read as Mood under ultimate stress; Pattern-2 Cn (Aspect) is
// always Mood. The total cell count is 6 moods × (9+9+9+9+36) = 432.
func TestRoundTrip_Grid_MoodVerbal(t *testing.T) {
	mkVerb := func(s8 g.SlotVIII, mood g.Mood) g.Formative {
		f := g.MinimalFormative("ml")
		f.Stress = g.Ultimate
		f.SlotIX = g.Assertive{Validation: g.OBS}
		f.SlotVIII = withMood(s8, mood)
		return f
	}
	for _, m := range g.AllMoods {
		for _, v := range g.AllValences {
			name := "Valence-" + v.String() + "-" + m.String()
			f := mkVerb(g.VnCnValence{Valence: v}, m)
			t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
		}
		for _, p := range g.AllPhases {
			name := "Phase-" + p.String() + "-" + m.String()
			f := mkVerb(g.VnCnPhase{Phase: p}, m)
			t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
		}
		for _, e := range g.AllEffects {
			name := "Effect-" + e.String() + "-" + m.String()
			f := mkVerb(g.VnCnEffect{Effect: e}, m)
			t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
		}
		for _, l := range g.AllLevels {
			name := "Level-" + l.String() + "-" + m.String()
			f := mkVerb(g.VnCnLevel{Level: l}, m)
			t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
		}
		for _, a := range g.AllAspects {
			name := "Aspect-" + a.String() + "-" + m.String()
			f := mkVerb(g.VnCnAspect{Aspect: a}, m)
			t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
		}
	}
}

// TestRoundTrip_Grid_CaseScopeNominal walks every CaseScope × every
// Slot VIII content under penultimate stress (nominal formative).
func TestRoundTrip_Grid_CaseScopeNominal(t *testing.T) {
	mkNoun := func(s8 g.SlotVIII, cs g.CaseScope) g.Formative {
		f := g.MinimalFormative("ml")
		f.SlotVIII = withCaseScope(s8, cs)
		return f
	}
	for _, cs := range g.AllCaseScopes {
		for _, v := range g.AllValences {
			name := "Valence-" + v.String() + "-" + cs.String()
			f := mkNoun(g.VnCnValence{Valence: v}, cs)
			t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
		}
		for _, p := range g.AllPhases {
			name := "Phase-" + p.String() + "-" + cs.String()
			f := mkNoun(g.VnCnPhase{Phase: p}, cs)
			t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
		}
		for _, e := range g.AllEffects {
			name := "Effect-" + e.String() + "-" + cs.String()
			f := mkNoun(g.VnCnEffect{Effect: e}, cs)
			t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
		}
		for _, l := range g.AllLevels {
			name := "Level-" + l.String() + "-" + cs.String()
			f := mkNoun(g.VnCnLevel{Level: l}, cs)
			t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
		}
		for _, a := range g.AllAspects {
			name := "Aspect-" + a.String() + "-" + cs.String()
			f := mkNoun(g.VnCnAspect{Aspect: a}, cs)
			t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
		}
	}
}

func withMood(s8 g.SlotVIII, m g.Mood) g.SlotVIII {
	ms := g.MoodVal{Mood: m}
	switch v := s8.(type) {
	case g.VnCnValence:
		v.MS = ms
		return v
	case g.VnCnPhase:
		v.MS = ms
		return v
	case g.VnCnEffect:
		v.MS = ms
		return v
	case g.VnCnLevel:
		v.MS = ms
		return v
	case g.VnCnAspect:
		v.MS = ms
		return v
	}
	return s8
}

func withCaseScope(s8 g.SlotVIII, cs g.CaseScope) g.SlotVIII {
	ms := g.CaseScopeVal{CaseScope: cs}
	switch v := s8.(type) {
	case g.VnCnValence:
		v.MS = ms
		return v
	case g.VnCnPhase:
		v.MS = ms
		return v
	case g.VnCnEffect:
		v.MS = ms
		return v
	case g.VnCnLevel:
		v.MS = ms
		return v
	case g.VnCnAspect:
		v.MS = ms
		return v
	}
	return s8
}

// TestRoundTrip_SlotV exercises Slot V affixes (reversed Cs-Vx form
// with geminated Ca). One affix, two affixes (with §3.5.1 Vv glottal-
// stop signal), and combinations with Slot VII.
func TestRoundTrip_SlotV(t *testing.T) {
	cases := []struct {
		name    string
		slotV   []g.Affix
		slotVII []g.Affix
	}{
		{"one-affix", []g.Affix{
			{Consonant: "r", Vowel: "a", Type: g.Type1Affix},
		}, nil},
		{"two-affixes", []g.Affix{
			{Consonant: "r", Vowel: "a", Type: g.Type1Affix},
			{Consonant: "t", Vowel: "a", Type: g.Type1Affix},
		}, nil},
		{"one-V-one-VII", []g.Affix{
			{Consonant: "r", Vowel: "a", Type: g.Type1Affix},
		}, []g.Affix{
			{Consonant: "t", Vowel: "a", Type: g.Type1Affix},
		}},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.SlotV = c.slotV
		f.SlotVII = c.slotVII
		t.Run(c.name, func(t *testing.T) {
			assertRoundTrip(t, c.name, f)
		})
	}
}

// TestRoundTrip_Concatenation covers Slot I Type1/Type2 without shortcut.
func TestRoundTrip_Concatenation(t *testing.T) {
	t1 := g.Type1
	t2 := g.Type2
	cases := []struct {
		name string
		c    *g.ConcatenationStatus
	}{
		{"type1", &t1},
		{"type2", &t2},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.SlotI = c.c
		t.Run(c.name, func(t *testing.T) {
			assertRoundTrip(t, c.name, f)
		})
	}
}
