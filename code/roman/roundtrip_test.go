package roman

import (
	"reflect"
	"testing"

	"github.com/christian-oudard/ithkuil/allomorph"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
)

// assertRoundTrip renders f, parses the result, and asserts that the
// parsed Formative matches f field-by-field.
func assertRoundTrip(t *testing.T, name string, f g.Formative) {
	t.Helper()
	rom := Formative(f)
	if err := phonology.CheckText(rom); err != nil && !allomorph.UnresolvedCa(rom) {
		t.Errorf("%s: render produced %q, which our own validator rejects: %v",
			name, rom, err)
	}
	parsed, err := ParseFormative(rom)
	if err != nil {
		t.Errorf("%s: Formative(%q): %v", name, rom, err)
		return
	}
	if !reflect.DeepEqual(parsed.Root, f.Root) {
		t.Errorf("%s: Root: got %v, want %v (romanization %q)", name, parsed.Root, f.Root, rom)
	}
	if parsed.SlotVI != f.SlotVI {
		t.Errorf("%s: SlotVI: got %v, want %v (romanization %q)", name, parsed.SlotVI, f.SlotVI, rom)
	}
	if !reflect.DeepEqual(parsed.SlotV, f.SlotV) {
		t.Errorf("%s: SlotV: got %v, want %v (romanization %q)", name, parsed.SlotV, f.SlotV, rom)
	}
	if !reflect.DeepEqual(parsed.SlotVII, f.SlotVII) {
		t.Errorf("%s: SlotVII: got %v, want %v (romanization %q)", name, parsed.SlotVII, f.SlotVII, rom)
	}
	// MNO Valence at the FAC Mood/Case-Scope says only what an absent
	// Slot VIII says, so both arms fold it away and what comes back is
	// the absent form. That is the round trip working, not failing.
	wantSlotVIII := f.SlotVIII
	if g.SlotVIIIIsDefault(wantSlotVIII) {
		wantSlotVIII = nil
	}
	if !reflect.DeepEqual(parsed.SlotVIII, wantSlotVIII) {
		t.Errorf("%s: SlotVIII: got %v, want %v (romanization %q)",
			name, parsed.SlotVIII, wantSlotVIII, rom)
	}
	if !reflect.DeepEqual(parsed.Final, f.Final) {
		t.Errorf("%s: Final: got %v, want %v (romanization %q)", name, parsed.Final, f.Final, rom)
	}
	if parsed.Concat != f.Concat {
		t.Errorf("%s: Concat: got %v, want %v (romanization %q)", name, parsed.Concat, f.Concat, rom)
	}
}

// TestRoundTrip_Final asserts parse(render(F)).Final == F.Final for
// every reasonable Final variant. This is the guard rail that catches
// accidental category shifts where the rendered romanization's default
// stress disagrees with the Formative's intended category.
func TestRoundTrip_Final(t *testing.T) {
	mkFormative := func(final g.Final) g.Formative {
		f := g.MinimalFormative("ml")
		f.Final = final
		return f
	}
	cases := []struct {
		name string
		f    g.Formative
	}{
		{
			"nominal-thm",
			mkFormative(g.UnframedNominal{Case: g.THM}),
		},
		{
			"nominal-erg",
			mkFormative(g.UnframedNominal{Case: g.ERG}),
		},
		{
			"verbal-asr-inf",
			mkFormative(g.UnframedVerbal{Vk: g.Assertive{Validation: g.INF}}),
		},
		{
			"verbal-dir",
			mkFormative(g.UnframedVerbal{Vk: g.Directive{}}),
		},
		{
			"framed-thm",
			mkFormative(g.FramedVerbal{Case: g.THM}),
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			rom := Formative(tc.f)
			parsed, err := ParseFormative(rom)
			if err != nil {
				t.Fatalf("Formative(%q): %v", rom, err)
			}
			if !reflect.DeepEqual(parsed.Final, tc.f.Final) {
				t.Errorf("Final: got %v, want %v (romanization: %q)", parsed.Final, tc.f.Final, rom)
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
			cr := f.Root.(g.CrRoot)
			cr.Stem = g.S2
			f.Root = cr
			return f
		}()},
		{"non-default-Vr", func() g.Formative {
			f := g.MinimalFormative("ml")
			cr := f.Root.(g.CrRoot)
			cr.SlotIV = g.SlotIV{Function: g.DYN, Specification: g.OBJ, Context: g.EXS}
			f.Root = cr
			return f
		}()},
		{"erg-case", func() g.Formative {
			f := g.MinimalFormative("ml")
			f.Final = g.UnframedNominal{Case: g.ERG}
			return f
		}()},
		{"verbal-ultimate", func() g.Formative {
			f := g.MinimalFormative("ml")
			f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.INF}}
			return f
		}()},
		{"framed-antepenult", func() g.Formative {
			f := g.MinimalFormative("ml")
			f.Final = g.FramedVerbal{Case: g.THM}
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
			cr := f.Root.(g.CrRoot)
			cr.Stem = stem
			cr.Version = ver
			f.Root = cr
			name := stem.String() + "/" + ver.String()
			t.Run(name, func(t *testing.T) {
				assertRoundTrip(t, name, f)
			})
		}
	}
}

// TestRoundTrip_AllFinal exercises every Final variant with a multi-
// syllable body so every diacritic position is reachable.
func TestRoundTrip_AllFinal(t *testing.T) {
	cases := []struct {
		name  string
		final g.Final
	}{
		{"nominal", g.UnframedNominal{Case: g.THM}},
		{"verbal", g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}},
		{"framed", g.FramedVerbal{Case: g.THM}},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.Final = c.final
		// Add a Slot VII affix to give the body more syllables, ensuring
		// every stress can land.
		f.SlotVII = []g.Affix{{Type: g.Type1Affix, Degree: 1, Consonant: "r"}}
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
		{"single-type1", []g.Affix{{Type: g.Type1Affix, Degree: 1, Consonant: "r"}}},
		{"single-type2", []g.Affix{{Type: g.Type2Affix, Degree: 1, Consonant: "t"}}},
		{"two-affixes", []g.Affix{
			{Type: g.Type2Affix, Degree: 5, Consonant: "ţř"},
			{Type: g.Type2Affix, Degree: 1, Consonant: "t"},
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
	scope := g.SUB
	cases := []struct {
		name string
		s8   g.SlotVIII
	}{
		{"valence", g.VnCnValence{Valence: g.PRL, MoodScope: scope}},
		{"phase", g.VnCnPhase{Phase: g.PCT, MoodScope: scope}},
		{"effect", g.VnCnEffect{Effect: g.BEN1, MoodScope: scope}},
		{"level", g.VnCnLevel{Level: g.MIN, MoodScope: scope}},
		{"aspect", g.VnCnAspect{Aspect: g.RTR, MoodScope: scope}},
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
	mood := g.SUB
	cases := []struct {
		name string
		s8   g.SlotVIII
	}{
		{"valence", g.VnCnValence{Valence: g.PRL, MoodScope: mood}},
		{"phase", g.VnCnPhase{Phase: g.PCT, MoodScope: mood}},
		{"effect", g.VnCnEffect{Effect: g.BEN1, MoodScope: mood}},
		{"level", g.VnCnLevel{Level: g.MIN, MoodScope: mood}},
		{"aspect", g.VnCnAspect{Aspect: g.RTR, MoodScope: mood}},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
		f.SlotVIII = c.s8
		t.Run(c.name, func(t *testing.T) {
			assertRoundTrip(t, c.name, f)
		})
	}
}

// TestRoundTrip_ShortcutEncodableSlotVI walks every SlotVI value that
// the renderer can encode via Cc-Vv shortcut form. Each combination
// renders to a shortcut romanization and parses back to the same SlotVI.
// The shortcut/W vs shortcut/Y distinction is purely a rendering
// choice — the grammar carries only the SlotVI.
func TestRoundTrip_ShortcutEncodableSlotVI(t *testing.T) {
	wDefault := g.DefaultSlotVI
	wG := g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}
	wN := g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.N_, Extension: g.DEL, Essence: g.NRM}
	wGR := g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.RPV}
	yPRX := g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}
	yRPV := g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.RPV}
	yA := g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.A_, Extension: g.DEL, Essence: g.NRM}
	yBoth := g.SlotVI{Configuration: g.UPX, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.RPV}

	cases := []struct {
		name   string
		concat g.ConcatenationStatus
		slotVI g.SlotVI
	}{
		{"w-series1", g.ConcatNone, wDefault},
		{"w-series2", g.ConcatNone, wG},
		{"w-series3", g.ConcatNone, wN},
		{"w-series4", g.ConcatNone, wGR},
		{"y-series1", g.ConcatNone, yPRX},
		{"y-series2", g.ConcatNone, yRPV},
		{"y-series3", g.ConcatNone, yA},
		{"y-series4", g.ConcatNone, yBoth},
		{"hl-type1+w", g.Type1, wDefault},
		{"hm-type1+y", g.Type1, yPRX},
		{"hr-type2+w", g.Type2, wDefault},
		{"hn-type2+y", g.Type2, yPRX},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.Concat = c.concat
		f.SlotVI = c.slotVI
		t.Run(c.name, func(t *testing.T) {
			assertRoundTrip(t, c.name, f)
			// Also exercise the shortcut romanization explicitly.
			rom := Formative(f)
			parsed, err := ParseFormative(rom)
			if err != nil {
				t.Fatalf("Formative(%q): %v", rom, err)
			}
			if parsed.SlotVI != c.slotVI {
				t.Errorf("shortcut SlotVI: got %v, want %v (romanization %q)", parsed.SlotVI, c.slotVI, rom)
			}
		})
	}
}

// TestRoundTrip_Grid_AllSlotVI exercises every Configuration ×
// Affiliation × Perspective × Extension × Essence combination — the
// full 20·4·4·6·2 = 3840-cell space of the Ca complex. Catches any
// allomorph table entry that doesn't round-trip.
func TestRoundTrip_Grid_AllSlotVI(t *testing.T) {
	for _, cfg := range g.AllConfigurations {
		for _, aff := range g.AllAffiliations {
			for _, per := range g.AllPerspectives {
				for _, ext := range g.AllExtensions {
					for _, ess := range g.AllEssences {
						s6 := g.SlotVI{
							Configuration: cfg,
							Affiliation:   aff,
							Perspective:   per,
							Extension:     ext,
							Essence:       ess,
						}
						f := g.MinimalFormative("ml")
						f.SlotVI = s6
						name := cfg.String() + "-" + aff.String() + "-" +
							per.String() + "-" + ext.String() + "-" + ess.String()
						t.Run(name, func(t *testing.T) {
							assertRoundTrip(t, name, f)
						})
					}
				}
			}
		}
	}
}

// TestRoundTrip_Grid_AllCases walks all 68 cases as the nominal V_C
// under penultimate stress.
func TestRoundTrip_Grid_AllCases(t *testing.T) {
	for _, c := range g.AllCases {
		f := g.MinimalFormative("ml")
		f.Final = g.UnframedNominal{Case: c}
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
		f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: val}}
		name := "ASR-" + val.String()
		t.Run(name, func(t *testing.T) { assertRoundTrip(t, name, f) })
	}
	// Skip index 0 (Assertive); the rest are leaf illocutions.
	for _, v := range g.AllVk[1:] {
		f := g.MinimalFormative("ml")
		f.Final = g.UnframedVerbal{Vk: v}
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
		f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
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
	switch v := s8.(type) {
	case g.VnCnValence:
		v.MoodScope = m
		return v
	case g.VnCnPhase:
		v.MoodScope = m
		return v
	case g.VnCnEffect:
		v.MoodScope = m
		return v
	case g.VnCnLevel:
		v.MoodScope = m
		return v
	case g.VnCnAspect:
		v.MoodScope = m
		return v
	}
	return s8
}

func withCaseScope(s8 g.SlotVIII, cs g.CaseScope) g.SlotVIII {
	return withMood(s8, g.CaseScopeToMood(cs))
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
			{Type: g.Type1Affix, Degree: 1, Consonant: "r"},
		}, nil},
		{"two-affixes", []g.Affix{
			{Type: g.Type1Affix, Degree: 1, Consonant: "r"},
			{Type: g.Type1Affix, Degree: 1, Consonant: "t"},
		}, nil},
		{"one-V-one-VII", []g.Affix{
			{Type: g.Type1Affix, Degree: 1, Consonant: "r"},
		}, []g.Affix{
			{Type: g.Type1Affix, Degree: 1, Consonant: "t"},
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
	cases := []struct {
		name string
		c    g.ConcatenationStatus
	}{
		{"type1", g.Type1},
		{"type2", g.Type2},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.Concat = c.c
		t.Run(c.name, func(t *testing.T) {
			assertRoundTrip(t, c.name, f)
		})
	}
}
