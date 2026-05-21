package slots

import (
	"reflect"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
)

// TestRoundTrip_Grid_AllVk walks every Vk variant — the 9 Validations
// under Assertive plus the 8 dedicated non-ASR illocutions — and
// checks slots.FromGrammar/ToGrammar round-trips them. Exercises
// vkVowel's 9 branches that the corpus tests miss.
func TestRoundTrip_Grid_AllVk(t *testing.T) {
	var vks []g.Vk
	for _, val := range g.AllValidations {
		vks = append(vks, g.Assertive{Validation: val})
	}
	for _, v := range g.AllVk[1:] { // skip Assertive (index 0)
		vks = append(vks, v)
	}
	for _, vk := range vks {
		f := g.MinimalFormative("ml")
		f.Final = g.UnframedVerbal{Vk: vk}
		name := vk.Tag()
		if asr, ok := vk.(g.Assertive); ok {
			name = "ASR/" + asr.Validation.String()
		}
		t.Run(name, func(t *testing.T) {
			l := FromGrammar(f, Options{})
			got, err := ToGrammar(l)
			if err != nil {
				t.Fatalf("ToGrammar: %v", err)
			}
			if !reflect.DeepEqual(got.Final, f.Final) {
				t.Errorf("Vk drift:\n  got  %+v\n  want %+v", got.Final, f.Final)
			}
		})
	}
}

// TestRoundTrip_Grid_CsRoot walks Cs-root formatives across all
// Function × Version × Context × a few Degree values. Exercises
// csRootVv and csRootVr's full table.
func TestRoundTrip_Grid_CsRoot(t *testing.T) {
	for _, fn := range []g.Function{g.STA, g.DYN} {
		for _, ver := range []g.Version{g.PRC, g.CPT} {
			for _, ctx := range []g.Context{g.EXS, g.FNC, g.RPS, g.AMG} {
				for _, deg := range []int{0, 1, 5, 9} {
					f := g.MinimalFormative("ml")
					f.Root = g.CsRoot{
						Cs: "r", Degree: deg, Version: ver,
						Function: fn, Context: ctx,
					}
					name := fn.String() + "-" + ver.String() + "-" + ctx.String() + "-d" + string(rune('0'+deg))
					t.Run(name, func(t *testing.T) {
						l := FromGrammar(f, Options{})
						got, err := ToGrammar(l)
						if err != nil {
							t.Fatalf("ToGrammar: %v", err)
						}
						if !reflect.DeepEqual(got.Root, f.Root) {
							t.Errorf("CsRoot drift:\n  got  %+v\n  want %+v", got.Root, f.Root)
						}
					})
				}
			}
		}
	}
}

// TestRoundTrip_Grid_RefRoot covers RefRoot across Version × SlotIV
// (Function, Spec, Context). Exercises refRootVv's PRC/CPT branches.
func TestRoundTrip_Grid_RefRoot(t *testing.T) {
	for _, ver := range []g.Version{g.PRC, g.CPT} {
		for _, fn := range []g.Function{g.STA, g.DYN} {
			for _, sp := range []g.Specification{g.BSC, g.CTE, g.CSV, g.OBJ} {
				f := g.MinimalFormative("ml")
				f.Root = g.RefRoot{
					C1: "l", Version: ver,
					SlotIV: g.SlotIV{Function: fn, Specification: sp, Context: g.EXS},
				}
				name := ver.String() + "-" + fn.String() + "-" + sp.String()
				t.Run(name, func(t *testing.T) {
					l := FromGrammar(f, Options{})
					got, err := ToGrammar(l)
					if err != nil {
						t.Fatalf("ToGrammar: %v", err)
					}
					if !reflect.DeepEqual(got.Root, f.Root) {
						t.Errorf("RefRoot drift:\n  got  %+v\n  want %+v", got.Root, f.Root)
					}
				})
			}
		}
	}
}

// TestRoundTrip_Grid_ConcatShortcut walks every (Concat × Shortcut)
// combination on shortcut-eligible formatives, hitting ccFromGrammar's
// hl/hm/hr/hn branches that the existing tests miss.
func TestRoundTrip_Grid_ConcatShortcut(t *testing.T) {
	t1, t2 := g.Type1, g.Type2
	concats := []struct {
		name   string
		concat *g.ConcatenationStatus
	}{
		{"plain", nil},
		{"type1", &t1},
		{"type2", &t2},
	}
	// Pick a SlotVI value for each shortcut series.
	series := []g.SlotVI{
		g.DefaultSlotVI, // w-series-1
		{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM},  // w-series-2
		{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM},  // y-series-1
		{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.RPV},  // y-series-2
	}
	for _, c := range concats {
		for i, s := range series {
			f := g.MinimalFormative("ml")
			f.Concat = c.concat
			f.SlotVI = s
			name := c.name + "-series" + string(rune('1'+i))
			t.Run(name, func(t *testing.T) {
				l := FromGrammar(f, Options{Shortcut: true})
				got, err := ToGrammar(l)
				if err != nil {
					t.Fatalf("ToGrammar (shortcut): %v", err)
				}
				if !reflect.DeepEqual(got, f) {
					t.Errorf("shortcut+concat drift:\n  got  %+v\n  want %+v", got, f)
				}
			})
		}
	}
}

func TestShortcutVariant_Selection(t *testing.T) {
	// shortcutVariant resolves a SlotVI to W or Y or None.
	if v := shortcutVariant(g.DefaultSlotVI); v != parse.ShortcutW {
		t.Errorf("default SlotVI: shortcutVariant = %v, want W", v)
	}
	yShortcut := g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}
	if v := shortcutVariant(yShortcut); v != parse.ShortcutY {
		t.Errorf("PRX SlotVI: shortcutVariant = %v, want Y", v)
	}
	// A non-shortcut-encodable SlotVI returns None.
	non := g.SlotVI{Configuration: g.MSS, Affiliation: g.CSL, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}
	if v := shortcutVariant(non); v != parse.ShortcutNone {
		t.Errorf("non-encodable SlotVI: shortcutVariant = %v, want None", v)
	}
}

func TestCanUseShortcut(t *testing.T) {
	// Minimal: shortcut-eligible.
	f := g.MinimalFormative("ml")
	if !canUseShortcut(f) {
		t.Error("minimal formative should be shortcut-eligible")
	}
	// Non-default SlotIV → not eligible.
	cr := f.Root.(g.CrRoot)
	cr.SlotIV = g.SlotIV{Function: g.DYN, Specification: g.BSC, Context: g.EXS}
	f.Root = cr
	if canUseShortcut(f) {
		t.Error("DYN SlotIV: shouldn't be shortcut-eligible")
	}
	// CsRoot → not eligible.
	f = g.MinimalFormative("ml")
	f.Root = g.CsRoot{Cs: "r", Degree: 5, Version: g.PRC, Function: g.STA, Context: g.EXS}
	if canUseShortcut(f) {
		t.Error("CsRoot: shouldn't be shortcut-eligible")
	}
	// Slot V populated → not eligible.
	f = g.MinimalFormative("ml")
	f.SlotV = []g.Affix{{Type: g.Type1Affix, Degree: 1, Consonant: "r"}}
	if canUseShortcut(f) {
		t.Error("Slot V filled: shouldn't be shortcut-eligible")
	}
}

func TestRestoreMovedGlottal(t *testing.T) {
	cases := []struct {
		in, want string
	}{
		// Single vowel → reduplicated around the glottal.
		{"a", "a'a"},
		{"ö", "ö'ö"},
		// Multi-rune diphthong/disyllabic → glottal between first and rest.
		{"ai", "a'i"},
		{"uä", "u'ä"},
		// Empty input round-trips.
		{"", ""},
	}
	for _, c := range cases {
		got := restoreMovedGlottal(c.in)
		if got != c.want {
			t.Errorf("restoreMovedGlottal(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

// TestParse_SlotV exercises parseFromCa's Slot V branch: render a
// Slot-V formative and re-parse the surface, asserting the affixes
// come back. This is the path the existing test corpus barely
// touches because most natural strings don't have geminated Ca.
func TestParse_SlotV(t *testing.T) {
	f := g.MinimalFormative("ml")
	cr := f.Root.(g.CrRoot)
	cr.Stem = g.S2 // pin Vv so it doesn't elide and leave too few syllables
	f.Root = cr
	f.SlotV = []g.Affix{
		{Type: g.Type1Affix, Degree: 5, Consonant: "r"},
		{Type: g.Type1Affix, Degree: 5, Consonant: "r"},
	}
	l := FromGrammar(f, Options{})
	surface := Render(l)
	got, err := Parse(surface)
	if err != nil {
		t.Fatalf("Parse(%q): %v", surface, err)
	}
	if len(got.SlotV) != 2 {
		t.Errorf("Parse(%q): SlotV count = %d, want 2", surface, len(got.SlotV))
	}
}

// TestParse_VowelStartingErrors exercises parseVowelInitial's error
// paths (special Vv with too few conjuncts, etc.).
func TestParse_Errors(t *testing.T) {
	cases := []string{
		"",                // empty word
		"a",               // too short
		"aml",             // too short to be a complete formative
		"ç" + "amláláu",   // double-stressed body
	}
	for _, w := range cases {
		if _, err := Parse(w); err == nil {
			t.Errorf("Parse(%q) succeeded; expected error", w)
		}
	}
}

func TestMaybeMoveCnToCa_Conditions(t *testing.T) {
	// Use a non-default Slot II so Vv doesn't elide; then the body
	// has enough vowels to drop one for the CnInCa shortcut.
	base := func() g.Formative {
		f := g.MinimalFormative("ml")
		cr := f.Root.(g.CrRoot)
		cr.Stem = g.S2 // forces Vv to stay
		f.Root = cr
		return f
	}
	// Shortcut applies: Vn=MNO, Cn=Pattern-1 non-FAC, Ca=default-l.
	f := base()
	f.SlotVIII = g.VnCnValence{Valence: g.MNO, MoodScope: g.SUB}
	l := FromGrammar(f, Options{})
	if !l.CnInCa {
		t.Error("MNO + SUB + default Ca: CnInCa should be true")
	}
	// Not applied: non-MNO Valence.
	f = base()
	f.SlotVIII = g.VnCnValence{Valence: g.PRL, MoodScope: g.SUB}
	l = FromGrammar(f, Options{})
	if l.CnInCa {
		t.Error("PRL + SUB: CnInCa should be false")
	}
	// Not applied: FAC Mood (the Cn would elide instead).
	f = base()
	f.SlotVIII = g.VnCnValence{Valence: g.MNO, MoodScope: g.FAC}
	l = FromGrammar(f, Options{})
	if l.CnInCa {
		t.Error("MNO + FAC: CnInCa should be false (Cn would elide)")
	}
	// Not applied: SlotVI not default.
	f = base()
	f.SlotVIII = g.VnCnValence{Valence: g.MNO, MoodScope: g.SUB}
	f.SlotVI = g.SlotVI{Configuration: g.UNI, Affiliation: g.ASO, Perspective: g.M_, Extension: g.DEL, Essence: g.NRM}
	l = FromGrammar(f, Options{})
	if l.CnInCa {
		t.Error("non-default Ca: CnInCa should be false")
	}
}
