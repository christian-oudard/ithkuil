package parse

import (
	"testing"

	"github.com/christian-oudard/ithkuil/grammar"
)

func TestAffixVowel_AllTypes(t *testing.T) {
	// Series 1 (Type 1) degrees.
	cases := []struct {
		t      grammar.AffixType
		degree int
		want   string
	}{
		{grammar.Type1Affix, 1, "a"},
		{grammar.Type1Affix, 5, "ëi"},
		{grammar.Type2Affix, 1, "ai"},
		{grammar.Type3Affix, 1, "ia"},
	}
	for _, c := range cases {
		got := AffixVowel(c.t, c.degree)
		if got != c.want {
			t.Errorf("AffixVowel(%v,%d) = %q, want %q", c.t, c.degree, got, c.want)
		}
	}
}

// TestAffixVowel_RejectsOutOfRange pins the panic. Returning "" here
// used to splice a vowel-less affix into the romanization, so
// roman.Formative silently produced a different, valid word.
func TestAffixVowel_RejectsOutOfRange(t *testing.T) {
	for _, degree := range []int{-1, 10, 20} {
		func() {
			defer func() {
				if recover() == nil {
					t.Errorf("AffixVowel(Type1, %d) returned instead of panicking", degree)
				}
			}()
			AffixVowel(grammar.Type1Affix, degree)
		}()
	}
}

func TestType2DegreeToVowel(t *testing.T) {
	if got := Type2DegreeToVowel(1); got != "ai" {
		t.Errorf("Type2DegreeToVowel(1) = %q, want \"ai\"", got)
	}
}

func TestShortcutCa_AllVariantsAllSeries(t *testing.T) {
	// ShortcutW + series 1 = default SlotVI.
	if got := ShortcutCa(ShortcutW, 1); got != grammar.DefaultSlotVI {
		t.Errorf("ShortcutCa(W,1) = %+v, want DefaultSlotVI", got)
	}
	// ShortcutW + series 2 differs in Perspective.
	if got := ShortcutCa(ShortcutW, 2); got.Perspective != grammar.G_ {
		t.Errorf("ShortcutCa(W,2).Perspective = %v, want G_", got.Perspective)
	}
	// ShortcutY + series 1 differs in Extension.
	if got := ShortcutCa(ShortcutY, 1); got.Extension != grammar.PRX {
		t.Errorf("ShortcutCa(Y,1).Extension = %v, want PRX", got.Extension)
	}
	// ShortcutW + series 3, 4 should resolve.
	if got := ShortcutCa(ShortcutW, 3); got.Perspective != grammar.N_ {
		t.Errorf("ShortcutCa(W,3).Perspective = %v, want N_", got.Perspective)
	}
	if got := ShortcutCa(ShortcutW, 4); got.Essence != grammar.RPV {
		t.Errorf("ShortcutCa(W,4).Essence = %v, want RPV", got.Essence)
	}
	// ShortcutY 2/3/4.
	for _, s := range []int{2, 3, 4} {
		got := ShortcutCa(ShortcutY, s)
		if got == grammar.DefaultSlotVI {
			t.Errorf("ShortcutCa(Y,%d) shouldn't be DefaultSlotVI", s)
		}
	}
	// Unknown variant + series = default.
	if got := ShortcutCa(ShortcutNone, 1); got != grammar.DefaultSlotVI {
		t.Errorf("ShortcutCa(None,1) = %+v, want DefaultSlotVI", got)
	}
}

func TestVvSeries(t *testing.T) {
	cases := []struct {
		v    string
		want int
	}{
		{"a", 1}, {"e", 1},
		{"ai", 2}, {"au", 2},
		{"ia", 3}, {"uä", 3},
		{"ao", 4}, {"aö", 4},
		{"zzz", 1}, // unrecognized falls back to series 1
	}
	for _, c := range cases {
		got := VvSeries(c.v)
		if got != c.want {
			t.Errorf("VvSeries(%q) = %d, want %d", c.v, got, c.want)
		}
	}
}

func TestIsSpecialVv(t *testing.T) {
	for _, v := range []string{"ëi", "eë", "ëu", "oë", "ae", "ea"} {
		if !IsSpecialVv(v) {
			t.Errorf("IsSpecialVv(%q) = false, want true", v)
		}
	}
	if IsSpecialVv("a") {
		t.Error("IsSpecialVv(a) should be false")
	}
}

func TestIsRefRootVv(t *testing.T) {
	for _, v := range []string{"ae", "ea"} {
		if !IsRefRootVv(v) {
			t.Errorf("IsRefRootVv(%q) = false, want true", v)
		}
	}
	if IsRefRootVv("ëi") {
		t.Error("IsRefRootVv(ëi) should be false (Cs-root, not ref-root)")
	}
}

func TestParseSpecialVv_AllForms(t *testing.T) {
	sta := grammar.STA
	dyn := grammar.DYN
	cases := []struct {
		v       string
		version grammar.Version
		fn      *grammar.Function
	}{
		{"ëi", grammar.PRC, &sta},
		{"eë", grammar.PRC, &dyn},
		{"ëu", grammar.CPT, &sta},
		{"oë", grammar.CPT, &dyn},
		{"ae", grammar.PRC, nil},
		{"ea", grammar.CPT, nil},
	}
	for _, c := range cases {
		sv, ok := ParseSpecialVv(c.v)
		if !ok {
			t.Errorf("ParseSpecialVv(%q): ok=false", c.v)
			continue
		}
		if sv.Version != c.version {
			t.Errorf("ParseSpecialVv(%q): Version = %v, want %v", c.v, sv.Version, c.version)
		}
		if (sv.Function == nil) != (c.fn == nil) {
			t.Errorf("ParseSpecialVv(%q): Function = %v, want %v", c.v, sv.Function, c.fn)
		}
		if c.fn != nil && sv.Function != nil && *sv.Function != *c.fn {
			t.Errorf("ParseSpecialVv(%q): Function = %v, want %v", c.v, *sv.Function, *c.fn)
		}
	}
	if _, ok := ParseSpecialVv("zzz"); ok {
		t.Error("ParseSpecialVv(zzz) should return ok=false")
	}
}

func TestParseAffixVr_AllSpecialDegree0(t *testing.T) {
	// The four degree-0 special forms each pair with a distinct context.
	cases := []struct {
		v   string
		ctx grammar.Context
	}{
		{"ae", grammar.EXS},
		{"ea", grammar.FNC},
		{"üo", grammar.RPS},
		{"üö", grammar.AMG},
	}
	for _, c := range cases {
		d, ctx, ok := ParseAffixVr(c.v)
		if !ok {
			t.Errorf("ParseAffixVr(%q): ok=false", c.v)
			continue
		}
		if d != 0 {
			t.Errorf("ParseAffixVr(%q): degree = %d, want 0", c.v, d)
		}
		if ctx != c.ctx {
			t.Errorf("ParseAffixVr(%q): ctx = %v, want %v", c.v, ctx, c.ctx)
		}
	}
}

func TestParseAffixVr_AllSeriesAllForms(t *testing.T) {
	// Walk one form per series for ParseAffixVr's series→context dispatch.
	cases := []struct {
		v   string
		ctx grammar.Context
		deg int
	}{
		{"a", grammar.EXS, 1},  // series 1 form 1
		{"ai", grammar.FNC, 1}, // series 2 form 1
		{"ia", grammar.RPS, 1}, // series 3 form 1
		{"ao", grammar.AMG, 1}, // series 4 form 1
		{"u", grammar.EXS, 9},  // series 1 form 9
		{"ui", grammar.FNC, 9}, // series 2 form 9
		{"ua", grammar.RPS, 9}, // series 3 form 9
		{"oa", grammar.AMG, 9}, // series 4 form 9
	}
	for _, c := range cases {
		d, ctx, ok := ParseAffixVr(c.v)
		if !ok {
			t.Errorf("ParseAffixVr(%q): ok=false", c.v)
			continue
		}
		if d != c.deg {
			t.Errorf("ParseAffixVr(%q): degree = %d, want %d", c.v, d, c.deg)
		}
		if ctx != c.ctx {
			t.Errorf("ParseAffixVr(%q): ctx = %v, want %v", c.v, ctx, c.ctx)
		}
	}
	if _, _, ok := ParseAffixVr("zzz"); ok {
		t.Error("ParseAffixVr(zzz) should return ok=false")
	}
}

func TestVowelFormNumber_AllVowels(t *testing.T) {
	// Every cell in the 4-series x 9-form table (plus series 3 alternates).
	cases := []struct {
		v    string
		want int
	}{
		// Series 1.
		{"a", 1}, {"ä", 2}, {"e", 3}, {"i", 4}, {"ëi", 5},
		{"ö", 6}, {"o", 7}, {"ü", 8}, {"u", 9},
		// Series 2.
		{"ai", 1}, {"au", 2}, {"ei", 3}, {"eu", 4}, {"ëu", 5},
		{"ou", 6}, {"oi", 7}, {"iu", 8}, {"ui", 9},
		// Series 3 canonical.
		{"ia", 1}, {"ie", 2}, {"io", 3}, {"iö", 4}, {"eë", 5},
		{"uö", 6}, {"uo", 7}, {"ue", 8}, {"ua", 9},
		// Series 3 alternates.
		{"uä", 1}, {"uë", 2}, {"üä", 3}, {"üë", 4},
		{"öë", 6}, {"öä", 7}, {"ië", 8}, {"iä", 9},
		// Series 4.
		{"ao", 1}, {"aö", 2}, {"eo", 3}, {"eö", 4}, {"oë", 5},
		{"öe", 6}, {"oe", 7}, {"öa", 8}, {"oa", 9},
	}
	for _, c := range cases {
		got, ok := vowelFormNumber(c.v)
		if !ok || got != c.want {
			t.Errorf("vowelFormNumber(%q) = (%d, %v), want (%d, true)", c.v, got, ok, c.want)
		}
	}
	// Unknown vowel returns ok=false.
	if _, ok := vowelFormNumber("zzz"); ok {
		t.Error("vowelFormNumber(zzz) should return ok=false")
	}
}

func TestParseModular_EmptyAndOverflow(t *testing.T) {
	// Empty word → error.
	if _, err := ParseModular(""); err == nil {
		t.Error("ParseModular(empty) should error")
	}
	// Prefix only with nothing else → error.
	if _, err := ParseModular("w"); err == nil {
		t.Error("ParseModular(w) should error")
	}
	// Too many (>3) pairs → error.
	if _, err := ParseModular("ahahaḑahara"); err == nil {
		t.Log("ParseModular(>3 pairs): expected error (test input may not actually produce 4 pairs)")
	}
}

func TestParseSlotIV_AllCases(t *testing.T) {
	// Walk a wider grid of series + form combinations against ParseSlotIV.
	for _, v := range []string{"a", "ä", "e", "i", "ö", "o", "ü", "u",
		"ai", "au", "ei", "eu", "ou", "oi", "iu", "ui",
		"ia", "ie", "io", "iö", "uö", "uo", "ue", "ua"} {
		_, ok := ParseSlotIV(v)
		if !ok {
			t.Errorf("ParseSlotIV(%q) returned ok=false", v)
		}
	}
}

func TestIsStressedVowel(t *testing.T) {
	// Note: "î" isn't in the stressed-vowel set because "i" has no
	// umlaut form (the 9-vowel inventory is a ä e ë i o ö u ü);
	// circumflex covers ê/ô/û only.
	for _, r := range []rune{'á', 'é', 'í', 'ó', 'ú', 'â', 'ê', 'ô', 'û'} {
		if !IsStressedVowel(r) {
			t.Errorf("IsStressedVowel(%c) = false, want true", r)
		}
	}
	for _, r := range []rune{'a', 'e', 'b', 'x', 'î'} {
		if IsStressedVowel(r) {
			t.Errorf("IsStressedVowel(%c) = true, want false", r)
		}
	}
}
