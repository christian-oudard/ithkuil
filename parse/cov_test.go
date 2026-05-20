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
		{grammar.Type1Affix, -1, ""},
		{grammar.Type1Affix, 10, ""},
	}
	for _, c := range cases {
		got := AffixVowel(c.t, c.degree)
		if got != c.want {
			t.Errorf("AffixVowel(%v,%d) = %q, want %q", c.t, c.degree, got, c.want)
		}
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

func TestParseSpecialVv(t *testing.T) {
	// ëi → PRC + STA (Cs-root)
	sv, ok := ParseSpecialVv("ëi")
	if !ok {
		t.Fatal("ParseSpecialVv(ëi) returned ok=false")
	}
	if sv.Version != grammar.PRC {
		t.Errorf("ëi: Version = %v, want PRC", sv.Version)
	}
	if sv.Function == nil || *sv.Function != grammar.STA {
		t.Errorf("ëi: Function = %v, want STA", sv.Function)
	}
	// ae → ref-root (Function == nil)
	sv, ok = ParseSpecialVv("ae")
	if !ok {
		t.Fatal("ParseSpecialVv(ae) returned ok=false")
	}
	if sv.Function != nil {
		t.Errorf("ae: Function = %v, want nil (ref-root)", sv.Function)
	}
	if _, ok := ParseSpecialVv("zzz"); ok {
		t.Error("ParseSpecialVv(zzz) should return ok=false")
	}
}

func TestParseAffixVr(t *testing.T) {
	// ParseAffixVr decodes a Cs-root Vr vowel into (degree, context).
	// The 9 standard Vr forms correspond to 9 (degree, context) pairs.
	d, ctx, ok := ParseAffixVr("a")
	if !ok {
		t.Fatal("ParseAffixVr(a) returned ok=false")
	}
	if d < 1 || d > 9 {
		t.Errorf("ParseAffixVr(a): degree = %d, want 1-9", d)
	}
	if ctx.String() == "" {
		t.Errorf("ParseAffixVr(a): empty context")
	}
	if _, _, ok := ParseAffixVr("zzz"); ok {
		t.Error("ParseAffixVr(zzz) should return ok=false")
	}
}

func TestIsStressedVowel(t *testing.T) {
	// Note: "î" isn't in the stressed-vowel set (it's reserved as the
	// diaeresis hiatus marker "ï" pair); circumflex covers ê/ô/û only.
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
