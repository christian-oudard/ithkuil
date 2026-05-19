package parse

import (
	"reflect"
	"testing"

	"github.com/coudard/ithkuil/go/grammar"
)

func TestSplitConjuncts(t *testing.T) {
	cases := []struct {
		in   string
		want []string
	}{
		{"mala", []string{"m", "a", "l", "a"}},
		{"ţřai", []string{"ţř", "ai"}},
		{"emal", []string{"e", "m", "a", "l"}},
		{"", nil},
		{"Malëuţřait", []string{"M", "a", "l", "ëu", "ţř", "ai", "t"}},
	}
	for _, c := range cases {
		got := SplitConjuncts(c.in)
		if !reflect.DeepEqual(got, c.want) {
			t.Errorf("SplitConjuncts(%q) = %#v, want %#v", c.in, got, c.want)
		}
	}
}

func TestNormalizeAccents(t *testing.T) {
	cases := []struct {
		in, want string
	}{
		{"a", "a"},
		{"á", "a"},
		{"é", "e"},
		{"â", "ä"},
		{"ô", "ö"},
		{"ï", "i"},
		{"Malëuţřait", "Malëuţřait"},
		{"Maléuţřait", "Maleuţřait"},
	}
	for _, c := range cases {
		if got := NormalizeAccents(c.in); got != c.want {
			t.Errorf("NormalizeAccents(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestParseSlotII(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.SlotII
		ok   bool
	}{
		{"a", grammar.SlotII{Stem: grammar.S1, Version: grammar.PRC}, true},
		{"ä", grammar.SlotII{Stem: grammar.S1, Version: grammar.CPT}, true},
		{"e", grammar.SlotII{Stem: grammar.S2, Version: grammar.PRC}, true},
		{"i", grammar.SlotII{Stem: grammar.S2, Version: grammar.CPT}, true},
		{"u", grammar.SlotII{Stem: grammar.S3, Version: grammar.PRC}, true},
		{"ü", grammar.SlotII{Stem: grammar.S3, Version: grammar.CPT}, true},
		{"o", grammar.SlotII{Stem: grammar.S0, Version: grammar.PRC}, true},
		{"ö", grammar.SlotII{Stem: grammar.S0, Version: grammar.CPT}, true},
		{"á", grammar.SlotII{Stem: grammar.S1, Version: grammar.PRC}, true}, // accent stripped
		{"ëi", grammar.SlotII{}, false},                      // form 5 reserved
		{"x", grammar.SlotII{}, false},
	}
	for _, c := range cases {
		got, ok := ParseSlotII(c.in)
		if ok != c.ok || got != c.want {
			t.Errorf("ParseSlotII(%q) = (%v,%v), want (%v,%v)",
				c.in, got, ok, c.want, c.ok)
		}
	}
}

func TestParseSlotIV(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.SlotIV
		ok   bool
	}{
		// Series 1 = EXS
		{"a", grammar.SlotIV{Function: grammar.STA, Specification: grammar.BSC, Context: grammar.EXS}, true},
		{"ä", grammar.SlotIV{Function: grammar.STA, Specification: grammar.CTE, Context: grammar.EXS}, true},
		{"e", grammar.SlotIV{Function: grammar.STA, Specification: grammar.CSV, Context: grammar.EXS}, true},
		{"i", grammar.SlotIV{Function: grammar.STA, Specification: grammar.OBJ, Context: grammar.EXS}, true},
		{"u", grammar.SlotIV{Function: grammar.DYN, Specification: grammar.BSC, Context: grammar.EXS}, true},
		{"o", grammar.SlotIV{Function: grammar.DYN, Specification: grammar.CSV, Context: grammar.EXS}, true},
		// Series 2 = FNC
		{"ai", grammar.SlotIV{Function: grammar.STA, Specification: grammar.BSC, Context: grammar.FNC}, true},
		{"au", grammar.SlotIV{Function: grammar.STA, Specification: grammar.CTE, Context: grammar.FNC}, true},
		{"ui", grammar.SlotIV{Function: grammar.DYN, Specification: grammar.BSC, Context: grammar.FNC}, true},
		// Series 3 = RPS (with alternate)
		{"ia", grammar.SlotIV{Function: grammar.STA, Specification: grammar.BSC, Context: grammar.RPS}, true},
		{"ie", grammar.SlotIV{Function: grammar.STA, Specification: grammar.CTE, Context: grammar.RPS}, true},
		{"ua", grammar.SlotIV{Function: grammar.DYN, Specification: grammar.BSC, Context: grammar.RPS}, true},
		{"uä", grammar.SlotIV{Function: grammar.STA, Specification: grammar.BSC, Context: grammar.RPS}, true},
		{"uë", grammar.SlotIV{Function: grammar.STA, Specification: grammar.CTE, Context: grammar.RPS}, true},
		// Form 5 reserved
		{"ëi", grammar.SlotIV{}, false},
		// Junk
		{"x", grammar.SlotIV{}, false},
	}
	for _, c := range cases {
		got, ok := ParseSlotIV(c.in)
		if ok != c.ok || got != c.want {
			t.Errorf("ParseSlotIV(%q) = (%v,%v), want (%v,%v)",
				c.in, got, ok, c.want, c.ok)
		}
	}
}

func TestIsVowelChar(t *testing.T) {
	for _, r := range "aäeëiïoöuü" {
		if !IsVowelChar(r) {
			t.Errorf("IsVowelChar(%q) = false, want true", r)
		}
	}
	for _, r := range "mtkpřţ'" {
		if IsVowelChar(r) {
			t.Errorf("IsVowelChar(%q) = true, want false", r)
		}
	}
}
