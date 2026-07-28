package parse

import (
	"testing"

	"github.com/christian-oudard/ithkuil/grammar"
)

func TestParseVnCn_Pattern1(t *testing.T) {
	cases := []struct {
		vn, cn string
		want   grammar.SlotVIII
	}{
		// Valence (Series 1).
		{"a", "h", grammar.VnCnValence{Valence: grammar.MNO, MoodScope: grammar.FAC}},
		{"u", "hň", grammar.VnCnValence{Valence: grammar.PTI, MoodScope: grammar.HYP}},
		// Phase (Series 2).
		{"ai", "h", grammar.VnCnPhase{Phase: grammar.PCT, MoodScope: grammar.FAC}},
		// Effect (Series 3, canonical + alternate).
		{"ia", "h", grammar.VnCnEffect{Effect: grammar.BEN1, MoodScope: grammar.FAC}},
		{"uä", "hl", grammar.VnCnEffect{Effect: grammar.BEN1, MoodScope: grammar.SUB}},
		// Level (Series 4).
		{"ao", "h", grammar.VnCnLevel{Level: grammar.MIN, Absolute: false, MoodScope: grammar.FAC}},
		{"oa", "hm", grammar.VnCnLevel{Level: grammar.MAX, Absolute: false, MoodScope: grammar.SPC}},
	}
	for _, c := range cases {
		got, ok := ParseVnCn(c.vn, c.cn)
		if !ok {
			t.Errorf("ParseVnCn(%q, %q) failed", c.vn, c.cn)
			continue
		}
		if got != c.want {
			t.Errorf("ParseVnCn(%q, %q) = %#v, want %#v", c.vn, c.cn, got, c.want)
		}
	}
}

func TestParseVnCn_Pattern2(t *testing.T) {
	// Pattern-2 Cn pairs with Aspect Vn. MoodScope is stored as Mood
	// (CaseScope label is applied by gloss for nominal contexts).
	cases := []struct {
		vn, cn string
		want   grammar.SlotVIII
	}{
		{"a", "w", grammar.VnCnAspect{Aspect: grammar.RTR, MoodScope: grammar.FAC}},
		{"a", "y", grammar.VnCnAspect{Aspect: grammar.RTR, MoodScope: grammar.FAC}},
		{"ai", "hw", grammar.VnCnAspect{Aspect: grammar.RSM, MoodScope: grammar.SUB}},
		{"ia", "hňw", grammar.VnCnAspect{Aspect: grammar.PMP, MoodScope: grammar.HYP}},
	}
	for _, c := range cases {
		got, ok := ParseVnCn(c.vn, c.cn)
		if !ok {
			t.Errorf("ParseVnCn(%q, %q) failed", c.vn, c.cn)
			continue
		}
		if got != c.want {
			t.Errorf("ParseVnCn(%q, %q) = %#v, want %#v", c.vn, c.cn, got, c.want)
		}
	}
}

func TestParseVnCn_InvalidCn(t *testing.T) {
	if got, ok := ParseVnCn("a", "x"); ok {
		t.Errorf("ParseVnCn(\"a\", \"x\") = %#v, want failure", got)
	}
	if got, ok := ParseVnCn("a", ""); ok {
		t.Errorf("ParseVnCn(\"a\", \"\") = %#v, want failure", got)
	}
}

func TestParseVnCn_InvalidVn(t *testing.T) {
	if got, ok := ParseVnCn("xyz", "h"); ok {
		t.Errorf("ParseVnCn(\"xyz\", \"h\") = %#v, want failure", got)
	}
}
