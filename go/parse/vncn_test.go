package parse

import (
	"testing"

	"github.com/coudard/ithkuil/go/grammar"
)

func TestParseVnCn_Pattern1(t *testing.T) {
	cases := []struct {
		vn, cn string
		want   grammar.SlotVIII
	}{
		// Valence (Series 1) + Mood.
		{"a", "h", grammar.VnCnValence{Valence: grammar.MNO, MS: grammar.MoodVal{Mood: grammar.FAC}}},
		{"u", "hň", grammar.VnCnValence{Valence: grammar.PTI, MS: grammar.MoodVal{Mood: grammar.HYP}}},
		// Phase (Series 2).
		{"ai", "h", grammar.VnCnPhase{Phase: grammar.PCT, MS: grammar.MoodVal{Mood: grammar.FAC}}},
		// Effect (Series 3, canonical + alternate).
		{"ia", "h", grammar.VnCnEffect{Effect: grammar.BEN1, MS: grammar.MoodVal{Mood: grammar.FAC}}},
		{"uä", "hl", grammar.VnCnEffect{Effect: grammar.BEN1, MS: grammar.MoodVal{Mood: grammar.SUB}}},
		// Level (Series 4).
		{"ao", "h", grammar.VnCnLevel{Level: grammar.MIN, Absolute: false, MS: grammar.MoodVal{Mood: grammar.FAC}}},
		{"oa", "hm", grammar.VnCnLevel{Level: grammar.MAX, Absolute: false, MS: grammar.MoodVal{Mood: grammar.SPC}}},
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
	// Pattern-2 Cn produces a CaseScopeVal initial parse. Aspect Vn applies.
	cases := []struct {
		vn, cn string
		want   grammar.SlotVIII
	}{
		{"a", "w", grammar.VnCnAspect{Aspect: grammar.RTR, MS: grammar.CaseScopeVal{CaseScope: grammar.CCN}}},
		{"a", "y", grammar.VnCnAspect{Aspect: grammar.RTR, MS: grammar.CaseScopeVal{CaseScope: grammar.CCN}}},
		{"ai", "hw", grammar.VnCnAspect{Aspect: grammar.RSM, MS: grammar.CaseScopeVal{CaseScope: grammar.CCA}}},
		{"ia", "hňw", grammar.VnCnAspect{Aspect: grammar.PMP, MS: grammar.CaseScopeVal{CaseScope: grammar.CCV}}},
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

func TestDisambiguateMoodScope_Verbal(t *testing.T) {
	// Ultimate stress = verbal: CaseScopeVal flips to MoodVal.
	got := grammar.DisambiguateMoodScope(grammar.Ultimate,
		grammar.CaseScopeVal{CaseScope: grammar.CCN})
	want := grammar.MoodVal{Mood: grammar.FAC}
	if got != want {
		t.Errorf("Ultimate + CCN = %#v, want %#v", got, want)
	}
	// Already a MoodVal passes through.
	mv := grammar.MoodVal{Mood: grammar.SUB}
	if got := grammar.DisambiguateMoodScope(grammar.Ultimate, mv); got != mv {
		t.Errorf("Ultimate + MoodVal{SUB} = %#v, want %#v", got, mv)
	}
}

func TestDisambiguateMoodScope_Nominal(t *testing.T) {
	// Penultimate stress = nominal: MoodVal flips to CaseScopeVal.
	got := grammar.DisambiguateMoodScope(grammar.Penultimate,
		grammar.MoodVal{Mood: grammar.FAC})
	want := grammar.CaseScopeVal{CaseScope: grammar.CCN}
	if got != want {
		t.Errorf("Penultimate + FAC = %#v, want %#v", got, want)
	}
}

func TestDisambiguateSlotVIII(t *testing.T) {
	// Parse a Pattern-2 form (gets CaseScopeVal), then apply Ultimate
	// stress to flip it to MoodVal.
	s, ok := ParseVnCn("a", "w")
	if !ok {
		t.Fatal("ParseVnCn(\"a\", \"w\") failed")
	}
	got := grammar.DisambiguateSlotVIII(grammar.Ultimate, s)
	want := grammar.VnCnAspect{
		Aspect: grammar.RTR,
		MS:     grammar.MoodVal{Mood: grammar.FAC},
	}
	if got != want {
		t.Errorf("DisambiguateSlotVIII(Ultimate, VnCnAspect{...CCN}) = %#v, want %#v", got, want)
	}
}
