package parse

import (
	"testing"

	"github.com/coudard/ithkuil/go/grammar"
)

func TestParseVk_Series1(t *testing.T) {
	// Series 1 = ASR + each of the 9 validations.
	cases := []struct {
		in  string
		val grammar.Validation
	}{
		{"a", grammar.OBS},
		{"ä", grammar.REC},
		{"e", grammar.PUP},
		{"i", grammar.RPR},
		{"ëi", grammar.USP},
		{"ö", grammar.IMA},
		{"o", grammar.CVN},
		{"ü", grammar.ITU},
		{"u", grammar.INF},
	}
	for _, c := range cases {
		ill, val, ok := ParseVk(c.in)
		if !ok || ill != grammar.ASR || val != c.val {
			t.Errorf("ParseVk(%q) = (%v,%v,%v), want (ASR,%v,true)",
				c.in, ill, val, ok, c.val)
		}
	}
}

func TestParseVk_Series2(t *testing.T) {
	cases := []struct {
		in  string
		ill grammar.Illocution
	}{
		{"ai", grammar.DIR},
		{"au", grammar.DEC},
		{"ei", grammar.IRG},
		{"eu", grammar.VER},
		{"ou", grammar.ADM},
		{"oi", grammar.POT},
		{"iu", grammar.HOR},
		{"ui", grammar.CNJ},
	}
	for _, c := range cases {
		ill, val, ok := ParseVk(c.in)
		if !ok || ill != c.ill || val != grammar.OBS {
			t.Errorf("ParseVk(%q) = (%v,%v,%v), want (%v,OBS,true)",
				c.in, ill, val, ok, c.ill)
		}
	}
}

func TestParseVk_AcceptsAccents(t *testing.T) {
	// Stressed vowel should normalize before lookup.
	ill, val, ok := ParseVk("á")
	if !ok || ill != grammar.ASR || val != grammar.OBS {
		t.Errorf("ParseVk(\"á\") = (%v,%v,%v), want (ASR,OBS,true)", ill, val, ok)
	}
}

func TestParseVk_Rejects(t *testing.T) {
	for _, s := range []string{"", "x", "ia", "ao"} {
		if ill, val, ok := ParseVk(s); ok {
			t.Errorf("ParseVk(%q) = (%v,%v,true), want failure", s, ill, val)
		}
	}
}
