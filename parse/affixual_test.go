package parse

import (
	"testing"

	"github.com/christian-oudard/ithkuil/grammar"
)

func TestParseSingleAffix_VxCs(t *testing.T) {
	a, err := ParseSingleAffix("ar")
	if err != nil {
		t.Fatalf("ParseSingleAffix(\"ar\") error: %v", err)
	}
	if a.Affix.Consonant != "r" || a.Affix.Type != grammar.Type1Affix || a.Affix.Degree != 1 {
		t.Errorf("ParseSingleAffix(\"ar\") = %+v, want Affix{Type1, deg 1, r}", a.Affix)
	}
}

func TestParseSingleAffix_CsVx(t *testing.T) {
	a, err := ParseSingleAffix("ra")
	if err != nil {
		t.Fatalf("ParseSingleAffix(\"ra\") error: %v", err)
	}
	if a.Affix.Consonant != "r" || a.Affix.Type != grammar.Type1Affix || a.Affix.Degree != 1 {
		t.Errorf("ParseSingleAffix(\"ra\") = %+v, want Affix{Type1, deg 1, r}", a.Affix)
	}
}

func TestParseSingleAffix_WithVs(t *testing.T) {
	a, err := ParseSingleAffix("are")
	if err != nil {
		t.Fatalf("ParseSingleAffix(\"are\") error: %v", err)
	}
	if a.Affix.Consonant != "r" || a.Affix.Type != grammar.Type1Affix || a.Affix.Degree != 1 {
		t.Errorf("ParseSingleAffix(\"are\") = %+v, want Affix{Type1, deg 1, r}", a.Affix)
	}
	if a.Scope != grammar.ScopeVIIDom {
		t.Errorf("ParseSingleAffix(\"are\").Scope = %v, want VIIDom", a.Scope)
	}
}

func TestParseSingleAffix_ScopeFromVs(t *testing.T) {
	cases := []struct {
		word string
		want grammar.AffixScope
	}{
		// Default: no Vs → VDom.
		{"ar", grammar.ScopeVDom},
		// Vs="a" → VDom (explicit default).
		{"ara", grammar.ScopeVDom},
		{"aru", grammar.ScopeVSub},
		{"are", grammar.ScopeVIIDom},
		{"ari", grammar.ScopeVIISub},
		{"aro", grammar.ScopeFormative},
		{"arö", grammar.ScopeAdjacent},
	}
	for _, c := range cases {
		a, err := ParseSingleAffix(c.word)
		if err != nil {
			t.Fatalf("ParseSingleAffix(%q): %v", c.word, err)
		}
		if a.Scope != c.want {
			t.Errorf("ParseSingleAffix(%q).Scope = %v, want %v", c.word, a.Scope, c.want)
		}
	}
}

func TestParseSingleAffix_Rejects(t *testing.T) {
	for _, w := range []string{"", "a", "r", "aa", "rr"} {
		if _, err := ParseSingleAffix(w); err == nil {
			t.Errorf("ParseSingleAffix(%q) succeeded, want error", w)
		}
	}
}

func TestParseMultipleAffix_Basic(t *testing.T) {
	// "xaheitr" → first=(x, a)→{Type1, deg 1, x}, Cz=h, more=[(ei, tr)]→{Type2, deg 3, tr}, Vz="".
	ma, err := ParseMultipleAffix("xaheitr")
	if err != nil {
		t.Fatalf("ParseMultipleAffix(\"xaheitr\") error: %v", err)
	}
	if ma.First.Consonant != "x" || ma.First.Type != grammar.Type1Affix || ma.First.Degree != 1 {
		t.Errorf("first = %+v, want {Type1, deg 1, x}", ma.First)
	}
	if ma.FirstScope != grammar.ScopeVDom {
		t.Errorf("FirstScope = %v, want VDom (Cz=h)", ma.FirstScope)
	}
	if len(ma.Rest) != 1 || ma.Rest[0].Consonant != "tr" || ma.Rest[0].Type != grammar.Type2Affix {
		t.Errorf("Rest = %+v, want one Affix{Type2, _, tr}", ma.Rest)
	}
}

func TestParseMultipleAffix_WithVz(t *testing.T) {
	ma, err := ParseMultipleAffix("xaheitre")
	if err != nil {
		t.Fatalf("ParseMultipleAffix(\"xaheitre\") error: %v", err)
	}
	if ma.RestScope != grammar.ScopeVIIDom {
		t.Errorf("RestScope = %v, want VIIDom (Vz=e)", ma.RestScope)
	}
}

func TestParseMultipleAffix_GlottalCz(t *testing.T) {
	ma, err := ParseMultipleAffix("xa'heitr")
	if err != nil {
		t.Fatalf("ParseMultipleAffix(\"xa'heitr\") error: %v", err)
	}
	if ma.FirstScope != grammar.ScopeVSub {
		t.Errorf("FirstScope = %v, want VSub (Cz='h)", ma.FirstScope)
	}
}

func TestParseMultipleAffix_LeadingEPrefix(t *testing.T) {
	ma, err := ParseMultipleAffix("ëxaheitr")
	if err != nil {
		t.Fatalf("ParseMultipleAffix(\"ëxaheitr\") error: %v", err)
	}
	if ma.First.Consonant != "x" {
		t.Errorf("after ë prefix: First.Consonant = %q, want x", ma.First.Consonant)
	}
}

func TestParseMultipleAffix_FirstScopeFromCz(t *testing.T) {
	cases := []struct {
		word string
		want grammar.AffixScope
	}{
		{"xaheitr", grammar.ScopeVDom},
		{"xa'heitr", grammar.ScopeVSub},
		{"xa'hleitr", grammar.ScopeVIIDom},
		{"xa'hreitr", grammar.ScopeVIISub},
		{"xahweitr", grammar.ScopeFormative},
		{"xa'hweitr", grammar.ScopeAdjacent},
	}
	for _, c := range cases {
		ma, err := ParseMultipleAffix(c.word)
		if err != nil {
			t.Fatalf("ParseMultipleAffix(%q): %v", c.word, err)
		}
		if ma.FirstScope != c.want {
			t.Errorf("%q: FirstScope = %v, want %v", c.word, ma.FirstScope, c.want)
		}
	}
}

func TestParseMultipleAffix_RestScopeFromVz(t *testing.T) {
	// Vz="" or "ai" → RestScope mirrors FirstScope (from Cz).
	ma, err := ParseMultipleAffix("xahweitr")
	if err != nil {
		t.Fatal(err)
	}
	if ma.RestScope != ma.FirstScope {
		t.Errorf("Vz absent: RestScope = %v, want = FirstScope %v",
			ma.RestScope, ma.FirstScope)
	}
	ma, err = ParseMultipleAffix("xahweitrai")
	if err != nil {
		t.Fatal(err)
	}
	if ma.RestScope != ma.FirstScope {
		t.Errorf("Vz=ai: RestScope = %v, want = FirstScope %v",
			ma.RestScope, ma.FirstScope)
	}
	// Explicit Vz overrides.
	ma, err = ParseMultipleAffix("xaheitre")
	if err != nil {
		t.Fatal(err)
	}
	if ma.RestScope != grammar.ScopeVIIDom {
		t.Errorf("Vz=e: RestScope = %v, want VIIDom", ma.RestScope)
	}
}

func TestParseMultipleAffix_Rejects(t *testing.T) {
	// Words that don't fit the [ë] Cs Vx Cz (VxCs)+ [Vz] shape.
	for _, w := range []string{"", "a", "ara", "rar", "xa", "xahei"} {
		if _, err := ParseMultipleAffix(w); err == nil {
			t.Errorf("ParseMultipleAffix(%q) succeeded, want error", w)
		}
	}
}
