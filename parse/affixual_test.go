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
	if a.Vx != "a" || a.Cs != "r" {
		t.Errorf("ParseSingleAffix(\"ar\") = %v, want {Vx=a, Cs=r}", a)
	}
}

func TestParseSingleAffix_CsVx(t *testing.T) {
	a, err := ParseSingleAffix("ra")
	if err != nil {
		t.Fatalf("ParseSingleAffix(\"ra\") error: %v", err)
	}
	if a.Vx != "a" || a.Cs != "r" {
		t.Errorf("ParseSingleAffix(\"ra\") = %v, want {Vx=a, Cs=r}", a)
	}
}

func TestParseSingleAffix_WithVs(t *testing.T) {
	a, err := ParseSingleAffix("are")
	if err != nil {
		t.Fatalf("ParseSingleAffix(\"are\") error: %v", err)
	}
	if a.Vx != "a" || a.Cs != "r" || a.Vs != "e" {
		t.Errorf("got %+v, want {Vx=a, Cs=r, Vs=e}", a)
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
	// "xaheitr" → first=(x, a), Cz=h, more=[(ei, tr)], Vz="".
	ma, err := ParseMultipleAffix("xaheitr")
	if err != nil {
		t.Fatalf("ParseMultipleAffix(\"xaheitr\") error: %v", err)
	}
	if ma.First.Cs != "x" || ma.First.Vx != "a" {
		t.Errorf("first = %v, want {Cs=x, Vx=a}", ma.First)
	}
	if ma.Cz != "h" {
		t.Errorf("Cz = %q, want \"h\"", ma.Cz)
	}
	if len(ma.Affixes) != 1 || ma.Affixes[0] != (grammar.AffixPair{Vx: "ei", Cs: "tr"}) {
		t.Errorf("Affixes = %v, want [{ei, tr}]", ma.Affixes)
	}
	if ma.Vz != "" {
		t.Errorf("Vz = %q, want empty", ma.Vz)
	}
}

func TestParseMultipleAffix_WithVz(t *testing.T) {
	ma, err := ParseMultipleAffix("xaheitre")
	if err != nil {
		t.Fatalf("ParseMultipleAffix(\"xaheitre\") error: %v", err)
	}
	if ma.Vz != "e" {
		t.Errorf("Vz = %q, want \"e\"", ma.Vz)
	}
}

func TestParseMultipleAffix_GlottalCz(t *testing.T) {
	ma, err := ParseMultipleAffix("xa'heitr")
	if err != nil {
		t.Fatalf("ParseMultipleAffix(\"xa'heitr\") error: %v", err)
	}
	if ma.Cz != "'h" {
		t.Errorf("Cz = %q, want \"'h\"", ma.Cz)
	}
}

func TestParseMultipleAffix_LeadingEPrefix(t *testing.T) {
	ma, err := ParseMultipleAffix("ëxaheitr")
	if err != nil {
		t.Fatalf("ParseMultipleAffix(\"ëxaheitr\") error: %v", err)
	}
	if ma.First.Cs != "x" {
		t.Errorf("after ë prefix: First.Cs = %q, want x", ma.First.Cs)
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
