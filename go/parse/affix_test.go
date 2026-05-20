package parse

import (
	"reflect"
	"testing"

	"github.com/coudard/ithkuil/go/grammar"
)

func TestClassifyAffixVowel(t *testing.T) {
	cases := []struct {
		in     string
		typ    grammar.AffixType
		degree int
	}{
		// Type 1 (Series 1)
		{"a", grammar.Type1Affix, 1},
		{"ä", grammar.Type1Affix, 2},
		{"ëi", grammar.Type1Affix, 5},
		{"u", grammar.Type1Affix, 9},
		{"ae", grammar.Type1Affix, 0},
		// Type 2 (Series 2)
		{"ai", grammar.Type2Affix, 1},
		{"ui", grammar.Type2Affix, 9},
		{"ea", grammar.Type2Affix, 0},
		// Type 3 (Series 3)
		{"ia", grammar.Type3Affix, 1},
		{"uä", grammar.Type3Affix, 1}, // alternate
		{"ie", grammar.Type3Affix, 2},
		{"uë", grammar.Type3Affix, 2},
		{"iä", grammar.Type3Affix, 9},
		{"üo", grammar.Type3Affix, 0},
		// Junk falls back to Type 1 / degree 0.
		{"xyz", grammar.Type1Affix, 0},
	}
	for _, c := range cases {
		typ, deg := ClassifyAffixVowel(c.in)
		if typ != c.typ || deg != c.degree {
			t.Errorf("ClassifyAffixVowel(%q) = (%v, %d), want (%v, %d)",
				c.in, typ, deg, c.typ, c.degree)
		}
	}
}

func TestParseAffixes_VxCs(t *testing.T) {
	got := ParseAffixes("aval")
	want := []grammar.Affix{
		{Type: grammar.Type1Affix, Degree: 1, Consonant: "v"},
		{Type: grammar.Type1Affix, Degree: 1, Consonant: "l"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("ParseAffixes(\"aval\") = %#v, want %#v", got, want)
	}
}

func TestParseAffixes_CsVx(t *testing.T) {
	// "la" is Cs+Vx — the slot-V ordering — and the affix's vowel is
	// still "a"; only the surface ordering is swapped.
	got := ParseAffixes("la")
	want := []grammar.Affix{
		{Type: grammar.Type1Affix, Degree: 1, Consonant: "l"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("ParseAffixes(\"la\") = %#v, want %#v", got, want)
	}
}

func TestParseAffixes_MultipleTypes(t *testing.T) {
	// "aival" → ["ai", "v", "a", "l"]
	// First affix: Type 2 (ai = degree 1). Second: Type 1 (a = degree 1).
	got := ParseAffixes("aival")
	want := []grammar.Affix{
		{Type: grammar.Type2Affix, Degree: 1, Consonant: "v"},
		{Type: grammar.Type1Affix, Degree: 1, Consonant: "l"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("ParseAffixes(\"aival\") = %#v, want %#v", got, want)
	}
}

func TestParseAffixes_Type3(t *testing.T) {
	got := ParseAffixes("iav")
	want := []grammar.Affix{
		{Type: grammar.Type3Affix, Degree: 1, Consonant: "v"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("ParseAffixes(\"iav\") = %#v, want %#v", got, want)
	}
}

func TestParseAffixes_Empty(t *testing.T) {
	if got := ParseAffixes(""); got != nil {
		t.Errorf("ParseAffixes(\"\") = %#v, want nil", got)
	}
}

func TestParseAffixes_LoneConjunct(t *testing.T) {
	// A single conjunct can't form a pair.
	if got := ParseAffixes("a"); got != nil {
		t.Errorf("ParseAffixes(\"a\") = %#v, want nil", got)
	}
	if got := ParseAffixes("l"); got != nil {
		t.Errorf("ParseAffixes(\"l\") = %#v, want nil", got)
	}
}

func TestParseAffixes_ConsonantCluster(t *testing.T) {
	// splitConjuncts groups consecutive consonants into one conjunct.
	// "avalr" segments as ["a","v","a","lr"], so the second affix's
	// consonant is "lr" — that's the real surface form of the affix
	// identifier. Disambiguating "l" + trailing "r" is a higher-level
	// concern (lexicon + Ca-end detection) that lives in FullParse.
	got := ParseAffixes("avalr")
	want := []grammar.Affix{
		{Type: grammar.Type1Affix, Degree: 1, Consonant: "v"},
		{Type: grammar.Type1Affix, Degree: 1, Consonant: "lr"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("ParseAffixes(\"avalr\") = %#v, want %#v", got, want)
	}
}

func TestParseAffixes_OddTrailing(t *testing.T) {
	// 3 conjuncts: the last unpaired conjunct is silently dropped.
	got := ParseAffixes("aval")
	want := []grammar.Affix{
		{Type: grammar.Type1Affix, Degree: 1, Consonant: "v"},
		{Type: grammar.Type1Affix, Degree: 1, Consonant: "l"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("ParseAffixes(\"aval\") = %#v, want %#v", got, want)
	}
	// Vowel-initial + 3 mid conjuncts = ["a","v","a","l","a"]: pair takes
	// (a,v), (a,l), leaves trailing "a".
	got2 := ParseAffixes("avala")
	if !reflect.DeepEqual(got2, want) {
		t.Errorf("ParseAffixes(\"avala\") = %#v, want %#v", got2, want)
	}
}

func TestIsVowelConjunctIsConsonantConjunct(t *testing.T) {
	if !IsVowelConjunct("ai") {
		t.Error("IsVowelConjunct(\"ai\") = false")
	}
	if IsVowelConjunct("") {
		t.Error("IsVowelConjunct(\"\") = true")
	}
	if !IsConsonantConjunct("ţř") {
		t.Error("IsConsonantConjunct(\"ţř\") = false")
	}
	if IsConsonantConjunct("") {
		t.Error("IsConsonantConjunct(\"\") = true")
	}
}
