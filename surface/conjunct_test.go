package surface

import (
	"reflect"
	"testing"
)

// SplitConjuncts and JoinConjuncts are inverses on every well-formed
// input. The property holds for any string composed of recognized
// vowel and consonant runes: JoinConjuncts is just strings.Join with
// an empty separator, so the only thing being verified is that
// SplitConjuncts doesn't drop or reorder characters.
func TestSplitJoin_RoundTrip(t *testing.T) {
	cases := []string{
		"malëuţřait",
		"amlala",
		"fkhalo",
		"ihwe",
		"öhwoňo",
		"agulaha",
		"la",
		"to",
		"mzalörmëiňva",
		"walurx",
		"ëilal",
		"ealali",
		"jwala",
		"",
		"a",
		"m",
	}
	for _, w := range cases {
		got := JoinConjuncts(SplitConjuncts(w))
		if got != w {
			t.Errorf("JoinConjuncts(SplitConjuncts(%q)) = %q, want %q", w, got, w)
		}
	}
}

// SplitConjuncts produces the expected segmentation for a few hand-
// picked inputs, including the canonical formative.
func TestSplitConjuncts_Shape(t *testing.T) {
	cases := []struct {
		word string
		want []string
	}{
		{"malëuţřait", []string{"m", "a", "l", "ëu", "ţř", "ai", "t"}},
		{"ţřai", []string{"ţř", "ai"}},
		{"emal", []string{"e", "m", "a", "l"}},
		{"a", []string{"a"}},
		{"m", []string{"m"}},
		{"", nil},
	}
	for _, c := range cases {
		got := SplitConjuncts(c.word)
		if !reflect.DeepEqual(got, c.want) {
			t.Errorf("SplitConjuncts(%q) = %q, want %q", c.word, got, c.want)
		}
	}
}

func TestIsVowel(t *testing.T) {
	for _, r := range []rune{'a', 'ä', 'e', 'i', 'o', 'á', 'ô', 'ë'} {
		if !IsVowel(r) {
			t.Errorf("IsVowel(%q) = false, want true", r)
		}
	}
	for _, r := range []rune{'m', 'l', 'ţ', 'ř', 'k', ' ', '\'', '-'} {
		if IsVowel(r) {
			t.Errorf("IsVowel(%q) = true, want false", r)
		}
	}
}

func TestIsVowelConjunct(t *testing.T) {
	cases := []struct {
		s    string
		want bool
	}{
		{"a", true},
		{"ëu", true},
		{"ai", true},
		{"m", false},
		{"ţř", false},
		{"", false},
	}
	for _, c := range cases {
		if got := IsVowelConjunct(c.s); got != c.want {
			t.Errorf("IsVowelConjunct(%q) = %v, want %v", c.s, got, c.want)
		}
	}
}

func TestIsConsonantConjunct(t *testing.T) {
	for _, c := range []struct {
		s    string
		want bool
	}{
		{"m", true},
		{"ţř", true},
		{"a", false},
		{"ëu", false},
		{"", false},
	} {
		if got := IsConsonantConjunct(c.s); got != c.want {
			t.Errorf("IsConsonantConjunct(%q) = %v, want %v", c.s, got, c.want)
		}
	}
}
