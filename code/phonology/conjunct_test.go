package phonology

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

// §1.7 Rule 3: a single vowel reduplicates around the glottal-stop and a
// diphthong takes it intervocalically. These are the spellings a
// word-final vowel-form is forced into, and the ones the case tables are
// keyed on. Expectations come from the rule text, not from the tables.
func TestGlottalizeVowel(t *testing.T) {
	cases := []struct{ in, want string }{
		{"a", "a'a"},  // Rule 3, single vowel: PRN
		{"ä", "ä'ä"},  // DSP
		{"u", "u'u"},  // RLT
		{"ai", "a'i"}, // Rule 3, diphthong: ACT
		{"ui", "u'i"}, // VOC
		{"ëi", "ë'i"}, // COM
		{"ia", "i'a"}, // Rule 2, disyllabic: LOC
		{"ua", "u'a"}, // NAV
		{"uä", "u'ä"}, // the series-3 alternate of NAV
		{"", ""},
	}
	for _, c := range cases {
		if got := GlottalizeVowel(c.in); got != c.want {
			t.Errorf("GlottalizeVowel(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

// DissimilateGlides is §1.6's footnote applied to a whole word. A Cr
// ending in -y or -w puts a glide directly before Slot IV's Vr, which
// is Series 3 whenever the Context is RPS, so this is reachable without
// any Slot I shortcut.
func TestDissimilateGlides(t *testing.T) {
	cases := []struct{ in, want string }{
		{"lyiala", "lyuäla"}, // y + ia
		{"lwuala", "lwiäla"}, // w + ua
		{"lwiala", "lwiala"}, // w + i-initial: untouched
		{"lyuala", "lyuala"}, // y + u-initial: untouched
		{"lyuäla", "lyuäla"}, // idempotent
		{"člala", "člala"},   // no glide
		{"lyëila", "lyëila"}, // series 1 form 5 after a glide
		{"", ""},
	}
	for _, c := range cases {
		if got := DissimilateGlides(c.in); got != c.want {
			t.Errorf("DissimilateGlides(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

// The §1.7 glottal-stop does not exempt a vowel from §1.6's footnote.
// It is a separate marker docked onto whichever spelling the footnote
// selects — the §3.9.1 SPECIAL NOTE can move it onto another slot's
// vowel entirely — so yi'a dissimilates exactly as yia does.
//
// Reachable through the Cc shortcut: §3.5.1 forces a glottal into Vv
// when Slot V holds two or more affixes, and a y- or w- shortcut puts
// that Vv straight after the glide.
func TestDissimilateGlides_Glottalized(t *testing.T) {
	cases := []struct{ in, want string }{
		{"yi'ačl", "yu'äčl"}, // y + glottalized ia
		{"wu'ačl", "wi'äčl"}, // w + glottalized ua
		{"wi'ačl", "wi'ačl"}, // w + i-initial: untouched
		{"yu'ačl", "yu'ačl"}, // y + u-initial: untouched
		{"ya'ačl", "ya'ačl"}, // series 1, reduplicated around the stop
		{"yu'äčl", "yu'äčl"}, // idempotent
	}
	for _, c := range cases {
		if got := DissimilateGlides(c.in); got != c.want {
			t.Errorf("DissimilateGlides(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}
