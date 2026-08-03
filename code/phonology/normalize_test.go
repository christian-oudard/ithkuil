package phonology

import "testing"

// Decomposed (NFD) spellings: each diacritic here is a separate
// combining rune, not the precomposed letter it looks like.
const (
	nfdSvel   = "švel"         // s + combining caron
	nfdMala   = "malá"         // a + combining acute
	nfdMalut  = "malëuţřait" // e + diaeresis, t + cedilla, r + caron
	nfdZdot   = "ẓ"            // z + combining dot below
	nfdSvelUp = "Švel"         // capital S + combining caron
)

func TestNormalize(t *testing.T) {
	cases := []struct {
		name string
		in   string
		want string
	}{
		{"already canonical", "švel", "švel"},
		{"decomposed caron", nfdSvel, "švel"},
		{"decomposed acute", nfdMala, "malá"},
		{"decomposed mixed", nfdMalut, "malëuţřait"},
		{"decomposed dot below", nfdZdot, "ẓ"},
		{"capital", "Švel", "švel"},
		{"decomposed capital", nfdSvelUp, "švel"},
		{"capital with acute", "MALÁ", "malá"},
		{"empty", "", ""},
	}
	for _, c := range cases {
		if got := Normalize(c.in); got != c.want {
			t.Errorf("%s: Normalize(%q) = %q, want %q", c.name, c.in, got, c.want)
		}
	}
}

// Normalize must be idempotent: normalizing already-normal text is a
// no-op, which is what lets the parse entry points call it freely.
func TestNormalize_Idempotent(t *testing.T) {
	for _, in := range []string{"švel", "Švel", nfdSvel, nfdMalut, ""} {
		once := Normalize(in)
		if twice := Normalize(once); twice != once {
			t.Errorf("Normalize(%q) not idempotent: %q then %q", in, once, twice)
		}
	}
}

// TestNormalize_Variants covers the look-alike spellings. Almost
// nothing types a plain apostrophe for the glottal stop: keyboards and
// chat clients rewrite it, so the parser has to accept what they emit.
func TestNormalize_Variants(t *testing.T) {
	for _, c := range []struct{ in, want string }{
		{"wala’na", "wala'na"},           // ’ right single quotation mark
		{"wala‘na", "wala'na"},           // ‘ left single quotation mark
		{"walaʼna", "wala'na"},           // ʼ modifier letter apostrophe
		{"wala'na", "wala'na"},           // already plain
		{"hlamëuțřaitä", "hlamëuţřaitä"}, // ț t-comma to ţ t-cedilla
		{"HlamëuȚřaitä", "hlamëuţřaitä"}, // folded after lowercasing
	} {
		if got := Normalize(c.in); got != c.want {
			t.Errorf("Normalize(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

// TestNormalize_LeavesPreV4Letters pins the other half of the rule.
// The pre-v4 alphabet had letters v4 dropped, and a word using one is
// not v4 text. Folding it to something that parses would turn a word
// we should reject into a wrong answer.
//
// The grave ì used to be listed here. It is v4's own: §1.3.1 puts it on
// the unstressed -i- of a -Cìa- conjunct, and gives karésìa and vélkìo
// as the examples. See the comment on variants.
func TestNormalize_LeavesPreV4Letters(t *testing.T) {
	for _, w := range []string{"ëıtfoıgyaölw", "iţkuîl"} {
		if got := Normalize(w); got != w {
			t.Errorf("Normalize(%q) = %q, want it left alone", w, got)
		}
	}
}

// §1.3 lets ţ be written ṭ or ŧ, ḑ as ḍ or đ, ň as ņ or ṇ, ř as ŗ or ṛ,
// and ļ as ł or ḷ. The phonotactics document additionally writes every
// ẓ as ż, although §1.3 grants ẓ no alternate at all.
func TestNormalize_FoldsSanctionedAlternates(t *testing.T) {
	for _, c := range []struct{ in, want string }{
		{"ṭ", "ţ"}, {"ŧ", "ţ"},
		{"ḍ", "ḑ"}, {"đ", "ḑ"},
		{"ņ", "ň"}, {"ṇ", "ň"},
		{"ŗ", "ř"}, {"ṛ", "ř"},
		{"ł", "ļ"}, {"ḷ", "ļ"},
		{"ż", "ẓ"},
		{"đa", "ḑa"}, // folded mid-word, not only alone
	} {
		if got := Normalize(c.in); got != c.want {
			t.Errorf("Normalize(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}
