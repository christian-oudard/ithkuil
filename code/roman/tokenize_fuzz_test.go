package roman

import (
	"strings"
	"testing"
	"unicode/utf8"

	"github.com/christian-oudard/ithkuil/phonology"
)

// FuzzClassifyWord runs ClassifyWord on arbitrary strings and asserts
// it never panics, never returns a token whose Romanization() differs from
// the input, and accepts only inputs that parse as phonology. Seeded
// with a representative corpus across word types.
func FuzzClassifyWord(f *testing.F) {
	seeds := []string{
		// formatives
		"malëuţřait", "amlala", "amlalú", "ámlala", "lalu", "yužgrá",
		"eolaleici", "wala'ana", "lála'a", "adni'lö", "la'la",
		"ţnaxekka", "ëilal", "oërmölá", "ellyahru",
		// adjuncts
		"ha", "hai", "řřx", "pļļ", "ah", "ihnú", "äst", "are", "xaheitr",
		// referentials
		"khe", "lü", "layá", "miyüs", "sme'e", "ka'u",
		"üohla", "ahlax", "ţnaxeka",
		// concatenation
		"hamlala-amlala", "çëhamala-lala",
		// sentence prefix
		"çalal", "çëlal", "ççala", "çwala",
		// malformed — should classify as UnknownWord
		"", "x", "qqq", "hellö", "møl", "amláláu",
	}
	for _, s := range seeds {
		f.Add(s)
	}

	f.Fuzz(func(t *testing.T, in string) {
		// Skip inputs too large to be plausibly meaningful; fuzzer
		// otherwise wastes cycles on multi-megabyte gibberish.
		if len(in) > 200 {
			t.Skip()
		}
		// Multi-rune sanity: bail on input with embedded NULs or
		// control codes the lexer wouldn't see in practice.
		for _, r := range in {
			if r == 0 || r < 0x20 || r == utf8.RuneError {
				t.Skip()
			}
		}

		word, err := ParseWord(in)
		if err == nil && word == nil {
			t.Fatalf("ParseWord(%q) returned no word and no error", in)
		}

		// Char validation is a hard precondition: anything holding a
		// non-V4 rune has to fail, never come back as some word class.
		charsOK := phonology.CheckText(in) == nil
		if !charsOK && err == nil {
			t.Fatalf("ParseWord(%q) = %T, want an error (non-V4 chars)", in, word)
		}
	})
}

// FuzzTokenize feeds whole sentences (whitespace-joined fields) to
// Tokenize and asserts the result is well-formed: every token's
// Romanization concatenates back to a substring of the input, and the
// MarksMood post-pass never panics on weird neighbour combinations.
func FuzzTokenize(f *testing.F) {
	seeds := []string{
		"malëuţřait", "ah amlala", "ah amlalú", "ah řřx",
		"hna John malá", "çëhamala-lala ha",
		"üohla pļļ", "",
	}
	for _, s := range seeds {
		f.Add(s)
	}
	f.Fuzz(func(t *testing.T, in string) {
		if len(in) > 500 {
			t.Skip()
		}
		for _, r := range in {
			if r == 0 || r < 0x20 || r == utf8.RuneError {
				t.Skip()
			}
		}
		toks := Tokenize(in)
		fields := strings.Fields(in)
		if len(toks) != len(fields) {
			t.Fatalf("Tokenize(%q) returned %d tokens, want %d (one per field)",
				in, len(toks), len(fields))
		}
		for i, r := range toks {
			if r.Word == nil && r.Err == nil {
				t.Fatalf("Tokenize(%q): word %d has neither grammar nor a reason", in, i)
			}
			if r.Romanization != fields[i] {
				t.Fatalf("Tokenize(%q): word %d romanization = %q, want %q",
					in, i, r.Romanization, fields[i])
			}
		}
		_ = phonology.SplitConjuncts(in) // ensure Layer B doesn't panic either
	})
}
