package slots

import "testing"

// FormativeCorpus is defined in corpus.go so the same test set can
// be consumed by higher-layer tests (fullparse, compose) that need
// to exercise their round-trip invariants on the same well-known
// words.

func TestLayout_RoundTrip(t *testing.T) {
	for _, w := range FormativeCorpus {
		l, err := Parse(w)
		if err != nil {
			t.Errorf("Parse(%q): %v", w, err)
			continue
		}
		got := Render(l)
		if got != w {
			t.Errorf("Render(Parse(%q)) = %q", w, got)
		}
	}
}

// TestLayout_SentencePrefix_csFamily covers §5.8.8: the modern cs-
// family of sentence-juncture prefixes (cs-/cse-/csw-/cscs-) parses
// to the same SentenceStarter flag as the ç(ë)-family from §3.2 / §1.3.2.
// Render only emits the ç-family on output; round-trip on the cs-
// inputs goes to the canonical ç-form.
func TestLayout_SentencePrefix_csFamily(t *testing.T) {
	cases := []struct {
		in       string
		wantBody string // body after the prefix strips (re-encoded if needed)
	}{
		{"csalal", "alal"},          // cs- before vowel
		{"cseamlala", "amlala"},     // cse- before consonant body
		{"cswamlala", "wamlala"},    // csw- = cs- + w-Cc shortcut
		{"cscsalal", "yalal"},       // cscs- = cs- + y-Cc shortcut
	}
	for _, c := range cases {
		body, starter := stripSentencePrefix(c.in)
		if !starter {
			t.Errorf("stripSentencePrefix(%q): SentenceStarter = false, want true", c.in)
		}
		if body != c.wantBody {
			t.Errorf("stripSentencePrefix(%q) body = %q, want %q", c.in, body, c.wantBody)
		}
	}
	// Bare cs- before a consonant other than w must NOT be treated as
	// a sentence prefix per §5.8.8 (cse- is required there).
	body, starter := stripSentencePrefix("csmalal")
	if starter || body != "csmalal" {
		t.Errorf("stripSentencePrefix(%q) = (%q, %v), want unchanged",
			"csmalal", body, starter)
	}
}
