package slots

import "testing"

// Each line in formativeCorpus is a word that successfully parses to a
// Formative via fullparse. Layer C's Parse/Render pair must round-trip
// every one — Layer C does no grammar decoding, so any word that
// fullparse accepts is a word that slots.Parse must accept too.
var formativeCorpus = []string{
	// Canonical and minimal.
	"malëuţřait",
	"amlal",
	"amlala",
	"amlalú",
	"emlölo",
	"malal",
	// Concat prefixes.
	"hamlala",
	"hwamlala",
	// Shortcut forms.
	"waml",
	"yuml",
	"waiml",
	"wamlar",
	"hlaml",
	// Cs-root and reference-root.
	"ëilal",
	"ëilael",
	"oërmölá",
	"oërmoulá",
	"ealali",
	"aelali",
	// Slot V (multiple affixes between Vv and Ca).
	"amlalahla",
	"amlalahlá",
	"amlalara",
	"amlali'a",
	"ärmaläwi'a",
	// §3.8.1.2 Cn→Ca shortcut: a Pattern-1 Cn (hl/hr/hm/hn/hň) in the
	// Ca slot, eliding default -l- Ca and default -a- Vn.
	"amlahla",
	"amlahra",
	"amlahma",
	"amlahna",
	"amlahňa",
	// Sentence-start prefixes. The çç-form normalizes to çëy-,
	// so it's not round-trippable — only canonical forms are.
	"çamlala",
	"çëmlala",
}

func TestLayout_RoundTrip(t *testing.T) {
	for _, w := range formativeCorpus {
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
