package layout

import "testing"

// Each line in formativeCorpus is a word that successfully parses to a
// Formative via fullparse. Layer C's Parse/Render pair must round-trip
// every one — Layer C does no grammar decoding, so any word that
// fullparse accepts is a word that layout.Parse must accept too.
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
