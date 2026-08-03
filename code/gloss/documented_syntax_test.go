package gloss_test

import (
	"github.com/christian-oudard/ithkuil/gloss"
	"testing"

	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/store"
)

// SPEC.md and README.md document the gloss syntax by example, one line
// per punctuation mark. Those examples are the first thing a reader
// copies, so they have to be real input rather than plausible-looking
// input.
//
// They were not, once: the table illustrated "." with "ASR.RPT", and
// RPT is not a Validation. Nothing checked it, because prose is not
// compiled. This is that check.
//
// Keep it in step with the table in SPEC.md § Gloss punctuation.
func TestDocumentedSyntaxExamples(t *testing.T) {
	st, err := store.Open(store.DefaultPath())
	if err != nil {
		t.Skip("no data store; run tools/build_db.py")
	}
	lex, err := lexicon.LoadFromStore(st)
	if err != nil {
		t.Fatal(err)
	}

	// Formative-level examples, each wrapped on the "ml" root where the
	// table shows a bare slot.
	for _, in := range []string{
		"S2.CPT-ml-ERG",     // "-" separates slots
		"ml-DYN.OBJ.FNC",    // "." joins category values
		"ml-MSS.G",          // "." in a Ca complex
		"ml-RCP.HYP",        // "." in Slot VIII
		"ml-ASR.RPR",        // "." on an Assertive Vk
		"ml-DEV/3",          // "/" binds a degree
		"ml-ACC/INS",        // "/" binds a case, §3.9.2
		"ml-(1m)/AFF",       // "/" binds a case, §4.6.5
		"ml-(1m/BEN)/3",     // "/" binds an effect, then a degree
		"ml-t/1_2",          // "_" trails the affix Type
		"ml-IAC/PRP_3",      // "_" on an accessor
		"ml-Ca:MSS.G",       // ":" tags a stacked Ca
		"ml-Ca:{Ca}",        // ":" with the structural body
		"ml-(1m+2p/BEN)/3",  // "()" and "+"
		"(CTR)/1",           // "()" around a Cs root
		"ml-DEV/3-{Ca}-t/1", // "{}" as the Slot V/VII boundary
	} {
		if _, err := gloss.ParseFormative(in, lex.Affixes); err != nil {
			t.Errorf("documented example %q does not compose: %v", in, err)
		}
	}

	// Word-level examples, which go through the token dispatcher.
	for _, in := range []string{
		"[QUO]",            // "[]" around a carrier head
		"[1m+2p]-ERG",      // "[]" around a multi-referent head
		"RCP.HYP-{parent}", // "{}" around a scope marker
		"DSV_END",          // "_" as a word-level modifier
		"NOM:1m-ERG",       // ":" tags a §4.6 referent category
		"1m/BEN-ERG",       // "/" binds an effect to a referent
		"1m-THM-[2m]/IND",  // "/" binds the second referent's own case
		"1m-THM-ERG",       // a stacked second case, which binds to nothing
		`"John"`,           // `""` quotes non-Ithkuil text
	} {
		if _, err := gloss.ParseWord(in, lex); err != nil {
			t.Errorf("documented example %q does not parse: %v", in, err)
		}
	}

	// Span-level examples, where the mark is the space between tokens.
	// ParseWord cannot read these by construction, which is the point.
	for _, in := range []struct {
		gloss string
		words int
	}{
		{`[CAR] "John"`, 2}, // " " separates words
		{"T1-ml ml", 1},     // ...except inside a chain, which rejoins
	} {
		got, err := gloss.ParseText(in.gloss, lex)
		if err != nil {
			t.Errorf("documented example %q does not parse: %v", in.gloss, err)
			continue
		}
		if len(got) != in.words {
			t.Errorf("documented example %q is %d words, want %d",
				in.gloss, len(got), in.words)
		}
	}
}
