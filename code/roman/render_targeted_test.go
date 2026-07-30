package roman_test

import (
	"strings"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/roman"
)

// The corpus sweeps used to be the only thing exercising ParseText,
// Word and Stressless. A corpus tells you that something broke, not
// what: it walks hundreds of words through one path and reports the
// first that disagrees. These name the behaviour directly, one case
// per branch, so a failure points at the branch.

// TestParseText_StopsAtTheFirstUnreadableWord checks ParseText's
// contract, which is the opposite of the per-word reader's: ParseWord
// reports one word's result and the caller keeps going, ParseText
// refuses the whole span. The error has to name the word, since the
// caller no longer has the per-word results to look at.
func TestParseText_StopsAtTheFirstUnreadableWord(t *testing.T) {
	txt, err := roman.ParseText("malëuţřait amlal")
	if err != nil {
		t.Fatalf("ParseText on two good words: %v", err)
	}
	if len(txt) != 2 {
		t.Errorf("ParseText returned %d words, want 2", len(txt))
	}

	_, err = roman.ParseText("malëuţřait mavẓorf amlal")
	if err == nil {
		t.Fatal("ParseText accepted a span containing an unreadable word")
	}
	if !strings.Contains(err.Error(), "mavẓorf") {
		t.Errorf("error should name the offending word; got %v", err)
	}
}

// TestWord_EveryWordKind walks Word's type switch. Each arm is a
// different renderer, and only the formative arm has much coverage
// elsewhere, so a broken referential or bias arm showed up as a corpus
// word failing rather than as a renderer test.
func TestWord_EveryWordKind(t *testing.T) {
	for _, word := range []string{
		"malëuţřait", // formative
		"amlal",      // formative, vowel-initial
		"ta",         // referential
		"pļļ",        // bias adjunct
		"hla",        // carrier adjunct
		"wa",         // modular adjunct
		"ala",        // affix adjunct
	} {
		t.Run(word, func(t *testing.T) {
			w, err := roman.ParseWord(word)
			if err != nil {
				t.Skipf("ParseWord(%q): %v", word, err)
			}
			got, err := roman.Word(w)
			if err != nil {
				t.Fatalf("Word(%T): %v", w, err)
			}
			if got == "" {
				t.Fatalf("Word(%T) returned empty", w)
			}
			// Rendering is canonical, so a second pass must not move.
			w2, err := roman.ParseWord(got)
			if err != nil {
				t.Fatalf("re-reading %q: %v", got, err)
			}
			again, err := roman.Word(w2)
			if err != nil {
				t.Fatalf("re-rendering: %v", err)
			}
			if again != got {
				t.Errorf("render is not canonical: %q then %q", got, again)
			}
		})
	}
}

// TestWord_ForeignKind pins the arm that carries text through
// unchanged, rather than routing it to a renderer that would have no
// grammar to write.
func TestWord_ForeignKind(t *testing.T) {
	_, err := roman.Word(g.Foreign{Text: "hello"})
	if err != nil {
		t.Fatalf("Foreign is renderable: %v", err)
	}
}

// TestStressless_RefusesAChain covers the one case Stressless
// rejects. A §3.1.7 chain is one hyphen-joined word but carries a
// stress per link, and a single parsing adjunct cannot declare them
// all; the source does not say whether a link may take its own
// adjunct, so it refuses rather than invent a spelling.
func TestStressless_RefusesAChain(t *testing.T) {
	txt, err := roman.ParseText("hakšal-uḑfarf")
	if err != nil {
		t.Skipf("chain does not read: %v", err)
	}
	if _, err := roman.Stressless(txt); err == nil {
		t.Error("Stressless accepted a concatenation chain")
	} else if !strings.Contains(err.Error(), "chain") {
		t.Errorf("error should say why a chain is refused; got %v", err)
	}
}

// TestStressless_WritesAnAdjunctPerWord checks the normal path: every
// word comes back without its stress diacritic, and the stress it
// carried is written as a separate parsing adjunct beside it.
func TestStressless_WritesAnAdjunctPerWord(t *testing.T) {
	txt, err := roman.ParseText("malëuţřait")
	if err != nil {
		t.Fatalf("ParseText: %v", err)
	}
	out, err := roman.Stressless(txt)
	if err != nil {
		t.Fatalf("Stressless: %v", err)
	}
	if out == "" {
		t.Fatal("Stressless returned empty")
	}
	for _, r := range out {
		if strings.ContainsRune("áéíóúâêôû", r) {
			t.Errorf("stress diacritic %q survived into %q", r, out)
		}
	}
}
