package roman_test

import (
	"strings"
	"testing"
	"unicode/utf8"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/roman"
)

func endsInConsonant(w string) bool {
	bare, _ := phonology.Strip(phonology.Normalize(w))
	r, _ := utf8.DecodeLastRuneInString(bare)
	return !phonology.IsVowel(r)
}

// The §1.5 junction rule, measured on Quijada's own sentences: read
// each one, write it back, and count the words left ending in a
// consonant with another word after them.
//
// Before Text chose in context this stood at 62 of 386 junctions, all
// of them a Slot IX default that had been elided with no regard for
// what followed. It is now 4, and those 4 are not ours to fix: they are
// Siryus and Deneb, §7 foreign names whose letters are their meaning,
// and Quijada leaves them consonant-final too.
//
// Pinned in both directions. Rising means the junction rule stopped
// applying somewhere; falling below 4 means a foreign name got
// rewritten, which would be a worse bug than the one this guards.
func TestSpanFillsEveryJunctionItCan(t *testing.T) {
	const wantConsonantFinal = 4
	const wantJunctions = 386

	got, junctions := 0, 0
	for _, ex := range corpus.Examples() {
		span, err := roman.ParseText(ex.Ithkuil)
		if err != nil {
			continue
		}
		out, err := roman.Text(span)
		if err != nil {
			continue
		}
		words := strings.Fields(out)
		if len(words) != len(strings.Fields(ex.Ithkuil)) {
			continue
		}
		for i := 0; i < len(words)-1; i++ {
			junctions++
			if endsInConsonant(words[i]) {
				got++
			}
		}
	}
	if junctions != wantJunctions {
		t.Errorf("swept %d junctions, want %d", junctions, wantJunctions)
	}
	if got != wantConsonantFinal {
		t.Errorf("%d junctions left consonant-final, want %d", got, wantConsonantFinal)
	}
}

// No word we write should end in a bare -h, in any position. §4.1
// permits it and §3.8.1.2 generates it, but it is barely audible and
// Quijada never writes one; see phonology.EndsInBareH. Where a word has
// another spelling, Text takes it.
func TestSpanNeverWritesABareFinalH(t *testing.T) {
	for _, ex := range corpus.Examples() {
		span, err := roman.ParseText(ex.Ithkuil)
		if err != nil {
			continue
		}
		out, err := roman.Text(span)
		if err != nil {
			continue
		}
		for _, w := range strings.Fields(out) {
			if phonology.EndsInBareH(w) {
				t.Errorf("%q ends in a bare -h, in %q", w, out)
			}
		}
	}
}

// Text is the only thing that chooses in context. Word keeps writing
// the shortest spelling, because a word quoted on its own has no
// junction to repair and the dictionary form is the one wanted.
func TestWordIsUnchangedByTheSpanPass(t *testing.T) {
	for _, w := range []string{"malëuţřait", "mal", "kši'la", "wam"} {
		parsed, err := roman.ParseWord(w)
		if err != nil {
			t.Fatalf("ParseWord(%q): %v", w, err)
		}
		alone, err := roman.Word(parsed)
		if err != nil {
			t.Fatalf("Word(%q): %v", w, err)
		}
		cands, err := roman.Spellings(parsed)
		if err != nil {
			t.Fatalf("Spellings(%q): %v", w, err)
		}
		if cands[0] != alone {
			t.Errorf("%q: Spellings starts %q but Word gives %q", w, cands[0], alone)
		}
	}
}

// A junction inside a §3.1.7 chain is a junction. §3.1.8 calls the
// hyphen "a simple mnemonic indicator", so nothing is paused there and
// §1.5 reads across a link boundary as it reads across a space.
func TestSpanFillsAcrossAChainLink(t *testing.T) {
	span, err := roman.ParseText("hlamalëuţřait-malá")
	if err != nil {
		t.Skipf("chain did not parse: %v", err)
	}
	out, err := roman.Text(span)
	if err != nil {
		t.Fatalf("Text: %v", err)
	}
	links := strings.Split(out, "-")
	if len(links) != 2 {
		t.Fatalf("wrote %q, want two links", out)
	}
	if endsInConsonant(links[0]) {
		t.Errorf("first link %q ends in a consonant before %q", links[0], links[1])
	}
}
