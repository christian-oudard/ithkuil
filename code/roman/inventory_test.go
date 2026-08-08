package roman_test

import (
	"fmt"
	"reflect"
	"strings"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/inventory"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/roman"
)

// The romanization arm is supposed to handle the whole grammar, and no
// corpus can show that: attested text uses what people happen to say,
// so a category nobody writes is untested however large the corpus
// grows. These three sweeps drive the inventory instead. See package
// inventory for how "every value" is kept honest.

// TestInventory_RoundTrips writes each value out and reads it back,
// and requires the grammar that comes back to be the grammar that went
// in. Comparing values rather than romanizations is the point: a
// renderer that silently drops a marked value produces a perfectly
// legal word, and only the comparison at the far end notices.
func TestInventory_RoundTrips(t *testing.T) {
	for _, s := range inventory.Samples() {
		out, err := roman.Word(s.Word)
		if err != nil {
			t.Errorf("%s/%s: render: %v", s.Category, s.Abbrev, err)
			continue
		}
		if s.Unwritten {
			if out != "" {
				t.Errorf("%s/%s writes no word, but rendered %q", s.Category, s.Abbrev, out)
			}
			continue
		}
		if out == "" {
			t.Errorf("%s/%s rendered nothing", s.Category, s.Abbrev)
			continue
		}
		back, err := roman.ParseWord(out)
		if err != nil {
			t.Errorf("%s/%s -> %q: parse: %v", s.Category, s.Abbrev, out, err)
			continue
		}
		if !reflect.DeepEqual(back, s.Word) {
			t.Errorf("%s/%s -> %q came back changed\n  sent %s\n  got  %s",
				s.Category, s.Abbrev, out, describe(s.Word), describe(back))
		}
	}
}

// describe prints a word for a failure message. The word classes that
// are a bare enum — bias, register — print as their abbreviation, since
// %#v on those gives an integer and "sent 0, got 1" says nothing.
func describe(w g.Word) string {
	if s, ok := w.(fmt.Stringer); ok {
		return s.String()
	}
	return fmt.Sprintf("%#v", w)
}

// TestInventory_DistinctWithinCategory requires the values of one
// category to be written differently from each other. The round trip
// alone does not catch a table that maps two values to one form, since
// whichever the parser picks first will satisfy the sample for that
// one and only the other will look wrong; here both are named.
//
// Across categories the same word is expected. Every category has an
// unmarked default, and a word at every default is the bare formative,
// so THM, S1, PRC and the rest all render to the same thing. That is
// what unmarked means, and it is TestInventory_RoundTrips that shows
// the value survives anyway.
func TestInventory_DistinctWithinCategory(t *testing.T) {
	seen := map[string]map[string]string{} // category -> word -> abbrev
	for _, s := range inventory.Samples() {
		if s.Unwritten {
			continue
		}
		out, err := roman.Word(s.Word)
		if err != nil {
			continue // TestInventory_RoundTrips reports this.
		}
		if seen[s.Category] == nil {
			seen[s.Category] = map[string]string{}
		}
		if prev, ok := seen[s.Category][out]; ok {
			t.Errorf("%s: %s and %s are both written %q", s.Category, prev, s.Abbrev, out)
			continue
		}
		seen[s.Category][out] = s.Abbrev
	}
}

// TestInventory_PairsAreDistinct is the same property as
// DistinctWithinCategory over a much larger space: the romanization has
// to be injective, so two formatives that are written the same way must
// be the same grammar.
//
// Varying one value at a time cannot see a collision that needs two
// slots filled to appear, and the Slot V against Slot VII collapse was
// one of those. Comparing the grammar rather than the labels is what
// lets the sweep run over combinations at all, since many combinations
// name different coordinates and mean the same formative — Case/THM
// with Stem/S1 is the bare baseline, and so is every other pair of
// defaults.
func TestInventory_PairsAreDistinct(t *testing.T) {
	seen := map[string]inventory.Combination{}
	var unwritable int
	for _, c := range inventory.Pairs() {
		out, err := roman.Word(c.Word)
		if err != nil {
			unwritable++
			continue
		}
		prev, ok := seen[out]
		if !ok {
			seen[out] = c
			continue
		}
		if !reflect.DeepEqual(prev.Word, c.Word) {
			t.Errorf("%q is written by two different formatives\n  %s + %s\n  %s + %s",
				out, prev.A, prev.B, c.A, c.B)
		}
	}
	if len(seen) == 0 {
		t.Fatal("no combination was written; the test is not exercising anything")
	}
	t.Logf("%d distinct words, %d combinations with no written form", len(seen), unwritable)
}

// TestInventory_RendersPronounceableWords requires what we write to be
// a word by our own phonotactics. A round trip cannot see this: a form
// both arms mishandle the same way still comes back equal, and it took
// writing every value out to find that the §4.4 carrier-end adjunct was
// spelled hüi, where the PDF has hü and §1.2's ten diphthongs do not
// include üi.
//
// The adjunct classes are held to the vowel rules but not the cluster
// rules, which is the same line phonology.ParseWord draws and for the
// same reason: §2 is written about a conjunct with a vowel-form beside
// it, and the adjunct tables are authoritative over shapes it never
// contemplated. §4.5's own worked examples begin hňa, which the
// word-initial table rejects. Exempting those adjuncts wholesale is
// what would have let hüi stand, so the exemption stops at the rules it
// was argued for.
func TestInventory_RendersPronounceableWords(t *testing.T) {
	clusterRules := map[string]bool{"Bias": true, "Register": true, "CarrierType": true}
	var checked int
	for _, s := range inventory.Samples() {
		if s.Unwritten {
			continue
		}
		out, err := roman.Word(s.Word)
		if err != nil || out == "" {
			continue // TestInventory_RoundTrips reports this.
		}
		checked++
		w, err := phonology.ParseWord(out)
		if err != nil {
			t.Errorf("%s/%s renders %q, which is not readable: %v", s.Category, s.Abbrev, out, err)
			continue
		}
		for _, v := range w.Violations() {
			if clusterRules[s.Category] && (strings.HasPrefix(v.Code, "phonotactics §2.") || strings.HasPrefix(v.Code, "phonotactics §3.")) {
				continue
			}
			t.Errorf("%s/%s renders %q, which breaks %v", s.Category, s.Abbrev, out, v)
		}
	}
	if checked == 0 {
		t.Fatal("no sample was checked; the test is not exercising anything")
	}
}

// TestText_RoundTripsASpan covers the whole-span pair the package doc
// advertises, ParseText and Text, which nothing else in the tree calls:
// the CLI and the MCP server both want the per-word report Tokenize
// gives instead. An arm that is documented as a pair and used by nobody
// is the arm that rots, and Text had no test at all.
func TestText_RoundTripsASpan(t *testing.T) {
	const sentence = "hi malëuţřait a mala"
	span, err := roman.ParseText(sentence)
	if err != nil {
		t.Fatalf("ParseText(%q): %v", sentence, err)
	}
	if len(span) != 4 {
		t.Fatalf("read %d words from %q, want 4", len(span), sentence)
	}
	out, err := roman.Text(span)
	if err != nil {
		t.Fatalf("Text: %v", err)
	}
	if out != sentence {
		t.Errorf("span round-tripped to %q, want %q", out, sentence)
	}
}

// TestText_ReportsTheWordThatFailed pins what a span does with a word
// that cannot be written. Text stops at the first failure rather than
// returning a partial sentence, since half a span read back as whole is
// worse than an error.
func TestText_ReportsTheWordThatFailed(t *testing.T) {
	span := g.Text{
		g.MinimalFormative("ml"),
		g.ModularAdjunct{}, // §4.3 Slot 4 is mandatory; this cannot be written
	}
	if _, err := roman.Text(span); err == nil {
		t.Fatal("a span holding an unwritable word should not render")
	}
}

// TestConcat_IsNotAWordAlone crosses concatenation with the slots the
// inventory sweep varies one at a time, which is where this hid: every
// value round-tripped on its own, and no sample carried a Cc marker.
//
// §3.1.7 writes a chain as its members joined by hyphens, so a Cc
// marker says "another formative follows" and a formative carrying one
// is half a word. Rendering it anyway produced a string no reader
// takes: "hafçal" comes back as an affixual adjunct missing its scope
// consonant, because a lone h- is not how any word begins.
func TestConcat_IsNotAWordAlone(t *testing.T) {
	for _, concat := range []g.ConcatenationStatus{g.Type1, g.Type2} {
		for _, withSlotV := range []bool{false, true} {
			f := g.MinimalFormative("fç")
			f.Concat = concat
			if withSlotV {
				f.SlotV = []g.Affix{{Consonant: "x", Type: g.Type1Affix, Degree: 4}}
			}
			out, err := roman.Word(f)
			if err == nil {
				t.Errorf("%v with slotV=%v wrote %q, which no reader accepts",
					concat, withSlotV, out)
			}
		}
	}
}

// TestChain_RoundTrips is the other half: the same members inside a
// chain are a word, and must survive the trip.
func TestChain_RoundTrips(t *testing.T) {
	dep := g.MinimalFormative("fç")
	dep.SlotV = []g.Affix{{Consonant: "x", Type: g.Type1Affix, Degree: 4}}
	for _, add := range []func(*g.Chain, g.Formative) *g.Chain{
		(*g.Chain).AddType1, (*g.Chain).AddType2,
	} {
		chain := add(g.NewChain(g.MinimalFormative("ml")), dep)
		out, err := roman.Word(chain)
		if err != nil {
			t.Errorf("chain: render: %v", err)
			continue
		}
		back, err := roman.ParseWord(out)
		if err != nil {
			t.Errorf("chain -> %q: parse: %v", out, err)
			continue
		}
		got, ok := back.(*g.Chain)
		if !ok {
			t.Errorf("%q came back as %T, want a chain", out, back)
			continue
		}
		if len(got.Formatives()) != 2 {
			t.Errorf("%q came back with %d members, want 2", out, len(got.Formatives()))
		}
	}
}
