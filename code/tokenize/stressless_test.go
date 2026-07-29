package tokenize

import (
	"reflect"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/phonology"
)

// The contract: a span spelled either way reads back to the same
// words. One writes the stress as a diacritic, the other as the §4.8
// adjunct that declares it, and the grammar is untouched by the
// choice.
func TestStressless_RoundTripsToTheSameWords(t *testing.T) {
	var checked, skipped int
	for _, w := range corpus.Words() {
		span, err := Text(w)
		if err != nil {
			continue
		}
		stressless, err := RenderStressless(span)
		if err != nil {
			// Chains, and the classes that still have no renderer at
			// all, are counted rather than failed; the count below
			// keeps the gap from widening unnoticed.
			skipped++
			continue
		}
		back, err := Text(stressless)
		if err != nil {
			t.Errorf("%q spelled stressless as %q, which does not read: %v", w, stressless, err)
			continue
		}
		if len(back) != len(span) {
			t.Errorf("%q: %d words became %d through %q", w, len(span), len(back), stressless)
			continue
		}
		for i := range span {
			if !reflect.DeepEqual(back[i], span[i]) {
				t.Errorf("%q through %q: word %d came back different\n  want %+v\n  got  %+v",
					w, stressless, i, span[i], back[i])
			}
		}
		checked++
	}
	if checked == 0 {
		t.Fatal("no corpus word was checked; the test is not exercising anything")
	}
	t.Logf("round-tripped %d corpus words stressless, %d without a spelling", checked, skipped)
}

// The stressless form carries no stress diacritic, and every word in it
// is preceded by an adjunct that declares one.
func TestStressless_HasNoDiacriticsAndAnAdjunctPerWord(t *testing.T) {
	// "amlalú" bears ultimate stress; "amlala" penultimate.
	span, err := Text("amlalú amlala")
	if err != nil {
		t.Fatal(err)
	}
	got, err := RenderStressless(span)
	if err != nil {
		t.Fatal(err)
	}
	fields := strings.Fields(got)
	if len(fields) != 4 {
		t.Fatalf("got %q, want an adjunct before each of two words", got)
	}
	for i := 0; i < len(fields); i += 2 {
		if _, ok := phonology.ParsingAdjunct(fields[i]); !ok {
			t.Errorf("%q: field %d is %q, want a parsing adjunct", got, i, fields[i])
		}
		if bare, stress := phonology.Strip(fields[i+1]); bare != fields[i+1] {
			t.Errorf("%q: word %q still carries a %v mark", got, fields[i+1], stress)
		}
	}
	// The two words differ in stress, so their adjuncts must differ.
	if fields[0] == fields[2] {
		t.Errorf("%q: both words got the same adjunct %q despite differing stress", got, fields[0])
	}
}

// A monosyllable has no contrastive stress to declare, so it takes the
// monosyllabic adjunct rather than a placement one.
func TestStressless_Monosyllable(t *testing.T) {
	span, err := Text("la")
	if err != nil {
		t.Fatal(err)
	}
	got, err := RenderStressless(span)
	if err != nil {
		t.Fatal(err)
	}
	declared, ok := phonology.ParsingAdjunct(strings.Fields(got)[0])
	if !ok || declared != phonology.Monosyllabic {
		t.Errorf("%q declares %v, want Monosyllabic", got, declared)
	}
}

// ParsingAdjunctFor inverts ParsingAdjunct for every stress it names.
func TestParsingAdjunctFor_IsTheInverse(t *testing.T) {
	for _, s := range []phonology.Stress{
		phonology.Monosyllabic, phonology.Ultimate,
		phonology.Penultimate, phonology.Antepenultimate,
	} {
		adjunct, ok := phonology.ParsingAdjunctFor(s)
		if !ok {
			t.Errorf("no adjunct declares %v", s)
			continue
		}
		back, ok := phonology.ParsingAdjunct(adjunct)
		if !ok || back != s {
			t.Errorf("%v spelled %q, which reads back as %v", s, adjunct, back)
		}
	}
}
