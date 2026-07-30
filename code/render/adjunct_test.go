package render

import (
	"reflect"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
)

// The three adjunct classes are tested by round-trip, because the parse
// side is the older half and each renderer here is written as its
// inverse. The words are §4.1.1's, §4.1.2's and §4.3's own printed
// examples, so what round-trips is what Quijada wrote.

func TestSingleAffixAdjunct_RoundTrip(t *testing.T) {
	// §4.1.1's examples. "aull" is the Vx-Cs-Vs form; the rest elide Vs.
	for _, word := range []string{"ač", "iakse", "aull"} {
		sa, err := parse.ParseSingleAffix(word)
		if err != nil {
			t.Fatalf("ParseSingleAffix(%q): %v", word, err)
		}
		got, err := SingleAffixAdjunct(sa)
		if err != nil {
			t.Errorf("SingleAffixAdjunct(%+v): %v", sa, err)
			continue
		}
		if got != word {
			t.Errorf("round trip %q -> %q", word, got)
		}
	}
}

// TestSingleAffixAdjunct_DefaultScopeElided pins the one canonical
// choice this renderer makes: §4.1.1 parenthesises the default "(a)",
// so it is not written and the word is two conjuncts.
func TestSingleAffixAdjunct_DefaultScopeElided(t *testing.T) {
	sa, err := parse.ParseSingleAffix("ača")
	if err != nil {
		t.Fatal(err)
	}
	if sa.Scope != g.ScopeVDom {
		t.Fatalf("expected the default scope, got %v", sa.Scope)
	}
	got, err := SingleAffixAdjunct(sa)
	if err != nil {
		t.Fatal(err)
	}
	if got != "ač" {
		t.Errorf("default scope wrote %q, want \"ač\" with no Vs", got)
	}
}

func TestMultipleAffixAdjunct_RoundTrip(t *testing.T) {
	for _, word := range []string{"dohast", "xaheitr", "xaheitre", "xa'heitr"} {
		ma, err := parse.ParseMultipleAffix(word)
		if err != nil {
			t.Fatalf("ParseMultipleAffix(%q): %v", word, err)
		}
		got, err := MultipleAffixAdjunct(ma)
		if err != nil {
			t.Errorf("MultipleAffixAdjunct(%+v): %v", ma, err)
			continue
		}
		if got != word {
			t.Errorf("round trip %q -> %q", word, got)
		}
	}
}

// TestMultipleAffixAdjunct_SameScopeElidesVz checks §4.1.2's "(ai)":
// when the trailing affixes share the first's scope, saying so again is
// optional and the shorter form omits Vz.
func TestMultipleAffixAdjunct_SameScopeElidesVz(t *testing.T) {
	ma, err := parse.ParseMultipleAffix("xaheitrai")
	if err != nil {
		t.Fatal(err)
	}
	if ma.RestScope != ma.FirstScope {
		t.Fatalf("expected \"ai\" to copy the Cz scope, got %v and %v", ma.RestScope, ma.FirstScope)
	}
	got, err := MultipleAffixAdjunct(ma)
	if err != nil {
		t.Fatal(err)
	}
	if got != "xaheitr" {
		t.Errorf("wrote %q, want \"xaheitr\" with no Vz", got)
	}
}

func TestMultipleAffixAdjunct_NeedsTwoAffixes(t *testing.T) {
	ma := g.MultipleAffixAdjunct{
		First:      g.Affix{Type: g.Type1Affix, Degree: 1, Consonant: "x"},
		FirstScope: g.ScopeVDom,
	}
	if _, err := MultipleAffixAdjunct(ma); err == nil {
		t.Error("expected an error with no trailing affix")
	}
}

// TestModularAdjunct_RoundTrip compares grammar values rather than
// strings, because §4.3 gives Slot 2 an Aspect two spellings — "C_N =
// w~y" — and the renderer picks one. Quijada's "uya" comes back "uwa",
// the same adjunct spelled the other way. Every other example is
// written back exactly, which the next test pins.
func TestModularAdjunct_RoundTrip(t *testing.T) {
	// §4.3's own example list. "öhwoňó" carries ultimate stress and so
	// exercises the V_H reach in Slot 4.
	for _, word := range []string{"yu", "üha", "ihwe", "yewia", "uhlaini", "uya", "öhwoňó"} {
		ma, err := parse.ParseModular(word)
		if err != nil {
			t.Fatalf("ParseModular(%q): %v", word, err)
		}
		got, err := ModularAdjunct(ma)
		if err != nil {
			t.Errorf("ModularAdjunct(%q -> %+v): %v", word, ma, err)
			continue
		}
		back, err := parse.ParseModular(got)
		if err != nil {
			t.Errorf("%q rendered %q, which does not parse: %v", word, got, err)
			continue
		}
		if !reflect.DeepEqual(back, ma) {
			t.Errorf("%q -> %q -> %+v, want %+v", word, got, back, ma)
		}
	}
}

// TestModularAdjunct_Canonical pins the exact spelling, so a change to
// the layout rule — which value takes Slot 4, when the pair slots fill
// — shows up as a diff rather than as a silently different word.
func TestModularAdjunct_Canonical(t *testing.T) {
	for _, tc := range []struct{ in, want string }{
		{"yu", "yu"},
		{"üha", "üha"},
		{"ihwe", "ihwe"},
		{"yewia", "yewia"},
		{"uhlaini", "uhlaini"},
		{"öhwoňó", "öhwoňó"},
		{"uya", "uwa"}, // §4.3's w~y, canonicalized to w
	} {
		ma, err := parse.ParseModular(tc.in)
		if err != nil {
			t.Fatalf("ParseModular(%q): %v", tc.in, err)
		}
		got, err := ModularAdjunct(ma)
		if err != nil {
			t.Errorf("ModularAdjunct(%q): %v", tc.in, err)
			continue
		}
		if got != tc.want {
			t.Errorf("%q rendered %q, want %q", tc.in, got, tc.want)
		}
	}
}

func TestModularAdjunct_Empty(t *testing.T) {
	if _, err := ModularAdjunct(g.ModularAdjunct{}); err == nil {
		t.Error("expected an error with no content")
	}
}
