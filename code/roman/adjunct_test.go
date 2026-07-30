package roman

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

// The examples above are what Quijada printed, which is the right
// starting point and a thin one: between them they witness one affix
// scope out of six and no V_H reach at all. The dimensions each of
// these three words exists to carry are enumerated below.

func TestSingleAffixAdjunct_EveryScope(t *testing.T) {
	affix := g.Affix{Type: g.Type1Affix, Degree: 5, Consonant: "r"}
	for _, scope := range g.AllAffixScopes {
		want := g.SingleAffixAdjunct{Affix: affix, Scope: scope}
		word, err := SingleAffixAdjunct(want)
		if err != nil {
			t.Errorf("scope %v: %v", scope, err)
			continue
		}
		got, err := parse.ParseSingleAffix(word)
		if err != nil {
			t.Errorf("scope %v wrote %q, which does not parse: %v", scope, word, err)
			continue
		}
		if got != want {
			t.Errorf("scope %v wrote %q, which reads back as %+v", scope, word, got)
		}
	}
}

// §4.1.2 scopes the first affix with C_Z and the rest with V_Z, and
// lets V_Z go unwritten when the two agree. Both halves, every pair.
func TestMultipleAffixAdjunct_EveryScopePair(t *testing.T) {
	first := g.Affix{Type: g.Type1Affix, Degree: 3, Consonant: "r"}
	rest := []g.Affix{{Type: g.Type2Affix, Degree: 5, Consonant: "kt"}}
	for _, fs := range g.AllAffixScopes {
		for _, rs := range g.AllAffixScopes {
			want := g.MultipleAffixAdjunct{
				First: first, Rest: rest, FirstScope: fs, RestScope: rs,
			}
			word, err := MultipleAffixAdjunct(want)
			if err != nil {
				t.Errorf("scopes %v/%v: %v", fs, rs, err)
				continue
			}
			got, err := parse.ParseMultipleAffix(word)
			if err != nil {
				t.Errorf("scopes %v/%v wrote %q, which does not parse: %v", fs, rs, word, err)
				continue
			}
			if !reflect.DeepEqual(got, want) {
				t.Errorf("scopes %v/%v wrote %q, which reads back as %+v", fs, rs, word, got)
			}
		}
	}
}

// Slot 2 is the one position with a C_N of its own, so it is the only
// one that can carry a Mood/Case-Scope. Every V_N category against
// every mood.
func TestModularAdjunct_EveryCategoryAndMood(t *testing.T) {
	for _, mood := range g.AllMoods {
		for _, c := range []g.SlotVIII{
			g.VnCnValence{Valence: g.PRL, MoodScope: mood},
			g.VnCnPhase{Phase: g.ITR, MoodScope: mood},
			g.VnCnEffect{Effect: g.BEN2, MoodScope: mood},
			g.VnCnLevel{Level: g.SBE, MoodScope: mood},
			g.VnCnAspect{Aspect: g.AllAspects[3], MoodScope: mood},
		} {
			want := g.ModularAdjunct{Content: []g.SlotVIII{c}}
			word, err := ModularAdjunct(want)
			if err != nil {
				t.Errorf("%T mood %v: %v", c, mood, err)
				continue
			}
			got, err := parse.ParseModular(word)
			if err != nil {
				t.Errorf("%T mood %v wrote %q, which does not parse: %v", c, mood, word, err)
				continue
			}
			if !reflect.DeepEqual(got.Content, want.Content) {
				t.Errorf("%T mood %v wrote %q, which reads back as %+v", c, mood, word, got.Content)
			}
		}
	}
}

func TestModularAdjunct_EveryScope(t *testing.T) {
	for _, scope := range g.AllModularScopes {
		want := g.ModularAdjunct{
			Scope:   scope,
			Content: []g.SlotVIII{g.VnCnValence{Valence: g.PRL, MoodScope: g.SUB}},
		}
		word, err := ModularAdjunct(want)
		if err != nil {
			t.Errorf("scope %v: %v", scope, err)
			continue
		}
		got, err := parse.ParseModular(word)
		if err != nil {
			t.Errorf("scope %v wrote %q, which does not parse: %v", scope, word, err)
			continue
		}
		if got.Scope != scope {
			t.Errorf("scope %v wrote %q, which reads back as %v", scope, word, got.Scope)
		}
	}
}

// The V_H reach, which no corpus word and no printed example carries.
// §4.3 reads the trailing vowel as a reach only under ultimate stress,
// so the stress mark is half of what is being checked here.
func TestModularAdjunct_EveryReach(t *testing.T) {
	for _, reach := range g.AllModularReaches {
		if reach == g.ModularReachNone {
			continue
		}
		want := g.ModularAdjunct{
			Reach:   reach,
			Content: []g.SlotVIII{g.VnCnValence{Valence: g.PRL, MoodScope: g.SUB}},
		}
		word, err := ModularAdjunct(want)
		if err != nil {
			t.Errorf("reach %v: %v", reach, err)
			continue
		}
		got, err := parse.ParseModular(word)
		if err != nil {
			t.Errorf("reach %v wrote %q, which does not parse: %v", reach, word, err)
			continue
		}
		if got.Reach != reach {
			t.Errorf("reach %v wrote %q, which reads back as %v", reach, word, got.Reach)
		}
		if !reflect.DeepEqual(got.Content, want.Content) {
			t.Errorf("reach %v wrote %q, whose content reads back as %+v", reach, word, got.Content)
		}
	}
}

// A lone aspect at the default mood is Slot 4 by itself, and every
// aspect has to survive being written there.
func TestModularAdjunct_EveryLoneAspect(t *testing.T) {
	for _, a := range g.AllAspects {
		want := g.ModularAdjunct{Content: []g.SlotVIII{
			g.VnCnAspect{Aspect: a, MoodScope: g.FAC},
		}}
		word, err := ModularAdjunct(want)
		if err != nil {
			t.Errorf("aspect %v: %v", a, err)
			continue
		}
		got, err := parse.ParseModular(word)
		if err != nil {
			t.Errorf("aspect %v wrote %q, which does not parse: %v", a, word, err)
			continue
		}
		if !reflect.DeepEqual(got.Content, want.Content) {
			t.Errorf("aspect %v wrote %q, which reads back as %+v", a, word, got.Content)
		}
	}
}
