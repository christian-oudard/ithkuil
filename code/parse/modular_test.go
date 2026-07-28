package parse

import (
	"testing"

	"github.com/christian-oudard/ithkuil/grammar"
)

func TestParseModular_Valid(t *testing.T) {
	cases := []struct {
		word   string
		vn, cn string
	}{
		{"ah", "a", "h"},
		{"aihl", "ai", "hl"},
		{"iahňw", "ia", "hňw"},
	}
	for _, c := range cases {
		ma, err := ParseModular(c.word)
		if err != nil {
			t.Errorf("ParseModular(%q) error: %v", c.word, err)
			continue
		}
		if len(ma.Content) != 1 {
			t.Errorf("ParseModular(%q): Content len = %d, want 1", c.word, len(ma.Content))
			continue
		}
		_ = c.vn
		_ = c.cn
	}
}

func TestParseModular_Invalid(t *testing.T) {
	// A single vowel (e.g. "a") is now valid per §4.3 — a lone
	// aspect modular adjunct. Truly invalid inputs: empty string,
	// consonant-only forms (no Vn), and unrecognised conjunct
	// patterns.
	for _, w := range []string{"", "h", "ax", "axyz"} {
		if _, err := ParseModular(w); err == nil {
			t.Errorf("ParseModular(%q) succeeded, want error", w)
		}
	}
}

func TestParseModular_SingleVowel(t *testing.T) {
	// Per §4.3, a single vowel is a lone-aspect modular: no prefix,
	// just a trailing aspect vowel. Decoded as one VnCnAspect entry
	// with default FAC mood.
	ma, err := ParseModular("a")
	if err != nil {
		t.Fatalf("ParseModular(%q): %v", "a", err)
	}
	if ma.Scope != grammar.ModularScopeDefault || len(ma.Content) != 1 {
		t.Errorf("ParseModular(\"a\") = %+v, want lone aspect content", ma)
	}
	if _, ok := ma.Content[0].(grammar.VnCnAspect); !ok {
		t.Errorf("ParseModular(\"a\") content = %T, want VnCnAspect", ma.Content[0])
	}
}

func TestParseModular_ChainsWithVnCn(t *testing.T) {
	// A modular adjunct's typed Content should match ParseVnCn output
	// on the original surface (Vn, Cn) pair.
	ma, err := ParseModular("ah")
	if err != nil {
		t.Fatal(err)
	}
	if len(ma.Content) != 1 {
		t.Fatalf("Content len = %d, want 1", len(ma.Content))
	}
	s8 := ma.Content[0]
	// "a"/"h" should resolve to VnCnValence{MNO, ...}.
	vc, ok := s8.(grammar.VnCnValence)
	if !ok || vc.Valence != grammar.MNO {
		t.Errorf("expected VnCnValence{MNO, ...}, got %#v", s8)
	}
}

// TestParseModular_SpecExamples runs every worked example from the
// §4.3 structure table. The two longest, "uhlaini" and "öhwoňó", fill
// all four slots; they used to fail here and get picked up by the
// formative recogniser as roots "hl" and "hw".
func TestParseModular_SpecExamples(t *testing.T) {
	for _, w := range []string{
		"o", "yu", "üha", "ihwe", "yewia", "uhlaini", "uya", "öhwoňó",
	} {
		if _, err := ParseModular(w); err != nil {
			t.Errorf("ParseModular(%q): %v", w, err)
		}
	}
}

// TestParseModular_SlotConsonants pins the §4.3 split: Slot 2 takes a
// C_N, which carries Mood/Case-Scope, and Slot 3 takes a C_M, which is
// only n or ň and carries none. The inventories are disjoint, so
// neither consonant is legal in the other's slot.
func TestParseModular_SlotConsonants(t *testing.T) {
	// Slot 3 with a C_M: "ai" + "n" reads as an Aspect at default FAC.
	ma, err := ParseModular("uhlaini")
	if err != nil {
		t.Fatalf("uhlaini: %v", err)
	}
	if len(ma.Content) != 3 {
		t.Fatalf("uhlaini: Content len = %d, want 3 (slots 2, 3, 4)", len(ma.Content))
	}
	slot3, ok := ma.Content[1].(grammar.VnCnAspect)
	if !ok {
		t.Fatalf("uhlaini: slot 3 = %T, want VnCnAspect (C_M \"n\")", ma.Content[1])
	}
	if slot3.MoodScope != grammar.FAC {
		t.Errorf("uhlaini: slot 3 MoodScope = %v, want FAC (C_M carries none)", slot3.MoodScope)
	}
	// A C_M in slot 2, and a C_N in slot 3, are both rejected.
	for _, w := range []string{"ani", "uhlaihi"} {
		if _, err := ParseModular(w); err == nil {
			t.Errorf("ParseModular(%q) succeeded, want a slot-consonant rejection", w)
		}
	}
}

// TestParseModular_MaxTwoPairs covers the pair limit. §4.3 has exactly
// two (V_N C) slots, so a third pair is not a modular adjunct. We used
// to accept up to three.
func TestParseModular_MaxTwoPairs(t *testing.T) {
	for _, w := range []string{"uahuehuohuö", "eëyueyoaya", "aiwiuhayô"} {
		if _, err := ParseModular(w); err == nil {
			t.Errorf("ParseModular(%q) succeeded with three pairs, want an error", w)
		}
	}
}
