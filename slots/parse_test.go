package slots

import "testing"

// TestParse_SlotVGlottalBothPlacements pins §1.7's two landing spots
// for the §3.6.2 end-of-Slot-V marker. Rule 1 puts the glottal after
// the vowel-form; Rule 3 puts it inside, reduplicating a single vowel
// and splitting a diphthong. All of them mark the same boundary and
// decode to the same affix.
//
// Before this was handled, the Rule 3 spellings fell through Slot V
// entirely and the unrecognized Vx silently read as a degree-0 Slot
// VII affix — a different word, reported without complaint.
func TestParse_SlotVGlottalBothPlacements(t *testing.T) {
	cases := []struct {
		name   string
		a, b   string
		vx, cs string
	}{
		{"single vowel", "wala'na", "wala'ana", "a", "n"},
		{"diphthong", "womëu'ţřat", "womë'uţřat", "ëu", "ţř"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			for _, in := range []string{c.a, c.b} {
				l, err := Parse(in)
				if err != nil {
					t.Fatalf("Parse(%q): %v", in, err)
				}
				if len(l.SlotV) != 1 {
					t.Fatalf("Parse(%q): Slot V = %v, want 1 affix", in, l.SlotV)
				}
				if l.SlotV[0].Vx != c.vx || l.SlotV[0].Cs != c.cs {
					t.Errorf("Parse(%q): Slot V = %+v, want Vx=%q Cs=%q",
						in, l.SlotV[0], c.vx, c.cs)
				}
			}
		})
	}
}

// TestParse_RejectsWordFinalGlottal covers the §3.6.2 end-of-Slot-V
// marker with nothing after it. SplitConjuncts hands a word-final
// glottal back as a bare "'" conjunct, which used to have its leading
// glottal stripped like any other "'C" and be recorded as an affix
// with an empty Cs. An affix is its Cs, so that value was not a word
// at all: it re-rendered to different text and no encoding could
// represent it.
func TestParse_RejectsWordFinalGlottal(t *testing.T) {
	// Shortcut Cc, so the Slot V end-marker path is the one taken.
	for _, in := range []string{"warwä'", "yúřku'", "wasahňe'"} {
		if l, err := Parse(in); err == nil {
			t.Errorf("Parse(%q) succeeded with Slot V %+v, want an error", in, l.SlotV)
		}
	}
	// The same marker followed by a real Cs still parses.
	l, err := Parse("wala'na")
	if err != nil {
		t.Fatalf("Parse(wala'na): %v", err)
	}
	if len(l.SlotV) != 1 || l.SlotV[0].Cs != "n" {
		t.Errorf("Parse(wala'na): Slot V = %+v, want one affix with Cs \"n\"", l.SlotV)
	}
}

// TestToGrammar_RejectsNonVxVowel checks that a vowel outside the §3.5
// table is a parse error rather than a silently invented degree 0.
func TestToGrammar_RejectsNonVxVowel(t *testing.T) {
	l := Layout{
		Kind:    CrFormative,
		Cr:      "ml",
		Vr:      "a",
		Ca:      "l",
		SlotVII: []AffixChunk{{Vx: "ao", Cs: "r"}},
		Vc:      "a",
	}
	if _, err := ToGrammar(l); err == nil {
		t.Error("ToGrammar accepted Vx \"ao\", want an error")
	}
}
