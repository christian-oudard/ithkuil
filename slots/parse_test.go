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
