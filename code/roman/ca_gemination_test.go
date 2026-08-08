package roman_test

import (
	"reflect"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/allomorph"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/roman"
)

// §3.6.1 marks the end of Slot V by geminating the C_A, so every one of
// the 3840 C_A values has to survive being written beside a Slot V
// affix. allomorph checks the geminates in isolation — all distinct,
// all legal, none equal to a bare C_A — and that is a claim about
// clusters. This is the same claim about words: render the formative,
// say it, read it back, and require the grammar that returns to be the
// grammar that went in.
//
// The distinction is not academic. A geminate can be legal on its own
// and unsayable where the morphology puts it, between the vowel that
// ends Slot V and whatever follows Slot VI, and only assembling the
// word can tell.
//
// This is the evidence behind reading §3.6.1 as a default plus
// exceptions rather than as a dispatch table. Read as a dispatch table
// its nine rules reach 3725 of the 3840 forms and 115 have no geminate
// at all, which was filed as a hole in the language; read as a default
// the rules say where doubling the initial consonant does not hold, and
// the whole space comes out sayable and distinct. See GeminateCa.
func TestCa_EveryFormTakesASlotVAffix(t *testing.T) {
	affix := g.Affix{Type: g.Type1Affix, Degree: 6, Consonant: "r"}
	for _, mode := range []struct {
		name    string
		affixes []g.Affix
	}{
		{"one Slot V affix", []g.Affix{affix}},
		// Two affixes additionally force §3.5.1's Slot II glottal, so
		// the word carries both of the marks that say where Slot V ends.
		{"two Slot V affixes", []g.Affix{affix, affix}},
	} {
		t.Run(mode.name, func(t *testing.T) {
			seen := map[string]g.Formative{}
			for slotVI := range allomorph.CaForward {
				f := g.MinimalFormative("ml")
				f.SlotVI = slotVI
				f.SlotV = mode.affixes
				out, err := roman.Word(f)
				if err != nil {
					t.Errorf("Ca %v with Slot V cannot be written: %v", slotVI, err)
					continue
				}
				w, err := phonology.ParseWord(out)
				if err != nil {
					t.Errorf("Ca %v writes %q, which is not readable: %v", slotVI, out, err)
					continue
				}
				for _, v := range w.Violations() {
					t.Errorf("Ca %v writes %q, which breaks %v", slotVI, out, v)
				}
				back, err := roman.ParseWord(out)
				if err != nil {
					t.Errorf("Ca %v writes %q, which does not parse: %v", slotVI, out, err)
					continue
				}
				if !reflect.DeepEqual(back, g.Word(f)) {
					t.Errorf("Ca %v writes %q, which comes back as %v", slotVI, out, back)
				}
				if prev, ok := seen[out]; ok && !reflect.DeepEqual(prev, f) {
					t.Errorf("%q is written by two different Ca values", out)
				}
				seen[out] = f
			}
			if len(seen) != 3840 {
				t.Errorf("%d distinct words for 3840 Ca values", len(seen))
			}
		})
	}
}

// TestCa_SlotVIsMarkedByGemination is what stops the sweep above from
// passing vacuously. A renderer that reached for some other spelling —
// the Cc shortcut elides C_A altogether and marks Slot V with §3.6.2's
// glottal instead — would satisfy every assertion there while never
// geminating anything, and the §3.6.1 reading would go unexercised.
func TestCa_SlotVIsMarkedByGemination(t *testing.T) {
	affix := g.Affix{Type: g.Type1Affix, Degree: 6, Consonant: "r"}
	var geminated, glottal int
	for slotVI, ca := range allomorph.CaForward {
		f := g.MinimalFormative("ml")
		f.SlotVI = slotVI
		f.SlotV = []g.Affix{affix}
		out, err := roman.Word(f)
		if err != nil {
			continue // reported above
		}
		switch {
		case strings.Contains(out, allomorph.GeminateCa(ca)):
			geminated++
		case strings.Contains(out, "'"):
			glottal++
		default:
			t.Errorf("Ca %q writes %q, which marks the end of Slot V neither way", ca, out)
		}
	}
	// The one glottal is the default C_A. It alone can take the Cc
	// shortcut, which is the shorter spelling and so the canonical one,
	// and that shortcut has no C_A left to geminate.
	if geminated != 3839 || glottal != 1 {
		t.Errorf("%d geminated and %d glottal, want 3839 and 1", geminated, glottal)
	}
}

// §3.8.1.2 lets a Pattern-1 Mood/Case-Scope C_N take the Slot VI
// position when C_A is the default -l-, and no §3.6.1 rule fires on the
// h-initial cluster that puts there. That was filed as a second place
// the source leaves a construct unwritable: a Slot V affix plus a moved
// C_N would have no boundary marker.
//
// It does not arise, because the shortcut is optional and taking it is
// what would cost the marker. With Slot V filled the renderer writes
// the long form — the default C_A geminated to -ll- with the C_N back
// in its own slot — so both the affix and the mood survive. The
// shortcut is a spelling the grammar offers, not one it requires, and
// this is the case where offering it would lose information.
func TestMovedCn_YieldsToASlotVAffix(t *testing.T) {
	affix := g.Affix{Type: g.Type1Affix, Degree: 6, Consonant: "r"}
	for _, m := range g.AllMoods {
		if m == g.FAC {
			continue
		}
		f := g.MinimalFormative("ml")
		f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
		f.SlotVIII = g.VnCnValence{MoodScope: m}
		f.SlotV = []g.Affix{affix}
		out, err := roman.Word(f)
		if err != nil {
			t.Errorf("%s with a Slot V affix cannot be written: %v", m, err)
			continue
		}
		if !strings.Contains(out, "ll") {
			t.Errorf("%s with a Slot V affix writes %q, which does not geminate the Ca", m, out)
		}
		back, err := roman.ParseWord(out)
		if err != nil {
			t.Errorf("%s writes %q, which does not parse: %v", m, out, err)
			continue
		}
		if !reflect.DeepEqual(back, g.Word(f)) {
			t.Errorf("%s writes %q, which comes back as %v", m, out, back)
		}
	}
}
