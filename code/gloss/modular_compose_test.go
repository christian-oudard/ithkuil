package gloss_test

import (
	"reflect"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/roman"
)

// A modular adjunct's canonical gloss has to read back, and for the
// whole class it did not.
//
// slotVIII suppresses MNO Valence and FAC Mood/Case-Scope, which reads
// as "default" inside a formative because the slot holds its position
// whether or not anything is printed there. A modular adjunct's content
// is the whole word and its entries are told apart by their order in a
// hyphen list, so an empty entry does not say "default", it erases a
// slot: one all-default entry glossed to "MOD" and read back as no
// content at all, which §4.3 Slot 4 makes an impossible word and the
// writer then refused; two of them glossed to a bare "-".
//
// Canonical output names every entry for that reason. The display
// gloss keeps "MOD", where a human reads the label rather than the
// content.
func TestModularAdjunct_GlossComposesBack(t *testing.T) {
	gl := &gloss.Glosser{}
	for _, w := range corpus.Words() {
		word, err := roman.ParseWord(w)
		if err != nil {
			continue
		}
		m, ok := word.(g.ModularAdjunct)
		if !ok {
			continue
		}
		s := gl.Word(m, g.Text{m}, 0)
		back, err := gloss.ParseWord(s, nil)
		if err != nil {
			t.Errorf("%q glosses to %q, which does not compose: %v", w, s, err)
			continue
		}
		if !reflect.DeepEqual(back, m) {
			t.Errorf("%q glosses to %q, which composes to a different word\n  want %+v\n  got  %+v",
				w, s, m, back)
			continue
		}
		if again, err := roman.Word(back); err != nil || again != w {
			t.Errorf("%q glosses to %q, which writes back as %q: %v", w, s, again, err)
		}
	}
}

// The corpus holds three modular adjuncts and none of them is at its
// defaults, which is the shape the gloss lost. These are built rather
// than found: one default entry, two of them, and a default entry
// beside a typed one.
func TestModularAdjunct_DefaultContentSurvivesTheGloss(t *testing.T) {
	mno := g.VnCnValence{Valence: g.MNO, MoodScope: g.FAC}
	gl := &gloss.Glosser{}
	for _, want := range []g.ModularAdjunct{
		{Content: []g.SlotVIII{mno}},
		{Content: []g.SlotVIII{mno, mno}},
		{Content: []g.SlotVIII{mno, g.VnCnAspect{Aspect: g.PRG, MoodScope: g.FAC}}},
		{Content: []g.SlotVIII{mno}, Scope: g.ModularScopeParent},
	} {
		s := gl.Word(want, g.Text{want}, 0)
		back, err := gloss.ParseWord(s, nil)
		if err != nil {
			t.Errorf("%+v glosses to %q, which does not compose: %v", want.Content, s, err)
			continue
		}
		if !reflect.DeepEqual(back, want) {
			t.Errorf("%+v glosses to %q, which composes to %+v", want.Content, s, back)
			continue
		}
		if _, err := roman.Word(back); err != nil {
			t.Errorf("%+v glosses to %q, which cannot be written: %v", want.Content, s, err)
		}
	}
}
