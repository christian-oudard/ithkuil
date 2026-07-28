package compose_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/compose"
	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/gloss"
	"github.com/christian-oudard/ithkuil/render"
	"github.com/christian-oudard/ithkuil/validation"
)

// §4.6.5's Column-4 shortcut: a Column-4 vowel from the Standard Vowel
// Sequence on a referential Cs marks one of the nine Transrelative
// cases. Unlike the Type-3 referential shortcut in the same section,
// which fires only when it is alone in its slot, this one may be used
// "regardless of other V_X C_S affixes being present in the same Slot".
//
// Written "(refs)/CASE" against the Type-3 form's "(refs)/degree". A
// case is three uppercase letters and a degree is one digit, so the
// two token shapes cannot collide.
func TestColumn4_RoundTrip(t *testing.T) {
	gl := &gloss.Glosser{Canonical: true}
	for _, in := range []string{
		"ml-(1m)/AFF-ERG",
		// Alongside an ordinary affix, which the Type-3 shortcut would
		// not tolerate.
		"ml-(2m)/ERG-t/1-ERG",
		// A multi-referent cluster, and an effect on one of them.
		"ml-(1m+2p/BEN)/DAT-ERG",
	} {
		f, err := compose.Formative(in, nil)
		if err != nil {
			t.Errorf("compose(%q): %v", in, err)
			continue
		}
		w := render.Formative(f)
		if r := validation.ValidateWord(w); !r.Valid {
			t.Errorf("%q renders to %q, which our own validator rejects: %v", in, w, r.Errors)
		}
		back, err := fullparse.Formative(w)
		if err != nil {
			t.Errorf("parse(%q) from %q: %v", w, in, err)
			continue
		}
		if again := gl.Formative(back); again != in {
			t.Errorf("round trip of %q came back as %q (surface %q)", in, again, w)
		}
	}
}

// Only the nine Transrelative cases have a Column-4 form. Asking for
// any of the other 59 has to fail rather than silently pick one.
func TestColumn4_RejectsNonTransrelativeCase(t *testing.T) {
	if _, err := compose.Formative("ml-(1m)/PRP-ERG", nil); err == nil {
		t.Error("compose accepted PRP as a Column-4 case, want an error")
	}
}
