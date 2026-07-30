package roman_test

import (
	"errors"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/fault"
	"github.com/christian-oudard/ithkuil/roman"
)

// Word class is decided by trying each class in turn, so a word no
// class claims has failed several times over and the complaints are
// not equally worth printing. The one to keep is from the class that
// read furthest: it recognized the shape and objected to a value,
// where the others never recognized anything.
//
// Ranking only works if every class states its stage. A class whose
// failures are untyped is invisible to the comparison and loses to
// any class that types its own, however badly that class fit.

func faultsFor(t *testing.T, word string) fault.Faults {
	t.Helper()
	_, err := roman.ParseWord(word)
	if err == nil {
		t.Fatalf("ParseWord(%q) succeeded", word)
	}
	var fs fault.Faults
	if !errors.As(err, &fs) {
		t.Fatalf("ParseWord(%q) = %v (%T), not fault.Faults", word, err, err)
	}
	return fs
}

// "hlae" is hl- plus one vowel: the shape of a carrier adjunct and of
// nothing else. The carrier decoder recognizes it and objects that
// "ae" is not a case; the formative decoder objects that two
// conjuncts cannot be a formative at all. Reporting the second sends
// a reader to look for a missing Ca in a word that never wanted one.
func TestParseWord_KeepsTheClassThatReadFurthest(t *testing.T) {
	fs := faultsFor(t, "hlae")
	if fs.Stage() != fault.Value {
		t.Errorf("stage = %v, want value: the carrier shape was recognized", fs.Stage())
	}
	joined := strings.ToLower(fs.Error())
	if strings.Contains(joined, "formative needs at least") {
		t.Errorf("reported the formative's shape complaint: %q", joined)
	}
	if !strings.Contains(joined, "ae") {
		t.Errorf("does not name the offending vowel: %q", joined)
	}
}

// The same, one class over. "tae" is a referent cluster plus a vowel:
// §4.6.1's shape, and the referential decoder gets as far as looking
// the vowel up in the case table. Its complaint names the head that
// makes it a referential, which is the evidence for preferring it.
func TestParseWord_KeepsTheReferentialOverTheFormative(t *testing.T) {
	fs := faultsFor(t, "tae")
	if fs.Stage() != fault.Value {
		t.Errorf("stage = %v, want value: the referential shape was recognized", fs.Stage())
	}
	joined := fs.Error()
	if !strings.Contains(joined, "referent chain") {
		t.Errorf("does not say why a case was wanted here: %q", joined)
	}
}

// A word that fits no class at all has no value-stage complaint to
// promote, and must not manufacture one. Saying only that nothing
// recognized it is the whole of what is known.
func TestParseWord_SaysSoWhenNoClassFits(t *testing.T) {
	fs := faultsFor(t, "hlç")
	if fs.Stage() == fault.Value {
		t.Errorf("claimed a value-stage reading of a word nothing parsed: %v", fs)
	}
}

// Every fault a reader can reach carries all three parts. A code with
// no fix is a classification with nothing to act on, and a fix with no
// code is the prose we started with.
func TestParseWord_EveryFaultIsComplete(t *testing.T) {
	for _, word := range []string{"hlae", "hlç", "mavẓorf", "mëivẓoirf", "xyzzy"} {
		t.Run(word, func(t *testing.T) {
			for _, f := range faultsFor(t, word).List {
				if f.Code == "" {
					t.Errorf("fault has no code: %+v", f)
				}
				if f.Fix == "" {
					t.Errorf("fault has no fix: %+v", f)
				}
			}
		})
	}
}
