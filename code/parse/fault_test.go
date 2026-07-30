package parse_test

import (
	"errors"
	"testing"

	"github.com/christian-oudard/ithkuil/fault"
	"github.com/christian-oudard/ithkuil/parse"
)

// Each adjunct decoder answers one question — is this word my class? —
// and its "no" comes in two kinds. A shape "no" means the word was
// never a candidate, which every class says about almost every word.
// A value "no" means the shape fit and a table did not list what was
// in it, which is a word someone was writing with one thing wrong.
//
// The classifier ranks failed attempts by stage to decide which
// complaint to show, so a decoder that reports the wrong stage either
// buries a useful message or promotes a useless one.

func stageOf(t *testing.T, err error) fault.Stage {
	t.Helper()
	if err == nil {
		t.Fatal("decoder accepted the word")
	}
	var fs fault.Faults
	if !errors.As(err, &fs) {
		t.Fatalf("error %v (%T) is not fault.Faults", err, err)
	}
	return fs.Stage()
}

func TestAdjunctDecoders_ShapeVersusValue(t *testing.T) {
	for _, c := range []struct {
		name  string
		run   func(string) error
		word  string
		stage fault.Stage
	}{
		// "ç" is not a carrier consonant, so nothing about a carrier
		// adjunct was ever in question.
		{"carrier/wrong opener", func(w string) error {
			_, err := parse.ParseCarrier(w)
			return err
		}, "çal", fault.Shape},
		// hl- is a carrier opener and "ae" sits where the case goes.
		// The shape is settled; only the value is wrong.
		{"carrier/unlisted case", func(w string) error {
			_, err := parse.ParseCarrier(w)
			return err
		}, "hlae", fault.Value},
		// One conjunct cannot be a single-affix adjunct at all.
		{"single affix/too short", func(w string) error {
			_, err := parse.ParseSingleAffix(w)
			return err
		}, "a", fault.Shape},
		// Two conjuncts of the wrong kinds: still a shape question.
		{"multiple affix/too short", func(w string) error {
			_, err := parse.ParseMultipleAffix(w)
			return err
		}, "xa", fault.Shape},
	} {
		t.Run(c.name, func(t *testing.T) {
			if got := stageOf(t, c.run(c.word)); got != c.stage {
				t.Errorf("%q failed at stage %v, want %v", c.word, got, c.stage)
			}
		})
	}
}

// Whichever stage it is, the fault has to carry both halves: the code
// a program branches on and the sentence a person acts on.
func TestAdjunctDecoders_FaultsAreComplete(t *testing.T) {
	for _, word := range []string{"çal", "hlae", "a", "xa"} {
		for _, run := range []func(string) error{
			func(w string) error { _, err := parse.ParseCarrier(w); return err },
			func(w string) error { _, err := parse.ParseSingleAffix(w); return err },
			func(w string) error { _, err := parse.ParseMultipleAffix(w); return err },
			func(w string) error { _, err := parse.ParseModular(w); return err },
		} {
			err := run(word)
			if err == nil {
				continue
			}
			var fs fault.Faults
			if !errors.As(err, &fs) {
				t.Errorf("%q: %v (%T) is not fault.Faults", word, err, err)
				continue
			}
			for _, f := range fs.List {
				if f.Code == "" || f.Fix == "" {
					t.Errorf("%q: incomplete fault %+v", word, f)
				}
			}
		}
	}
}
