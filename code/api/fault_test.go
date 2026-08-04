package api

import (
	"encoding/json"
	"strings"
	"testing"
)

// Every reader below this package reports a failure as a stage, a
// code, the text at fault and what would be admissible. The API is
// where that stops: Error carries a message and Word carries an error
// string, so a caller holding the wire format has prose and has to
// match English to find the slot it needs.
//
// The prose stays — it is what a page prints — but the structure
// travels beside it.

func TestReply_CarriesTheFaultsBesideTheMessage(t *testing.T) {
	a := New()
	_, err := a.Compose("S9-ZZZ-ml-QQQ", false)
	if err == nil {
		t.Fatal("Compose accepted a gloss with three bad tokens")
	}
	var env struct {
		Error struct {
			Message string      `json:"message"`
			Faults  []Violation `json:"faults"`
		} `json:"error"`
	}
	if uerr := json.Unmarshal([]byte(Reply(nil, err)), &env); uerr != nil {
		t.Fatalf("unmarshal: %v", uerr)
	}
	if env.Error.Message == "" {
		t.Error("the message a page prints is gone")
	}
	if len(env.Error.Faults) != 3 {
		t.Fatalf("faults = %+v, want one per bad token", env.Error.Faults)
	}
	for _, f := range env.Error.Faults {
		if f.Stage == "" || f.Code == "" || f.Found == "" || f.Fix == "" {
			t.Errorf("incomplete fault: %+v", f)
		}
	}
}

// An error that is not a reader's — a bad request, a missing argument
// — has no faults to carry, and must not grow an empty array that a
// caller would read as "there were none".
func TestReply_OmitsFaultsWhereThereAreNone(t *testing.T) {
	a := New()
	_, err := a.Compose("", false)
	if err == nil {
		t.Fatal("Compose accepted an empty expression")
	}
	if strings.Contains(Reply(nil, err), `"faults"`) {
		t.Errorf("a non-reader error carries a faults field: %s", Reply(nil, err))
	}
}

// A word that would not read carries the same structure as the
// envelope does, since the page marks it in place rather than
// replacing the whole response with a failure.
func TestParse_AnUnreadableWordCarriesItsFaults(t *testing.T) {
	a := New()
	words := a.Parse("mavẓorf")
	if len(words) != 1 {
		t.Fatalf("got %d words, want 1", len(words))
	}
	w := words[0]
	if w.Error == "" {
		t.Error("the message a page prints is gone")
	}
	if len(w.Faults) == 0 {
		t.Fatal("no faults on an unreadable word")
	}
	if got := w.Faults[0].Code; got != "Ca" {
		t.Errorf("fault code = %q, want the slot at fault", got)
	}
}

// Faults and Violations answer different questions and must not be
// merged. A word can read perfectly and still break a §2 rule, which
// is why the Ca tables generate clusters our own reading rejects; and
// a word can be pronounceable and still not read.
func TestParse_FaultsAndViolationsStaySeparate(t *testing.T) {
	a := New()
	// Pronounceable, does not read: the Ca is not a Ca.
	w := a.Parse("mavẓorf")[0]
	if len(w.Faults) == 0 {
		t.Error("a word that does not read has no faults")
	}
	if len(w.Violations) != 0 {
		t.Errorf("a pronounceable word reports §2 violations: %+v", w.Violations)
	}
}
