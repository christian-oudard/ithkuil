package gloss_test

import (
	"errors"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/fault"
	"github.com/christian-oudard/ithkuil/gloss"
	"github.com/christian-oudard/ithkuil/lexicon"
)

// The gloss is an authoring syntax, so a reader of these messages is
// mid-edit and wants to know which token to change and to what. The
// same four stages apply as to a romanization: a root cluster can
// carry a character outside the alphabet or an unpronounceable
// cluster, a token can fail the punctuation grammar, and a name can
// fail to be in any inventory.

func glossFault(t *testing.T, expr string) fault.Faults {
	t.Helper()
	lex := testLexicon(t)
	_, err := gloss.ParseFormative(expr, lex.Affixes)
	if err == nil {
		t.Fatalf("ParseFormative(%q) succeeded", expr)
	}
	var fs fault.Faults
	if !errors.As(err, &fs) {
		t.Fatalf("ParseFormative(%q) = %v (%T), not fault.Faults", expr, err, err)
	}
	return fs
}

// A token that has committed to a shape must be judged as that shape.
// "DEV/99" is an affix with a bad degree; the affix pattern takes a
// single digit, so it used to fall through to the plain-flag branch
// and come back "unknown grammar flag DEV/99" — a description of
// something the token never was, naming an affix the lexicon knows
// perfectly well as if it were unheard of.
func TestParseFormative_ADegreeOutOfRangeIsNotAnUnknownFlag(t *testing.T) {
	fs := glossFault(t, "ml-DEV/99")
	msg := fs.Error()
	if strings.Contains(msg, "unknown grammar flag") {
		t.Errorf("a bad degree was reported as an unknown flag: %q", msg)
	}
	if !strings.Contains(msg, "99") {
		t.Errorf("does not name the offending degree: %q", msg)
	}
	if !strings.Contains(strings.ToLower(msg), "degree") {
		t.Errorf("does not say a degree is what is wrong: %q", msg)
	}
}

// The same rule, one shape over: "/" binds an argument to a head, so
// a token carrying one has claimed a shape and must be judged against
// it rather than re-read as something with no "/" in it at all.
func TestParseFormative_ASlashTokenIsJudgedAsOne(t *testing.T) {
	for _, expr := range []string{"ml-DEV/99", "ml-ACC/ZZZ", "ml-DEV/"} {
		t.Run(expr, func(t *testing.T) {
			msg := glossFault(t, expr).Error()
			if strings.Contains(msg, "unknown grammar flag") {
				t.Errorf("fell through to the plain-flag reading: %q", msg)
			}
		})
	}
}

// Every gloss fault carries the same three parts as a romanization
// fault: the code a program branches on, the text at fault, and the
// sentence saying what would be admissible.
func TestParseFormative_FaultsAreComplete(t *testing.T) {
	for _, expr := range []string{
		"ml-DEV/99",
		"ml-ZZZ/3",
		"S9-ml",
		"qqq",
		"",
		"ml-Ca:NOPE",
	} {
		t.Run(expr, func(t *testing.T) {
			for _, f := range glossFault(t, expr).List {
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

// A root cluster is Ithkuil text inside the gloss, so it reaches the
// same two early stages a romanization does and must be classified
// the same way. Without this the gloss would report a character
// outside the alphabet at whatever stage its own syntax happened to
// notice it.
func TestParseFormative_RootClusterKeepsThePhonologyStages(t *testing.T) {
	if got := glossFault(t, "qqq").Stage(); got != fault.Chars {
		t.Errorf("non-Ithkuil root reported at stage %v, want characters", got)
	}
	if got := glossFault(t, "zzzz").Stage(); got != fault.Sound {
		t.Errorf("triple-consonant root reported at stage %v, want sound", got)
	}
}

func testLexicon(t *testing.T) *lexicon.Lexicon {
	t.Helper()
	lex, err := lexicon.Load("../../data/data.json")
	if err != nil {
		t.Fatalf("load lexicon: %v", err)
	}
	return lex
}
