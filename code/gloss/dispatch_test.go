package gloss_test

import (
	"errors"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/fault"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
)

// tokenFault reads one token the way the CLI does, through ParseToken
// rather than ParseFormative, because which class claims a token is
// exactly what these check.
func tokenFault(t *testing.T, tok string) fault.Faults {
	t.Helper()
	_, err := gloss.ParseWord(tok, testLexicon(t))
	if err == nil {
		t.Fatalf("ParseWord(%q) succeeded", tok)
	}
	var fs fault.Faults
	if !errors.As(err, &fs) {
		t.Fatalf("ParseWord(%q) = %v (%T), not fault.Faults", tok, err, err)
	}
	return fs
}

func parseTokenFor(t *testing.T, tok string, lex *lexicon.Lexicon) (g.Word, error) {
	t.Helper()
	return gloss.ParseWord(tok, lex)
}

// Which word class a gloss token belongs to is decided by its head.
// Deciding it by whether the rest of the token is complete means an
// incomplete token gets handed to the wrong class, which then reports
// against a reading nobody was attempting.
//
// "1m" is a referent list and cannot be anything else: a root is a
// bare consonant cluster, and every referent abbreviation either
// starts with a digit, carries a vowel, or capitalises. It used to be
// recognised as a referential only once its case was written, so a
// referential missing its case fell through to the formative reader
// and came back as a bad root cluster with the digit at fault.

func TestParseToken_AReferentialIsRecognizedByItsHead(t *testing.T) {
	for _, head := range []string{"1m", "2p", "Mx", "Obv"} {
		t.Run(head, func(t *testing.T) {
			msg := tokenFault(t, head).Error()
			if strings.Contains(msg, "root") {
				t.Errorf("read as a formative root: %q", msg)
			}
			if !strings.Contains(strings.ToLower(msg), "case") {
				t.Errorf("does not say a case is what is missing: %q", msg)
			}
		})
	}
}

// The gate must not swing the other way. A root cluster that happens
// to be short is still a root, and stealing it for the referential
// reader would break every minimal formative.
func TestParseToken_AClusterIsStillARoot(t *testing.T) {
	lex := testLexicon(t)
	for _, expr := range []string{"ml", "m", "tpl", "ţk"} {
		t.Run(expr, func(t *testing.T) {
			if _, err := parseTokenFor(t, expr, lex); err != nil {
				t.Errorf("ParseWord(%q): %v", expr, err)
			}
		})
	}
}
