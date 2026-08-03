package roman_test

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/fault"
)

// A §3.1.7 chain is several formatives joined by hyphens, and it can
// fail in ways that are not the same problem at all: a link that does
// not read, a parent carrying a concatenation marker, a dependent
// missing one. All of them came back "hyphenated word %q is not a
// concatenation chain", which names the whole word and says nothing
// about which link is at fault or why — the reader is handed back
// what they typed and left to bisect it themselves.

// The link's own complaint is the useful one. "mavẓorf" fails on its
// Ca, and that is as true inside a chain as outside it.
func TestParseWord_AChainNamesTheLinkThatFailed(t *testing.T) {
	fs := faultsFor(t, "malëuţřait-mavẓorf")
	msg := fs.Error()
	if !strings.Contains(msg, "vẓ") {
		t.Errorf("does not carry the failing link's own complaint: %q", msg)
	}
	if !strings.Contains(msg, "mavẓorf") {
		t.Errorf("does not name which link failed: %q", msg)
	}
	if fs.Stage() != fault.Value {
		t.Errorf("stage = %v, want value: the link read as far as its slots", fs.Stage())
	}
}

// A structural failure is a different complaint from an unreadable
// link, and §3.1.7's rule is what a reader needs quoted back: the
// parent comes last and carries no marker, every link before it does.
func TestParseWord_AChainSaysWhichStructuralRuleBroke(t *testing.T) {
	// Two plain formatives: neither leading link carries a Cc, so
	// there is no dependent and nothing to attach.
	fs := faultsFor(t, "malëuţřait-amlal")
	msg := strings.ToLower(fs.Error())
	if !strings.Contains(msg, "dependent") && !strings.Contains(msg, "parent") {
		t.Errorf("does not say which side of §3.1.7 broke: %q", fs.Error())
	}
}

// Every chain fault is complete, like every other.
func TestParseWord_ChainFaultsAreComplete(t *testing.T) {
	for _, word := range []string{
		"malëuţřait-mavẓorf",
		"malëuţřait-amlal",
		"hakšal-xyzzy",
	} {
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
