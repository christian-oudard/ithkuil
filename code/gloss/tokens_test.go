package gloss_test

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/gloss"
	"github.com/christian-oudard/ithkuil/inventory"
	"github.com/christian-oudard/ithkuil/roman"
)

// TestTokens_JoinRoundTrips is the guarantee the whole thing rests on.
// A front end that renders tokens instead of the string must show
// exactly what the glosser wrote, or it is quietly displaying a
// different gloss. Checked over every sample in the inventory, one
// minimal word per grammatical value, and every word of the corpus, so
// the syntax is exercised where it is actually used rather than where
// someone thought to write a case.
func TestTokens_JoinRoundTrips(t *testing.T) {
	gl := &gloss.Glosser{}
	var lines []string
	for _, s := range inventory.Samples() {
		lines = append(lines, gl.Token(s.Word))
	}
	for _, w := range corpus.Words() {
		if word, err := roman.ParseWord(w); err == nil {
			lines = append(lines, gl.Token(word))
		}
	}
	if len(lines) < 500 {
		t.Fatalf("only %d glosses to check; the sweep is not running", len(lines))
	}
	for _, line := range lines {
		if got := gloss.Join(gloss.Tokens(line)); got != line {
			t.Errorf("round trip: %q became %q", line, got)
		}
	}
}

func TestTokens_Kinds(t *testing.T) {
	for _, tc := range []struct {
		gloss string
		want  string // Kind initials in order
	}{
		{"ml", "r"},
		{"S2.CPT-ml-ERG", "cpcprpc"},
		{"m-SYS/5_2-{Ca}-DCD/1_2", "rpcpdpdppcppcpdpd"},
		{"[CAR]", "pcp"},
		{"[pa/BEN+1m/BEN]-ERG", "prpcprpcppc"},
		{"S0.PRC-nt,l-STA.OBJ.EXS-IND", "cpcprpcpcpcpc"},
	} {
		var got strings.Builder
		for _, tok := range gloss.Tokens(tc.gloss) {
			got.WriteString(string(tok.Kind)[:1])
		}
		if got.String() != tc.want {
			t.Errorf("%s: kinds = %s, want %s", tc.gloss, got.String(), tc.want)
		}
	}
}

// TestTokens_ClusterCommaStaysWhole pins the one character that looks
// like a separator and is not: the comma of the ASCII digraph notation
// belongs to the cluster it sits in.
func TestTokens_ClusterCommaStaysWhole(t *testing.T) {
	var roots []string
	for _, tok := range gloss.Tokens("S0.PRC-nt,l-STA.OBJ.EXS-IND") {
		if tok.Kind == gloss.KindRoot {
			roots = append(roots, tok.Text)
		}
	}
	if len(roots) != 1 || roots[0] != "nt,l" {
		t.Errorf("roots = %q, want one cluster nt,l", roots)
	}
}

// TestTokens_CodesAreLookable pins that a code token is the string a
// caller can hand to a lookup, with no punctuation stuck to it.
func TestTokens_CodesAreLookable(t *testing.T) {
	for _, tok := range gloss.Tokens("S2.CPT-ml-DYN.OBJ-MSS.G-DEV/3-ERG") {
		if tok.Kind != gloss.KindCode {
			continue
		}
		if strings.ContainsAny(tok.Text, punctChars) {
			t.Errorf("code token %q carries punctuation", tok.Text)
		}
	}
}

const punctChars = "-./+_: ()[]{}"
