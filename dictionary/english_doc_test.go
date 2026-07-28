package dictionary_test

// docs/dictionary/english.md claims that each gloss expression composes
// to a particular word. Those claims are checked here, so an entry
// cannot quietly go stale when the lexicon or the canonical surface
// changes. The document is the source; this test is the proof.

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/compose"
	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/render"
)

// An entry line: - `S2-l` → **elal** — a child
var entryLine = regexp.MustCompile("^- `([^`]+)` → \\*\\*([^*]+)\\*\\*")

func loadDoc(t *testing.T) (string, *lexicon.Lexicon) {
	t.Helper()
	b, err := os.ReadFile(filepath.Join("..", "docs", "dictionary", "english.md"))
	if err != nil {
		t.Fatalf("read english.md: %v", err)
	}
	lex, err := lexicon.Load(filepath.Join("..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lexicon: %v", err)
	}
	return string(b), lex
}

func TestEnglishDocEntriesCompose(t *testing.T) {
	doc, lex := loadDoc(t)
	n := 0
	for _, line := range strings.Split(doc, "\n") {
		m := entryLine.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		expr, want := m[1], m[2]
		f, err := compose.Formative(expr, lex.Affixes)
		if err != nil {
			t.Errorf("compose(%q): %v", expr, err)
			continue
		}
		if got := render.Formative(f); got != want {
			t.Errorf("compose(%q) = %q, doc claims %q", expr, got, want)
		}
		n++
	}
	if n < 50 {
		t.Errorf("only %d entry lines matched; the entry format has drifted", n)
	}
}

// Worked phrases sit in indented blocks, the Ithkuil line followed by
// its English. Every Ithkuil word in them must parse.
func TestEnglishDocPhrasesParse(t *testing.T) {
	doc, _ := loadDoc(t)
	n := 0
	for _, line := range strings.Split(doc, "\n") {
		if !strings.HasPrefix(line, "    ") {
			continue
		}
		line = strings.TrimSpace(line)
		// Skip the English translation, shell transcripts, and the
		// sample compose output.
		if line == "" || strings.HasPrefix(line, "$") || !isIthkuil(line) {
			continue
		}
		for _, w := range strings.Fields(strings.TrimRight(line, ".")) {
			if _, err := fullparse.Formative(w); err != nil {
				t.Errorf("phrase word %q does not parse: %v", w, err)
				continue
			}
			n++
		}
	}
	if n == 0 {
		t.Error("no worked phrases found; the phrase format has drifted")
	}
}

// isIthkuil reports whether an indented line is the Ithkuil half of a
// worked phrase rather than its English translation. Ithkuil text in
// this document is lowercase throughout, since a capital is a
// sentence-position artifact that carries no meaning.
func isIthkuil(line string) bool {
	return line == strings.ToLower(line)
}
