package dictionary_test

// docs/dictionary/english.md claims that a list of Ithkuil words is
// built on the root or affix named above them. Those claims are checked
// here, so an entry cannot quietly go stale when the lexicon or the
// canonical romanization changes. The document is the source; this test is
// the proof.

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/roman"
)

func loadLexicon(t *testing.T) *lexicon.Lexicon {
	t.Helper()
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lexicon: %v", err)
	}
	return lex
}

var (
	// Root _Cr_=`-l-`: Human Being
	rootLine = regexp.MustCompile("^Root _Cr_=`-([^`-]+)-`")
	// Affix _Cs_=`-vẓ-` PSA: Personal Association
	affixLine = regexp.MustCompile("^Affix _Cs_=`-([^`-]+)-` ([A-Z0-9]+)")
	// - a person, a human being: **olal**
	entryLine = regexp.MustCompile(`^- .+: \*\*([^*]+)\*\*$`)
)

func doc(t *testing.T) []string {
	t.Helper()
	b, err := os.ReadFile(filepath.Join("..", "..", "docs", "dictionary", "english.md"))
	if err != nil {
		t.Fatalf("read english.md: %v", err)
	}
	return strings.Split(string(b), "\n")
}

// Every word parses, and carries whatever its block header names: the
// root, the affix, or both. A block that names only an affix leaves the
// root free, since the point of such a block is that the affix attaches
// to many different roots.
func TestEnglishDocWords(t *testing.T) {
	var cr, cs, prev string
	n := 0
	for i, line := range doc(t) {
		if m := rootLine.FindStringSubmatch(line); m != nil {
			cr, cs = phonology.FromASCII(m[1]), ""
			prev = "root"
			continue
		}
		if m := affixLine.FindStringSubmatch(line); m != nil {
			// An affix header directly under a root header narrows
			// that root; standing alone it governs no root at all.
			if prev != "root" {
				cr = ""
			}
			cs = phonology.FromASCII(m[1])
			prev = "affix"
			continue
		}
		if strings.HasPrefix(line, "## ") {
			cr, cs, prev = "", "", "head"
			continue
		}
		m := entryLine.FindStringSubmatch(line)
		if m == nil {
			if strings.TrimSpace(line) != "" {
				prev = "text"
			}
			continue
		}
		prev = "entry"
		word := m[1]
		f, err := roman.ParseFormative(word)
		if err != nil {
			t.Errorf("line %d: %q does not parse: %v", i+1, word, err)
			continue
		}
		n++
		if cr != "" {
			root, ok := f.Root.(g.CrRoot)
			if !ok {
				t.Errorf("line %d: %q is not a Cr-root formative", i+1, word)
			} else if root.Cluster != cr {
				t.Errorf("line %d: %q is built on -%s-, but the block claims -%s-",
					i+1, word, root.Cluster, cr)
			}
		}
		if cs != "" && !hasAffix(f, cs) {
			t.Errorf("line %d: %q does not carry the affix -%s- its block claims",
				i+1, word, cs)
		}
	}
	if n < 300 {
		t.Errorf("only %d words checked; the entry format has drifted", n)
	}
}

func hasAffix(f g.Formative, cs string) bool {
	for _, slot := range [][]g.Affix{f.SlotV, f.SlotVII} {
		for _, a := range slot {
			if a.Consonant == cs {
				return true
			}
		}
	}
	return false
}

// Entries are alphabetical. A dictionary that cannot be looked up in is
// not one, and entries get appended, so the order drifts the moment it
// is not checked.
func TestEnglishDocOrder(t *testing.T) {
	var heads []string
	for _, line := range doc(t) {
		if strings.HasPrefix(line, "## ") {
			heads = append(heads, strings.TrimSpace(line[3:]))
		}
	}
	if len(heads) < 50 {
		t.Fatalf("only %d entries found; the heading format has drifted", len(heads))
	}
	for i := 1; i < len(heads); i++ {
		a := strings.Split(heads[i-1], ",")[0]
		b := strings.Split(heads[i], ",")[0]
		if a >= b {
			t.Errorf("entries out of order: %q comes before %q", heads[i-1], heads[i])
		}
	}
}

// Every affix named in a block header exists in the affix reference by
// that abbreviation, so a header cannot invent one.
func TestEnglishDocAffixes(t *testing.T) {
	lex := loadLexicon(t)
	n := 0
	for i, line := range doc(t) {
		m := affixLine.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		cs, abbrev := phonology.FromASCII(m[1]), m[2]
		entry, ok := lex.Affixes[cs]
		if !ok {
			t.Errorf("line %d: no affix -%s-", i+1, cs)
			continue
		}
		if entry.Abbrev != abbrev {
			t.Errorf("line %d: affix -%s- is %s, not %s", i+1, cs, entry.Abbrev, abbrev)
		}
		n++
	}
	if n == 0 {
		t.Error("no affix blocks found; the header format has drifted")
	}
}
