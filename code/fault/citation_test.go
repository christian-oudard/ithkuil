package fault_test

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"
)

// A fault message points at the rule the word broke, so that a reader
// holding the reference can go and read it. That pointer is worth
// exactly as much as its accuracy, and nothing here re-reads the
// document to check: a message can name a section that says something
// else, or none at all, and every test still passes.
//
// Two have. Both vowel-sequence faults cited "1.2" for years, which is
// neither rule they check — the grammar's §1.2 is the pronunciation
// notes above the diphthong list, and the phonotactics' bars a
// word-initial vowel. The rules meant were grammar §1.2.1 and
// phonotactics §1.4.
//
// So: every § a shipped message cites must exist in the document it
// names. A bare § means the Grammar Design document, which is what a
// message about slots and values is already talking about; the
// phonotactics document is named, because the two number independently
// and collide throughout (docs/reference/READING.md).
//
// Comments are not checked. Inside phonology a bare §2.13 means the
// phonotactics rule, which is right for a file that is a transcription
// of it and wrong for a sentence shown to someone who has neither
// document open. The convention this pins is the one for messages.

// citation finds a §-reference and any document name in front of it.
var citation = regexp.MustCompile(`(phonotactics |grammar )?§(\d+(?:\.\d+)*)`)

func TestMessages_CiteASectionThatExists(t *testing.T) {
	root := repoRoot(t)
	morphology := sectionsOf(t, filepath.Join(root, "docs/reference/morphology.md"))
	phonotactics := sectionsOf(t, filepath.Join(root, "docs/reference/phonotactics.md"))

	var checked int
	for _, path := range goSources(t, filepath.Join(root, "code")) {
		for _, lit := range stringLiterals(t, path) {
			for _, m := range citation.FindAllStringSubmatch(lit.text, -1) {
				doc, section := strings.TrimSpace(m[1]), m[2]
				checked++
				index, name := morphology, "docs/reference/morphology.md"
				if doc == "phonotactics" {
					index, name = phonotactics, "docs/reference/phonotactics.md"
				}
				if !index[section] {
					t.Errorf("%s:%d cites %s, which %s does not have:\n\t%s",
						lit.file, lit.line, m[0], name, lit.text)
				}
			}
		}
	}
	if checked == 0 {
		t.Fatal("no citation was checked; the test is not exercising anything")
	}
	t.Logf("checked %d citations", checked)
}

type literal struct {
	file string
	line int
	text string
}

// stringLiterals returns every string constant in a file. It goes
// through the parser rather than a regexp so that a § inside a comment
// is not mistaken for one in a message — phonotactics.go has a comment
// quoting a "§2.24" precisely to say no such rule exists.
func stringLiterals(t *testing.T, path string) []literal {
	t.Helper()
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, path, nil, 0)
	if err != nil {
		t.Fatalf("parse %s: %v", path, err)
	}
	var out []literal
	ast.Inspect(f, func(n ast.Node) bool {
		b, ok := n.(*ast.BasicLit)
		if !ok || b.Kind != token.STRING {
			return true
		}
		s, err := strconv.Unquote(b.Value)
		if err != nil || !strings.Contains(s, "§") {
			return true
		}
		pos := fset.Position(b.Pos())
		out = append(out, literal{file: shortPath(path), line: pos.Line, text: s})
		return true
	})
	return out
}

func goSources(t *testing.T, dir string) []string {
	t.Helper()
	var out []string
	err := filepath.WalkDir(dir, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && strings.HasSuffix(path, ".go") && !strings.HasSuffix(path, "_test.go") {
			out = append(out, path)
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walk %s: %v", dir, err)
	}
	return out
}

// sectionsOf indexes the section numbers a reference document defines.
// Both documents number in two ways: a markdown heading for the top
// levels, and a bold run at the start of a line for the numbered rules
// and sub-sections under them.
func sectionsOf(t *testing.T, path string) map[string]bool {
	t.Helper()
	text, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	out := map[string]bool{}
	add := func(s string) {
		s = strings.TrimSuffix(s, ".")
		out[s] = true
		// "1.0 PHONOLOGY" is how the grammar writes a chapter, and
		// "§1" is how a message cites one.
		if rest, ok := strings.CutSuffix(s, ".0"); ok {
			out[rest] = true
		}
	}
	heading := regexp.MustCompile(`(?m)^#{2,6}\s+(\d+(?:\.\d+)*)\.?\s`)
	rule := regexp.MustCompile(`(?m)^\*\*(\d+(?:\.\d+)*)\.?(?:\*\*|\s)`)
	for _, m := range heading.FindAllStringSubmatch(string(text), -1) {
		add(m[1])
	}
	for _, m := range rule.FindAllStringSubmatch(string(text), -1) {
		add(m[1])
	}
	if len(out) < 20 {
		t.Fatalf("%s: found only %d sections; the index is not being built", path, len(out))
	}
	return out
}

// repoRoot walks up from the package directory to the tree holding
// docs/, since the reference documents are not in this package.
func repoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for range 5 {
		if _, err := os.Stat(filepath.Join(dir, "docs", "reference")); err == nil {
			return dir
		}
		dir = filepath.Dir(dir)
	}
	t.Fatal("no docs/reference above the package directory")
	return ""
}

func shortPath(path string) string {
	if i := strings.Index(path, "/code/"); i >= 0 {
		return path[i+1:]
	}
	return path
}
