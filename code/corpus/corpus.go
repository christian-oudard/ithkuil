// Package corpus holds the official example sentences from the New
// Ithkuil grammar, for use as parser and glosser test data. See
// examples.txt for provenance.
package corpus

import (
	_ "embed"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

//go:embed examples.txt
var examplesFile string

// Example is one row of the corpus: a sentence in Ithkuil, the
// upstream hand-curated gloss, and Quijada's English translation.
type Example struct {
	Section string
	Ithkuil string
	Gloss   string
	English string
}

// Examples returns every corpus row in file order.
func Examples() []Example {
	var out []Example
	for _, line := range strings.Split(examplesFile, "\n") {
		if line == "" || strings.HasPrefix(line, "#") || strings.HasPrefix(line, "Section |") {
			continue
		}
		f := strings.SplitN(line, "|", 4)
		if len(f) != 4 {
			panic("corpus: malformed line: " + line)
		}
		out = append(out, Example{
			Section: strings.TrimSpace(f[0]),
			Ithkuil: strings.TrimSpace(f[1]),
			Gloss:   strings.TrimSpace(f[2]),
			English: strings.TrimSpace(f[3]),
		})
	}
	return out
}

// Words returns every distinct word appearing in the Ithkuil column, in
// first-appearance order and lowercased. Sentence punctuation and the
// bare "/" alternation marker are dropped; hyphens are kept, since a
// hyphen joins the members of a concatenation chain into one word.
func Words() []string {
	var out []string
	seen := map[string]bool{}
	for _, ex := range Examples() {
		for _, w := range strings.FieldsFunc(ex.Ithkuil, isSeparator) {
			w = strings.Trim(w, "“”\"'()[]—;:")
			w = strings.ToLower(w)
			if w == "" || seen[w] {
				continue
			}
			seen[w] = true
			out = append(out, w)
		}
	}
	return out
}

func isSeparator(r rune) bool {
	switch r {
	case ' ', '\t', ',', '.', '?', '!', '/':
		return true
	}
	return false
}

// DiscordExamplesPath returns where the curated word list lives:
// $XDG_DATA_HOME/ithkuil/discord/examples.txt, or the same under
// ~/.local/share when XDG_DATA_HOME is unset, which is the directory
// tools/discord_archive writes its mirror and extracts to.
//
// The list is not in the repo. Its words are other people's Discord
// messages, quoted for a verdict, and where the sentences in
// examples.txt are Quijada's published grammar this is a private
// archive of a chat server. It is a testing record kept beside the
// mirror it was drawn from, so a checkout without it is expected and
// the tests over it skip rather than fail.
func DiscordExamplesPath() string {
	root := os.Getenv("XDG_DATA_HOME")
	if root == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return ""
		}
		root = filepath.Join(home, ".local", "share")
	}
	return filepath.Join(root, "ithkuil", "discord", "examples.txt")
}

// Verdict says whether a word from the community Discord archive is
// well-formed Ithkuil. It is a judgment about the word, not about
// whether we can currently read it.
type Verdict string

const (
	// Correct marks a word that is well-formed under v1.3.1.
	Correct Verdict = "correct"
	// Incorrect marks a word that is not, with Rule naming what it
	// breaks: a foreign name, a fragment, a typing slip, or correct
	// Ithkuil in a version we do not implement.
	Incorrect Verdict = "incorrect"
)

// DiscordExample is one curated word from the archive. See
// DiscordExamplesPath for where the list lives and why it is not in
// the repo; the head of the file itself carries the reasoning behind
// each entry, and why the archive is usage rather than authority.
type DiscordExample struct {
	Verdict Verdict
	Word    string
	Rule    string // the spec section the verdict rests on
	Reason  string
	// Defect is set when we currently disagree with the verdict: a
	// Correct word we fail to read, or an Incorrect one we accept.
	// Written as a leading "!" on the verdict column.
	Defect bool
}

// DiscordExamples returns every curated word in file order, and nil
// when the list is not on this machine. Absence is the ordinary state
// of a fresh checkout and not an error; a list that is present but
// malformed is, and says which line.
func DiscordExamples() ([]DiscordExample, error) {
	path := DiscordExamplesPath()
	b, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	var out []DiscordExample
	for _, line := range strings.Split(string(b), "\n") {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		f := strings.SplitN(line, "|", 4)
		if len(f) != 4 {
			return nil, fmt.Errorf("%s: malformed line: %s", path, line)
		}
		v := strings.TrimSpace(f[0])
		defect := strings.HasPrefix(v, "!")
		v = strings.TrimPrefix(v, "!")
		switch Verdict(v) {
		case Correct, Incorrect:
		default:
			return nil, fmt.Errorf("%s: unknown verdict %q in: %s", path, v, line)
		}
		out = append(out, DiscordExample{
			Verdict: Verdict(v),
			Word:    strings.TrimSpace(f[1]),
			Rule:    strings.TrimSpace(f[2]),
			Reason:  strings.TrimSpace(f[3]),
			Defect:  defect,
		})
	}
	return out, nil
}

//go:embed morphology_examples.txt
var morphologyFile string

// MorphologySection is one section of worked examples from the
// morphology document: the words it demonstrates, and the subset of
// them that no classifier reads today.
type MorphologySection struct {
	Name    string
	Words   []string
	Unknown []string
}

// MorphologySections returns every section in file order. It lives
// here rather than in a _test.go file because two packages need it —
// tokenize checks what classifies, compose checks the gloss round
// trip — and a test-only identifier cannot cross a package boundary.
// Keeping a second copy meant every corpus correction had to be made
// twice, in lockstep, from the same source.
func MorphologySections() []MorphologySection {
	var out []MorphologySection
	for _, line := range strings.Split(morphologyFile, "\n") {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") || strings.HasPrefix(line, "Section |") {
			continue
		}
		f := strings.SplitN(line, "|", 3)
		if len(f) != 3 {
			panic("corpus: malformed morphology_examples.txt line: " + line)
		}
		out = append(out, MorphologySection{
			Name:    strings.TrimSpace(f[0]),
			Words:   strings.Fields(f[1]),
			Unknown: strings.Fields(f[2]),
		})
	}
	return out
}

// MorphologyWords returns every worked-example word, sections
// flattened, in file order.
func MorphologyWords() []string {
	var out []string
	for _, s := range MorphologySections() {
		out = append(out, s.Words...)
	}
	return out
}
