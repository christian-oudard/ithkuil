// Package corpus holds the official example sentences from the New
// Ithkuil grammar, for use as parser and glosser test data. See
// examples.txt for provenance.
package corpus

import (
	_ "embed"
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

//go:embed discord_examples.txt
var discordFile string

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
// discord_examples.txt for the reasoning behind each entry and for why
// the archive is treated as usage rather than authority.
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

// DiscordExamples returns every curated word in file order.
func DiscordExamples() []DiscordExample {
	var out []DiscordExample
	for _, line := range strings.Split(discordFile, "\n") {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		f := strings.SplitN(line, "|", 4)
		if len(f) != 4 {
			panic("corpus: malformed discord_examples.txt line: " + line)
		}
		v := strings.TrimSpace(f[0])
		defect := strings.HasPrefix(v, "!")
		v = strings.TrimPrefix(v, "!")
		switch Verdict(v) {
		case Correct, Incorrect:
		default:
			panic("corpus: unknown verdict " + v + " in: " + line)
		}
		out = append(out, DiscordExample{
			Verdict: Verdict(v),
			Word:    strings.TrimSpace(f[1]),
			Rule:    strings.TrimSpace(f[2]),
			Reason:  strings.TrimSpace(f[3]),
			Defect:  defect,
		})
	}
	return out
}
