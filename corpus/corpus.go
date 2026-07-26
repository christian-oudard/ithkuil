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
