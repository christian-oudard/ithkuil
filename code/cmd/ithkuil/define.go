package main

import (
	"fmt"
	"io"
	"strings"
)

// cmdDefine looks an English word up in the lexicon's glosses and shows
// the Ithkuil lexical cores that name it.
//
// Usage: ithkuil define WORD...
func cmdDefine(args []string, stdout, stderr io.Writer, dataFile string) int {
	fs := newFlagSet("define", stderr)
	fs.describe("Look up an English word as Ithkuil lexical cores.", "WORD...")
	limit := fs.Int("limit", "n", 20, "N", "maximum senses shown (default 20)")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	word := strings.Join(fs.Args(), " ")
	if word == "" {
		fmt.Fprintln(stderr, "usage: ithkuil define WORD...")
		return 2
	}

	a, done := loadAPI(dataFile, stderr)
	defer done()
	got, err := a.Define(word, *limit)
	if err != nil {
		fmt.Fprintf(stderr, "define: %v\n", err)
		return 1
	}
	if len(got.Senses) == 0 {
		fmt.Fprintf(stdout, "%s: no root names this in English\n", word)
		return 1
	}

	fmt.Fprintf(stdout, "%s\n", word)
	for _, s := range got.Senses {
		fmt.Fprintf(stdout, "  %-16s %-20s %s\n", s.Word, s.Gloss, s.Meaning)
	}
	if got.More > 0 {
		fmt.Fprintf(stdout, "  ... %d more\n", got.More)
	}
	return 0
}
