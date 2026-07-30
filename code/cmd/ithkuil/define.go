package main

import (
	"fmt"
	"io"
	"strings"

	"github.com/christian-oudard/ithkuil/dictionary"
	"github.com/christian-oudard/ithkuil/gloss"
	"github.com/christian-oudard/ithkuil/roman"
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

	lex := loadLex(dataFile, stderr)
	if lex == nil {
		return 1
	}
	senses := dictionary.Build(lex.Roots).Lookup(word)
	if len(senses) == 0 {
		fmt.Fprintf(stdout, "%s: no root names this in English\n", word)
		return 1
	}

	gl := &gloss.Glosser{}
	fmt.Fprintf(stdout, "%s\n", word)
	for i, s := range senses {
		if i == *limit {
			fmt.Fprintf(stdout, "  ... %d more\n", len(senses)-*limit)
			break
		}
		f := s.Formative()
		fmt.Fprintf(stdout, "  %-16s %-20s %s\n", roman.Formative(f), gl.Formative(f), s.Gloss)
	}
	return 0
}
