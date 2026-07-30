package main

import (
	"fmt"
	"io"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/roman"
)

// cmdCompose builds a word from a gloss expression and prints the
// romanization plus the gloss it round-trips through.
//
// Usage: ithkuil compose EXPR
//
// EXPR is the canonical gloss, the same string "parse --short" emits:
// "-" separates slots, "." joins category values inside a slot, "/"
// binds a degree or a case to a head. Examples:
//
//	ml
//	S2.CPT-ml-ERG
//	S2.CPT-ml-DYN.OBJ-MSS.G.RPV-DEV/3-ERG
//	1m-ERG
//	[CAR]
//
// Every word class is accepted, not only formatives. Routing this
// through gloss.ParseFormative meant a referential gloss like "1m-ERG"
// was read as a formative whose root is the cluster "1m", which built
// the unpronounceable "wa1mo" and reported success.
func cmdCompose(args []string, stdout, stderr io.Writer, dataFile string) int {
	fs := newFlagSet("compose", stderr)
	fs.describe("Build a romanized word from a gloss expression.", "EXPR")
	stressless := fs.Bool("stressless", "", false,
		"write stress as a §4.8 parsing adjunct instead of a diacritic")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	rest := fs.Args()
	if len(rest) == 0 {
		fmt.Fprintln(stderr, "usage: ithkuil compose EXPR")
		return 2
	}
	if len(rest) > 1 {
		fmt.Fprintf(stderr, "compose: unexpected extra args %q\n", rest[1:])
		return 2
	}
	lex := loadLex(dataFile, stderr)
	tok, err := gloss.ParseWord(rest[0], lex)
	if err != nil {
		fmt.Fprintf(stderr, "compose: %v\n", err)
		return 2
	}
	var word string
	if *stressless {
		word, err = roman.Stressless(g.Text{tok})
	} else {
		word, err = roman.Word(tok)
	}
	if err != nil {
		fmt.Fprintf(stderr, "compose: %v\n", err)
		return 2
	}
	// A word class can be real and still write nothing: NRR is the
	// unmarked register, so it has no adjunct. Printing the blank line
	// would read as a bug in the renderer.
	if word == "" {
		fmt.Fprintf(stderr, "compose: %s is unmarked and writes no word\n", rest[0])
		return 2
	}
	fmt.Fprintln(stdout, word)
	fmt.Fprintln(stdout, (&gloss.Glosser{Lex: lex}).Token(tok))
	return 0
}
