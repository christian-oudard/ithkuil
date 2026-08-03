package main

import (
	"errors"
	"fmt"
	"io"

	"github.com/christian-oudard/ithkuil/fault"
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
//	T1-ml ml
//
// Every word class is accepted, not only formatives. Routing this
// through gloss.ParseFormative meant a referential gloss like "1m-ERG"
// was read as a formative whose root is the cluster "1m", which built
// the unpronounceable "wa1mo" and reported success.
//
// The expression is one word, but not always one token: a §3.1 chain is
// a single hyphenated word whose gloss separates its members with a
// space, so "T1-ml ml" is one argument holding two tokens. Reading it
// with gloss.ParseWord took the whole string for a root and failed on
// the space, which broke the round trip on exactly the output "parse
// --short" gives for a chain. gloss.ParseText splits and then rejoins
// the members by their Slot I markers.
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
	words, err := gloss.ParseText(rest[0], lex)
	if err != nil {
		renderGlossFaults(stderr, rest[0], err)
		return 2
	}
	if len(words) != 1 {
		fmt.Fprintf(stderr, "compose: %q is %d words; compose builds one\n",
			rest[0], len(words))
		return 2
	}
	tok := words[0]
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

// renderGlossFaults prints one fault per line under the expression
// they came from. Reading is permissive, so there may be several, and
// joining them onto one line ran three separate problems together
// into a sentence that had to be taken apart before any of them could
// be acted on.
func renderGlossFaults(w io.Writer, expr string, err error) {
	var fs fault.Faults
	if !errors.As(err, &fs) {
		fmt.Fprintf(w, "compose: %v\n", err)
		return
	}
	fmt.Fprintf(w, "compose: cannot read %s\n", stylize(ansiBold, expr))
	iw := indented(w, "  ")
	for _, f := range fs.List {
		fmt.Fprintf(iw, "%s %s\n", stylize(ansiRed, "\u2717"), f.Fix)
	}
}
