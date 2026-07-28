package main

import (
	"fmt"
	"io"

	"github.com/christian-oudard/ithkuil/compose"
	"github.com/christian-oudard/ithkuil/gloss"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/render"
)

// cmdCompose builds a formative from a gloss-style expression and
// prints the surface form plus the gloss it round-trips through.
//
// Usage: ithkuil compose EXPR
//
// EXPR is the syntax accepted by compose.Formative: "-" separates
// slots, "." joins category values inside a slot, "/" binds a degree
// or a case to a head. Examples:
//
//	ml
//	S2.CPT-ml-ERG
//	S2.CPT-ml-DYN.OBJ-MSS.G.RPV-DEV/3-ERG
func cmdCompose(args []string, stdout, stderr io.Writer, dataFile string) int {
	fs := newFlagSet("compose", stderr)
	fs.describe("Build a surface formative from a gloss-style expression.", "EXPR")
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
	var affixes map[string]lexicon.AffixEntry
	if lex != nil {
		affixes = lex.Affixes
	}
	f, err := compose.Formative(rest[0], affixes)
	if err != nil {
		fmt.Fprintf(stderr, "compose: %v\n", err)
		return 2
	}
	fmt.Fprintln(stdout, render.Formative(f))
	fmt.Fprintln(stdout, (&gloss.Glosser{Lex: lex}).Formative(f))
	return 0
}
