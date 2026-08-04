package main

import (
	"errors"
	"fmt"
	"io"
	"strings"

	"github.com/christian-oudard/ithkuil/fault"
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
	a, done := loadAPI(dataFile, stderr)
	defer done()
	built, err := a.Compose(rest[0], *stressless)
	if err != nil {
		renderGlossFaults(stderr, rest[0], err)
		return 2
	}
	fmt.Fprintln(stdout, built.Word)
	fmt.Fprintln(stdout, built.Gloss)
	return 0
}

// renderGlossFaults shows the expression token by token, marking the
// ones that failed and writing "ok" against the rest.
//
// The tokens that read are half the diagnosis, the same way the slots
// that read are half of a parse failure. A list of complaints alone
// says what is wrong without saying how much was understood, and a
// writer looking at a long gloss cannot tell whether one token is bad
// or the whole shape was misread.
//
// A fault that names no token — one about the expression as a whole,
// or one the split does not line up with — is printed under the table
// rather than dropped.
func renderGlossFaults(w io.Writer, expr string, err error) {
	var fs fault.Faults
	if !errors.As(err, &fs) {
		fmt.Fprintf(w, "compose: %v\n", err)
		return
	}
	fmt.Fprintf(w, "compose: cannot read %s\n", stylize(ansiBold, expr))
	iw := indented(w, "  ")

	tokens := glossTokens(expr)
	drawn := map[string]bool{}
	for _, t := range tokens {
		drawn[t] = true
	}
	// A fault is placed only against a row that will actually be
	// drawn. Keying on In alone dropped a fault naming the whole
	// expression: it matched no row, and it was not loose either, so
	// nothing printed it at all. Losing a fault is worse than printing
	// it twice, so the two sets are complements by construction.
	byToken := map[string][]fault.Fault{}
	var loose []fault.Fault
	table := len(tokens) > 1
	for _, f := range fs.List {
		if table && f.In != "" && drawn[f.In] {
			byToken[f.In] = append(byToken[f.In], f)
			continue
		}
		loose = append(loose, f)
	}
	if table && len(byToken) > 0 {
		fmt.Fprintln(iw)
		renderTokenTable(iw, tokens, byToken)
	}
	if len(loose) > 0 {
		fmt.Fprintln(iw)
		for _, f := range loose {
			fmt.Fprintf(iw, "%s %s\n", stylize(ansiRed, "\u2717"), f.Fix)
		}
	}
}

// glossTokens splits an expression into the units the reader judges.
// A gloss separates words by space and slots by "-", and a fault
// names whichever of the two it came from, so both are rows. Runs of
// "-" collapse, as the reader collapses them: the canonical gloss
// writes a double hyphen around the root.
func glossTokens(expr string) []string {
	var out []string
	for _, w := range strings.Fields(expr) {
		for _, t := range strings.Split(w, "-") {
			if t != "" {
				out = append(out, t)
			}
		}
	}
	return out
}

func renderTokenTable(w io.Writer, tokens []string, byToken map[string][]fault.Fault) {
	tokW := len("TOKEN")
	for _, t := range tokens {
		if n := runeWidth(t); n > tokW {
			tokW = n
		}
	}
	fmt.Fprintf(w, "   %s  %s\n",
		stylize(ansiDim, padRunes("TOKEN", tokW)),
		stylize(ansiDim, "READS AS"))
	for _, t := range tokens {
		faults := byToken[t]
		if len(faults) == 0 {
			fmt.Fprintf(w, "   %s  %s\n",
				stylize(ansiCyan, padRunes(t, tokW)),
				stylize(ansiDim, "ok"))
			continue
		}
		for i, f := range faults {
			mark, cell := stylize(ansiRed, "\u2717  "), padRunes(t, tokW)
			if i > 0 {
				// A token with two complaints keeps one row each, with
				// the name written once: repeating it would read as
				// two tokens spelled the same.
				mark, cell = "   ", padRunes("", tokW)
			}
			fmt.Fprintf(w, "%s%s  %s\n", mark, stylize(ansiCyan, cell), f.Fix)
		}
	}
}
