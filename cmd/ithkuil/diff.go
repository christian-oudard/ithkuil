package main

import (
	"fmt"
	"io"
	"strings"

	"github.com/christian-oudard/ithkuil/inspect"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// cmdDiff renders a slot-by-slot diff. Single-word pair: `diff A B`.
// Sentence pair: `diff WORDS... -- WORDS...`. See inspect.Diff.
func cmdDiff(args []string, stdout, stderr io.Writer) int {
	// Bare --help still works because cmdDiff is called from main with
	// the leftover args; "--help" is not consumed by main and we want
	// it to print the diff-specific usage instead of erroring.
	if len(args) == 0 || args[0] == "--help" || args[0] == "-h" {
		fmt.Fprintln(stderr, diffUsage)
		if len(args) > 0 {
			return 0
		}
		return 2
	}
	lhsRaw, rhsRaw := splitDiffArgs(args)
	lhs := tokenize.Tokenize(strings.Join(lhsRaw, " "))
	rhs := tokenize.Tokenize(strings.Join(rhsRaw, " "))
	if len(lhs) == 0 || len(rhs) == 0 {
		fmt.Fprintln(stderr, diffUsage)
		return 2
	}
	inspect.Diff(stdout, lhs, rhs)
	return 0
}

const diffUsage = `usage: ithkuil diff WORD_A WORD_B
   or: ithkuil diff WORDS... -- WORDS...`

// splitDiffArgs splits at the first "--", or treats exactly 2 args as
// a single-word pair when no separator is given.
func splitDiffArgs(args []string) ([]string, []string) {
	for i, a := range args {
		if a == "--" {
			return args[:i], args[i+1:]
		}
	}
	if len(args) == 2 {
		return args[:1], args[1:]
	}
	return args, nil
}
