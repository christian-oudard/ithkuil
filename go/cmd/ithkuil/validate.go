package main

import (
	"fmt"
	"io"
	"strings"

	"github.com/coudard/ithkuil/go/validation"
)

// cmdValidate runs phonotactic validation on each word in the input.
// Exit code is 0 when every word passes; 1 when at least one word
// has violations.
func cmdValidate(args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("validate", stderr)
	fs.describe("Run phonotactic validation per word; exits 1 if any word is invalid.", "WORD...")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	rest := fs.Args()
	text := strings.Join(rest, " ")
	if text == "" {
		fs.Usage()
		return 2
	}
	any := false
	for _, word := range strings.Fields(text) {
		res := validation.ValidateWord(word)
		if res.Valid {
			fmt.Fprintf(stdout, "%s  OK\n", word)
			continue
		}
		any = true
		for _, e := range res.Errors {
			fmt.Fprintf(stdout, "%s  %s: %s", word, e.Rule, e.Reason)
			if e.Cluster != "" {
				fmt.Fprintf(stdout, " (cluster %s)", e.Cluster)
			}
			fmt.Fprintln(stdout)
		}
	}
	if any {
		return 1
	}
	return 0
}
