package main

import (
	"fmt"
	"io"
	"strings"

	"github.com/coudard/ithkuil/go/compose"
)

func cmdRoot(args []string, stdout, stderr io.Writer, lexDir string) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, "usage: ithkuil-gloss --root QUERY")
		return 2
	}
	lex := loadLex(lexDir, stderr)
	if lex == nil {
		fmt.Fprintln(stderr, "root search requires a lexicon (-lex DIR)")
		return 1
	}
	q := strings.Join(args, " ")
	hits := compose.SearchRoots(q, lex.Roots)
	if len(hits) == 0 {
		fmt.Fprintf(stdout, "no roots match %q\n", q)
		return 0
	}
	// Cap output at 30 to avoid swamping the terminal.
	const cap = 30
	shown := len(hits)
	if shown > cap {
		shown = cap
	}
	for _, h := range hits[:shown] {
		fmt.Fprintf(stdout, "-%s-  S1: %s\n", h.Cr, h.Entry.Stem1)
		if h.Entry.Stem2 != "" {
			fmt.Fprintf(stdout, "      S2: %s\n", h.Entry.Stem2)
		}
		if h.Entry.Stem3 != "" {
			fmt.Fprintf(stdout, "      S3: %s\n", h.Entry.Stem3)
		}
	}
	if len(hits) > cap {
		fmt.Fprintf(stdout, "... %d more\n", len(hits)-cap)
	}
	return 0
}

func cmdAffix(args []string, stdout, stderr io.Writer, lexDir string) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, "usage: ithkuil-gloss --affix QUERY")
		return 2
	}
	lex := loadLex(lexDir, stderr)
	if lex == nil {
		fmt.Fprintln(stderr, "affix search requires a lexicon (-lex DIR)")
		return 1
	}
	q := strings.Join(args, " ")
	hits := compose.SearchAffixes(q, lex.Affixes)
	if len(hits) == 0 {
		fmt.Fprintf(stdout, "no affixes match %q\n", q)
		return 0
	}
	const cap = 30
	shown := len(hits)
	if shown > cap {
		shown = cap
	}
	for _, a := range hits[:shown] {
		fmt.Fprintf(stdout, "%s/%s  %s\n", a.Cs, a.Abbrev, a.Description)
		for i, d := range a.Degrees {
			if d != "" {
				fmt.Fprintf(stdout, "    %d: %s\n", i+1, d)
			}
		}
	}
	if len(hits) > cap {
		fmt.Fprintf(stdout, "... %d more\n", len(hits)-cap)
	}
	return 0
}
