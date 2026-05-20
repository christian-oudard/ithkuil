package main

import (
	"fmt"
	"io"

	"github.com/coudard/ithkuil/go/compose"
)

// cmdLexicon substring-searches the root and/or affix lexicons.
//
// Usage: ithkuil lexicon QUERY [--kind=root|affix|both] [--limit=N]
func cmdLexicon(args []string, stdout, stderr io.Writer, lexDir string) int {
	fs := newFlagSet("lexicon", stderr)
	fs.describe("Substring search the root and/or affix lexicons.", "QUERY")
	kind := fs.String("kind", "k", "both", "KIND", "root | affix | both (default both)")
	limit := fs.Int("limit", "n", 20, "N", "maximum hits per kind (default 20)")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	rest := fs.Args()
	if len(rest) == 0 {
		fmt.Fprintln(stderr, "usage: ithkuil lexicon QUERY [--kind=root|affix|both]")
		return 2
	}
	query := rest[0]

	lex := loadLex(lexDir, stderr)
	if lex == nil {
		return 1
	}

	doRoot := *kind == "root" || *kind == "both"
	doAffix := *kind == "affix" || *kind == "both"

	if doRoot {
		hits := compose.SearchRoots(query, lex.Roots)
		if len(hits) > *limit {
			hits = hits[:*limit]
		}
		fmt.Fprintln(stdout, "Roots:")
		if len(hits) == 0 {
			fmt.Fprintln(stdout, "  (none)")
		}
		for _, h := range hits {
			fmt.Fprintf(stdout, "  -%s- (score %d)\n", h.Cr, h.Score)
			printStem(stdout, "S0", h.Entry.Stem0)
			printStem(stdout, "S1", h.Entry.Stem1)
			printStem(stdout, "S2", h.Entry.Stem2)
			printStem(stdout, "S3", h.Entry.Stem3)
		}
	}

	if doAffix {
		hits := compose.SearchAffixes(query, lex.Affixes)
		if len(hits) > *limit {
			hits = hits[:*limit]
		}
		if doRoot {
			fmt.Fprintln(stdout)
		}
		fmt.Fprintln(stdout, "Affixes:")
		if len(hits) == 0 {
			fmt.Fprintln(stdout, "  (none)")
		}
		for _, a := range hits {
			fmt.Fprintf(stdout, "  -%s- %s  %s\n", a.Cs, a.Abbrev, a.Description)
			for i, d := range a.Degrees {
				if d == "" {
					continue
				}
				fmt.Fprintf(stdout, "    %d. %s\n", i+1, d)
			}
		}
	}
	return 0
}

func printStem(w io.Writer, label, s string) {
	if s == "" {
		return
	}
	fmt.Fprintf(w, "    %s: %s\n", label, s)
}
