package main

import (
	"fmt"
	"io"
)

// cmdLexicon searches the root and/or affix lexicons via FTS5.
//
// Usage: ithkuil lexicon QUERY [--kind=root|affix|both] [--limit=N]
func cmdLexicon(args []string, stdout, stderr io.Writer, dataFile string) int {
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

	s := openStore(dataFile, stderr)
	if s == nil {
		return 1
	}
	defer s.Close()

	doRoot := *kind == "root" || *kind == "both"
	doAffix := *kind == "affix" || *kind == "both"

	if doRoot {
		hits, err := s.SearchRoots(query, *limit)
		if err != nil {
			fmt.Fprintf(stderr, "lexicon: root search: %v\n", err)
			return 1
		}
		fmt.Fprintln(stdout, "Roots:")
		if len(hits) == 0 {
			fmt.Fprintln(stdout, "  (none)")
		}
		for _, h := range hits {
			fmt.Fprintf(stdout, "  -%s-\n", h.Cr)
			printStem(stdout, "S0", h.Stem0)
			printStem(stdout, "S1", h.Stem1)
			printStem(stdout, "S2", h.Stem2)
			printStem(stdout, "S3", h.Stem3)
			printStem(stdout, "CTE", h.Contential)
			printStem(stdout, "CSV", h.Constitutive)
			printTrio(stdout, "OBJ", h.Objective)
			printTrio(stdout, "CPT", h.Completive)
			printStem(stdout, "DYN", h.Dynamic)
			printTrio(stdout, "Wikidata", h.Wikidata)
		}
	}

	if doAffix {
		hits, err := s.SearchAffixes(query, *limit)
		if err != nil {
			fmt.Fprintf(stderr, "lexicon: affix search: %v\n", err)
			return 1
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

func printTrio(w io.Writer, label string, ss []string) {
	for i, s := range ss {
		if s == "" {
			continue
		}
		fmt.Fprintf(w, "    %s S%d: %s\n", label, i+1, s)
	}
}
