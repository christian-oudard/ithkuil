package main

import (
	"fmt"
	"io"

	"github.com/christian-oudard/ithkuil/compose"
)

// cmdSearch looks a query up in the grammar inventory and in the
// lexicon at once. Grammar hits print first: a short query is far more
// often a grammatical abbreviation than a root, and the grammar is the
// smaller, more definite space. A section with no hits is not printed.
//
// With no query and no category, it lists the categories available to
// --category. --category, --exact, and --form narrow the grammar half;
// the lexicon half is always a substring search for the query.
//
// Usage: ithkuil search [Q] [--category CAT] [--exact] [--form] [--limit N]
func cmdSearch(args []string, stdout, stderr io.Writer, dataFile string) int {
	fs := newFlagSet("search", stderr)
	fs.describe("Search the grammar inventory and the lexicon.", "[QUERY]")
	category := fs.String("category", "c", "", "CAT", "grammar: list only this category (Case, Aspect, Bias, …)")
	exact := fs.Bool("exact", "e", false, "grammar: exact abbreviation match against the query")
	formMode := fs.Bool("form", "f", false, "grammar: treat the query as a surface form (vowel/consonant)")
	limit := fs.Int("limit", "n", 20, "N", "lexicon: maximum hits per kind (default 20)")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	rest := fs.Args()
	query := ""
	if len(rest) > 0 {
		query = rest[0]
	}

	if query == "" && *category == "" {
		// Nothing to search for: list the categories so the user can
		// drill down with --category.
		fmt.Fprintln(stdout, "categories:")
		for _, c := range compose.Categories() {
			fmt.Fprintf(stdout, "  %s\n", c)
		}
		return 0
	}

	entries := grammarHits(query, *category, *exact, *formMode)
	if len(entries) > 0 {
		printEntries(stdout, entries)
	}

	// A category listing is a grammar request; there is no lexicon
	// half to it. --form likewise asks what a surface form encodes.
	found := len(entries) > 0
	if query != "" && !*formMode {
		s := openStore(dataFile, stderr)
		if s == nil {
			return 1
		}
		defer s.Close()

		roots, err := s.SearchRoots(query, *limit)
		if err != nil {
			fmt.Fprintf(stderr, "search: root search: %v\n", err)
			return 1
		}
		affixes, err := s.SearchAffixes(query, *limit)
		if err != nil {
			fmt.Fprintf(stderr, "search: affix search: %v\n", err)
			return 1
		}
		if len(roots) > 0 {
			if found {
				fmt.Fprintln(stdout)
			}
			found = true
			fmt.Fprintln(stdout, "Roots:")
			for _, h := range roots {
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
		if len(affixes) > 0 {
			if found {
				fmt.Fprintln(stdout)
			}
			found = true
			fmt.Fprintln(stdout, "Affixes:")
			for _, a := range affixes {
				fmt.Fprintf(stdout, "  -%s- %s  %s\n", a.Cs, a.Abbrev, a.Description)
				for i, d := range a.Degrees {
					if d == "" {
						continue
					}
					fmt.Fprintf(stdout, "    %d. %s\n", i+1, d)
				}
			}
		}
	}

	if !found {
		fmt.Fprintln(stdout, "no matches")
	}
	return 0
}

// grammarHits resolves the grammar half of a search.
func grammarHits(query, category string, exact, formMode bool) []compose.Entry {
	if formMode {
		hits := compose.LookupForm(query)
		if category != "" {
			hits = filterByCategory(hits, category)
		}
		return hits
	}
	return compose.Filter(category, query, exact)
}

func filterByCategory(in []compose.Entry, cat string) []compose.Entry {
	out := make([]compose.Entry, 0, len(in))
	keep := compose.Filter(cat, "", false)
	allowed := map[string]struct{}{}
	for _, e := range keep {
		allowed[e.Category+"|"+e.Abbrev] = struct{}{}
	}
	for _, e := range in {
		if _, ok := allowed[e.Category+"|"+e.Abbrev]; ok {
			out = append(out, e)
		}
	}
	return out
}

func printEntries(w io.Writer, hits []compose.Entry) {
	catW, abW, nmW, fmW := 8, 4, 4, 4
	for _, h := range hits {
		if n := len(h.Category); n > catW {
			catW = n
		}
		if n := len(h.Abbrev); n > abW {
			abW = n
		}
		if n := len(h.Name); n > nmW {
			nmW = n
		}
		if n := len(h.Form); n > fmW {
			fmW = n
		}
	}
	fmt.Fprintf(w, "%-*s  %-*s  %-*s  %-*s  %s\n",
		catW, "CATEGORY", abW, "ABBR", nmW, "NAME", fmW, "FORM", "DESCRIPTION")
	for _, h := range hits {
		fmt.Fprintf(w, "%-*s  %-*s  %-*s  %-*s  %s\n",
			catW, h.Category, abW, h.Abbrev, nmW, h.Name, fmW, h.Form, h.Description)
	}
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
