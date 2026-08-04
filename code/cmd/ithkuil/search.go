package main

import (
	"fmt"
	"io"
	"strings"

	"github.com/christian-oudard/ithkuil/api"
	"github.com/christian-oudard/ithkuil/search"
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
	formMode := fs.Bool("form", "f", false, "grammar: treat the query as a written form (vowel/consonant)")
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
		for _, c := range search.Categories() {
			fmt.Fprintf(stdout, "  %s\n", c)
		}
		return 0
	}

	a, done := loadAPI(dataFile, stderr)
	defer done()
	got := a.Search(query, api.SearchOptions{
		Category: *category, Exact: *exact, Form: *formMode, Limit: *limit,
	})
	entries := got.Grammar
	if len(entries) > 0 {
		printEntries(stdout, entries)
		// A single hit is a request to read about one value, so the
		// authored notes follow the row. In a list they would bury it.
		if len(entries) == 1 {
			printNotes(stdout, entries[0], stdout)
		}
	}

	// A category listing is a grammar request; there is no lexicon
	// half to it. --form likewise asks what a written form encodes.
	found := len(entries) > 0
	if len(got.Roots) > 0 {
		if found {
			fmt.Fprintln(stdout)
		}
		found = true
		fmt.Fprintln(stdout, "Roots:")
		for _, h := range got.Roots {
			fmt.Fprintf(stdout, "  -%s-\n", h.Root.Cr)
			for i, label := range []string{"S0", "S1", "S2", "S3"} {
				if i < len(h.Root.Stems) {
					printStem(stdout, label, h.Root.Stems[i])
				}
			}
			printStem(stdout, "CTE", h.Root.Contential)
			printStem(stdout, "CSV", h.Root.Constitutive)
			printTrio(stdout, "OBJ", h.Root.Objective)
			printTrio(stdout, "CPT", h.Root.Completive)
			printStem(stdout, "DYN", h.Root.Dynamic)
			printTrio(stdout, "Wikidata", h.Root.Wikidata)
		}
	}
	if len(got.Affixes) > 0 {
		if found {
			fmt.Fprintln(stdout)
		}
		found = true
		fmt.Fprintln(stdout, "Affixes:")
		for _, af := range got.Affixes {
			fmt.Fprintf(stdout, "  -%s- %s  %s\n", af.Cs, af.Abbrev, af.Description)
			for i, d := range af.Degrees {
				if d == "" {
					continue
				}
				fmt.Fprintf(stdout, "    %d. %s\n", i+1, d)
			}
		}
	}

	if !found {
		fmt.Fprintln(stdout, "no matches")
	}
	return 0
}

// grammarHits resolves the grammar half of a search.
func grammarHits(query, category string, exact, formMode bool) []search.Entry {
	if formMode {
		hits := search.LookupForm(query)
		if category != "" {
			hits = filterByCategory(hits, category)
		}
		return hits
	}
	return search.Filter(category, query, exact)
}

func filterByCategory(in []search.Entry, cat string) []search.Entry {
	out := make([]search.Entry, 0, len(in))
	keep := search.Filter(cat, "", false)
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

func printEntries(w io.Writer, hits []api.GrammarEntry) {
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

// printNotes writes the fuller reading of a value and how it lands in
// English, both of which live in the store rather than in the compiled
// inventory: they are authored, and the inventory is derived.
func printNotes(w io.Writer, e api.GrammarEntry, _ io.Writer) {
	if e.Explanation != "" {
		fmt.Fprintf(w, "\n%s\n", wrapAt(e.Explanation, 72, ""))
	}
	if e.Guidance != "" {
		fmt.Fprintf(w, "\nIn English: %s\n", wrapAt(e.Guidance, 60, "  "))
	}
}

// wrapAt fills text to width, indenting every line after the first.
func wrapAt(text string, width int, indent string) string {
	var out, line []string
	n := 0
	for _, word := range strings.Fields(text) {
		if n > 0 && n+len(word)+1 > width {
			out = append(out, strings.Join(line, " "))
			line, n = nil, 0
		}
		line = append(line, word)
		n += len(word) + 1
	}
	if len(line) > 0 {
		out = append(out, strings.Join(line, " "))
	}
	return strings.Join(out, "\n"+indent)
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
