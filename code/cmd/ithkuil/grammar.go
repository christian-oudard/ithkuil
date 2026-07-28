package main

import (
	"fmt"
	"io"

	"github.com/christian-oudard/ithkuil/compose"
)

// cmdGrammar prints inventory rows. With no args, lists categories.
// With --category CAT, lists all entries in that category. With a
// positional query, substring-matches across abbrev/category/form/
// description; --exact narrows to exact abbreviation match; --form
// treats the query as a surface form (exact match against the Form
// column).
//
// Usage: ithkuil grammar [Q] [--category CAT] [--exact] [--form]
func cmdGrammar(args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("grammar", stderr)
	fs.describe("Look up the grammar inventory by code, name, category, or surface form.", "[QUERY]")
	category := fs.String("category", "c", "", "CAT", "list only this category (Case, Aspect, Bias, …)")
	exact := fs.Bool("exact", "e", false, "exact abbreviation match against the query")
	formMode := fs.Bool("form", "f", false, "treat the query as a surface form (vowel/consonant)")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	rest := fs.Args()

	if *category == "" && len(rest) == 0 && !*formMode {
		// No query, no category: list available categories so the
		// user can drill down.
		fmt.Fprintln(stdout, "categories:")
		for _, c := range compose.Categories() {
			fmt.Fprintf(stdout, "  %s\n", c)
		}
		return 0
	}

	var hits []compose.Entry
	switch {
	case *formMode:
		if len(rest) == 0 {
			fmt.Fprintln(stderr, "grammar --form requires a query")
			return 2
		}
		hits = compose.LookupForm(rest[0])
		if *category != "" {
			hits = filterByCategory(hits, *category)
		}
	default:
		query := ""
		if len(rest) > 0 {
			query = rest[0]
		}
		hits = compose.Filter(*category, query, *exact)
	}

	if len(hits) == 0 {
		fmt.Fprintln(stdout, "no matches")
		return 0
	}
	printEntries(stdout, hits)
	return 0
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
