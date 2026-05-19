package main

import (
	"fmt"
	"io"
	"sort"
	"strings"

	"github.com/coudard/ithkuil/go/compose"
)

func cmdLookup(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, "usage: ithkuil-gloss --lookup ABBREV")
		return 2
	}
	q := strings.Join(args, " ")
	hits := compose.LookupGrammar(q)
	if len(hits) == 0 {
		fmt.Fprintf(stdout, "no grammar entry with abbreviation %q\n", q)
		return 0
	}
	printEntries(stdout, hits)
	return 0
}

func cmdForm(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, "usage: ithkuil-gloss --form FORM")
		return 2
	}
	q := strings.Join(args, " ")
	hits := compose.LookupForm(q)
	if len(hits) == 0 {
		fmt.Fprintf(stdout, "no grammar value with form %q\n", q)
		return 0
	}
	printEntries(stdout, hits)
	return 0
}

func cmdGrammar(stdout io.Writer) int {
	hits := append([]compose.Entry(nil), compose.Table...)
	sort.SliceStable(hits, func(i, j int) bool {
		if hits[i].Category != hits[j].Category {
			return hits[i].Category < hits[j].Category
		}
		return hits[i].Abbrev < hits[j].Abbrev
	})
	printEntries(stdout, hits)
	return 0
}

func printEntries(w io.Writer, hits []compose.Entry) {
	// Column widths: max category, max abbrev, form is last.
	catW, abbrW := 0, 0
	for _, h := range hits {
		if n := len(h.Category); n > catW {
			catW = n
		}
		if n := len(h.Abbrev); n > abbrW {
			abbrW = n
		}
	}
	for _, h := range hits {
		fmt.Fprintf(w, "%-*s  %-*s  %s\n", catW, h.Category, abbrW, h.Abbrev, h.Form)
	}
}
