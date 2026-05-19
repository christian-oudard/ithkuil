package main

import (
	"fmt"
	"io"
	"strings"

	g "github.com/coudard/ithkuil/go/grammar"
)

func cmdBiases(args []string, stdout io.Writer) int {
	needle := strings.ToLower(strings.Join(args, " "))
	matches := func(b g.Bias) bool {
		if needle == "" {
			return true
		}
		return strings.Contains(strings.ToLower(b.String()), needle) ||
			strings.Contains(strings.ToLower(g.BiasExpression(b)), needle) ||
			strings.Contains(strings.ToLower(g.BiasForm(b)), needle)
	}
	var hits []g.Bias
	for _, b := range g.AllBiases {
		if matches(b) {
			hits = append(hits, b)
		}
	}
	if len(hits) == 0 {
		fmt.Fprintf(stdout, "no biases match %q\n", needle)
		return 0
	}
	// Column widths.
	abbrW, formW := 4, 4
	for _, b := range hits {
		if n := len(b.String()); n > abbrW {
			abbrW = n
		}
		if n := len(g.BiasForm(b)); n > formW {
			formW = n
		}
	}
	fmt.Fprintf(stdout, "%-*s  %-*s  %s\n", abbrW, "ABBR", formW, "FORM", "GLOSS")
	for _, b := range hits {
		fmt.Fprintf(stdout, "%-*s  %-*s  %s\n",
			abbrW, b.String(), formW, g.BiasForm(b), g.BiasExpression(b))
	}
	return 0
}
