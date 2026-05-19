package main

import (
	"fmt"
	"io"
	"strings"

	"github.com/coudard/ithkuil/go/compose"
)

// cmdSearch is the unified search across grammar, roots, and affixes.
// Each section of results gets its own labeled block.
func cmdSearch(args []string, stdout, stderr io.Writer, lexDir string) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, "usage: ithkuil-gloss --search QUERY")
		return 2
	}
	q := strings.Join(args, " ")
	gramHits := compose.SearchGrammar(q)
	var rootHits []compose.RootHit
	var affixHits []interface{ /* lexicon.AffixEntry */ } // type placeholder; populated below
	_ = affixHits

	lex := loadLex(lexDir, stderr)
	var roots, affixes string
	if lex != nil {
		rh := compose.SearchRoots(q, lex.Roots)
		ah := compose.SearchAffixes(q, lex.Affixes)
		if len(rh) > 0 {
			const capR = 15
			n := len(rh)
			if n > capR {
				n = capR
			}
			var b strings.Builder
			for _, h := range rh[:n] {
				fmt.Fprintf(&b, "  -%s-  %s\n", h.Cr, h.Entry.Stem1)
			}
			if len(rh) > capR {
				fmt.Fprintf(&b, "  ... %d more\n", len(rh)-capR)
			}
			roots = b.String()
			rootHits = rh
		}
		if len(ah) > 0 {
			const capA = 15
			n := len(ah)
			if n > capA {
				n = capA
			}
			var b strings.Builder
			for _, a := range ah[:n] {
				fmt.Fprintf(&b, "  %s/%s  %s\n", a.Cs, a.Abbrev, a.Description)
			}
			if len(ah) > capA {
				fmt.Fprintf(&b, "  ... %d more\n", len(ah)-capA)
			}
			affixes = b.String()
		}
	}

	any := false
	if len(gramHits) > 0 {
		fmt.Fprintln(stdout, "Grammar:")
		printEntries(stdout, gramHits)
		fmt.Fprintln(stdout)
		any = true
	}
	if roots != "" {
		fmt.Fprintln(stdout, "Roots:")
		fmt.Fprint(stdout, roots)
		fmt.Fprintln(stdout)
		any = true
	}
	if affixes != "" {
		fmt.Fprintln(stdout, "Affixes:")
		fmt.Fprint(stdout, affixes)
		any = true
	}
	if !any {
		fmt.Fprintf(stdout, "No results for: %s\n", q)
	}
	_ = rootHits
	return 0
}
