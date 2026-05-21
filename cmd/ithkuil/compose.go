package main

import (
	"fmt"
	"io"
	"strings"

	"github.com/christian-oudard/ithkuil/compose"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/render"
)

// cmdCompose builds a formative from a root and named grammar flags,
// or from a single gloss-style expression. Each option corresponds to
// one slot value (stem, version, case, etc.). See compose.ApplyFlag
// for the recognized abbreviations and compose.ParseString for the
// expression syntax.
//
// Usage:
//
//	ithkuil compose [--stem S2] [--case ERG] ... ROOT
//	ithkuil compose 'S2/CPT-ROOT-DYN/OBJ-DEV/3-ERG'
//
// A positional argument containing "-" or "/" is parsed as a compose
// expression and any flags are ignored. Otherwise the bare argument
// is treated as a root cluster and flags supply each slot value.
//
// Flags must come before the positional ROOT (Go stdlib flag stops
// parsing at the first positional argument).
func cmdCompose(args []string, stdout, stderr io.Writer, lexDir string) int {
	fs := newFlagSet("compose", stderr)
	fs.describe("Build a surface formative from a root and named slot values.", "ROOT")
	stem := fs.String("stem", "", "", "S0|S1|S2|S3", "stem (default S1)")
	version := fs.String("version", "", "", "PRC|CPT", "version (default PRC)")
	function := fs.String("function", "", "", "STA|DYN", "function (default STA)")
	specification := fs.String("specification", "", "", "BSC|CTE|CSV|OBJ", "specification (default BSC)")
	context := fs.String("context", "", "", "EXS|FNC|RPS|AMG", "context (default EXS)")
	caseFlag := fs.String("case", "", "", "CASE", "any of the 68 cases (default THM)")
	aspect := fs.String("aspect", "", "", "ASPECT", "Slot VIII aspect")
	valence := fs.String("valence", "", "", "VAL", "Slot VIII valence")
	mood := fs.String("mood", "", "", "MOOD", "Slot VIII mood")
	illocution := fs.String("illocution", "", "", "ILL", "Slot IX illocution (forces ULT stress)")
	stress := fs.String("stress", "", "", "MON|PEN|ULT|ANT", "stress (default PEN)")
	gloss_ := fs.Bool("gloss", "g", false, "also print the formative gloss")

	if err := fs.Parse(args); err != nil {
		return 2
	}
	rest := fs.Args()
	if len(rest) == 0 {
		fmt.Fprintln(stderr, "usage: ithkuil compose ROOT [--flag VALUE ...]")
		return 2
	}
	if len(rest) > 1 {
		fmt.Fprintf(stderr, "compose: unexpected extra args %q (use --flag VALUE syntax)\n", rest[1:])
		return 2
	}
	root := rest[0]

	var f g.Formative
	// A hyphen or slash in the positional argument means it's a
	// compose-syntax expression, not a bare root cluster. Parse it
	// whole; flags are ignored in this mode.
	if strings.ContainsAny(root, "-/") {
		lex := loadLex(lexDir, stderr)
		var affixes map[string]lexicon.AffixEntry
		if lex != nil {
			affixes = lex.Affixes
		}
		parsed, err := compose.ParseString(root, affixes)
		if err != nil {
			fmt.Fprintf(stderr, "compose: %v\n", err)
			return 2
		}
		f = parsed
	} else {
		f = g.MinimalFormative(root)
		// Apply each non-empty flag via the shared compose.ApplyFlag.
		for _, v := range []string{
			*stem, *version, *function, *specification, *context,
			*caseFlag, *aspect, *valence, *mood, *illocution, *stress,
		} {
			if v == "" {
				continue
			}
			if err := compose.ApplyFlag(&f, v); err != nil {
				fmt.Fprintf(stderr, "compose: %v\n", err)
				return 2
			}
		}
	}

	surface := render.Formative(f)
	fmt.Fprintln(stdout, surface)
	if *gloss_ {
		gl := gloss.Glosser{Lex: loadLex(lexDir, stderr)}
		fmt.Fprintln(stdout, gl.Formative(f))
	}
	return 0
}
