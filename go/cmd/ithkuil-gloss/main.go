// Command ithkuil-gloss is the Go counterpart to the Haskell app/Gloss.hs:
// a multi-mode tool that parses, glosses, validates, searches,
// composes, and traces Ithkuil V4 morphology.
//
// Subcommands (a leading `--word`):
//
//	--help              Show this help.
//	--lookup ABBR       Show grammar entries with an exact abbreviation match.
//	--form FORM         Show every grammar value that uses a given surface form.
//	--grammar           Dump the entire grammar table.
//	--root QUERY        Substring-search the root lexicon.
//	--affix QUERY       Substring-search the affix lexicon.
//	--biases [QUERY]    List bias adjuncts (optional filter).
//	--trace WORDS...    Per-slot polygraph view across a sentence.
//	--diff A B          Slot-by-slot diff between two formatives.
//	                     Use `--diff A... -- B...` for sentences.
//	--search QUERY      Unified search across grammar/roots/affixes.
//	--compose ROOT ...  Build a formative from a root + grammar flags.
//	--validate WORDS... Gloss + run phonotactic validation.
//
// With no subcommand the arguments (or stdin) are treated as a sentence
// to gloss. -lex DIR loads the root/affix lexicon for any subcommand
// that needs it; defaults to ./data if not given.
package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/coudard/ithkuil/go/lexicon"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdin, os.Stdout, os.Stderr))
}

// run is the testable entry point. Returns a process exit code.
func run(args []string, stdin io.Reader, stdout, stderr io.Writer) int {
	lexDir, args := extractLexFlag(args)
	doValidate, args := extractFlag(args, "-validate", "--validate")

	if len(args) == 0 {
		return defaultMode(stdin, stdout, stderr, lexDir, doValidate)
	}

	switch args[0] {
	case "--help", "-h":
		printUsage(stdout)
		return 0
	case "--lookup":
		return cmdLookup(args[1:], stdout, stderr)
	case "--form":
		return cmdForm(args[1:], stdout, stderr)
	case "--grammar":
		return cmdGrammar(stdout)
	case "--root":
		return cmdRoot(args[1:], stdout, stderr, lexDir)
	case "--affix":
		return cmdAffix(args[1:], stdout, stderr, lexDir)
	case "--biases":
		return cmdBiases(args[1:], stdout)
	case "--trace":
		return cmdTrace(args[1:], stdout, stderr, lexDir)
	case "--diff":
		return cmdDiff(args[1:], stdout, stderr)
	case "--search":
		return cmdSearch(args[1:], stdout, stderr, lexDir)
	case "--compose":
		return cmdCompose(args[1:], stdout, stderr, lexDir)
	default:
		// Bare arguments → gloss as a sentence.
		return defaultMode(strings.NewReader(strings.Join(args, " ")), stdout, stderr, lexDir, doValidate)
	}
}

func printUsage(w io.Writer) {
	fmt.Fprint(w, `usage: ithkuil-gloss [-lex DIR] [-validate] [SUBCOMMAND] [ARGS...]

Subcommands:
  --help              Show this help.
  --lookup ABBR       Find grammar entries by exact abbreviation.
  --form FORM         Reverse-lookup a surface form to grammar values.
  --grammar           Dump the entire grammar inventory.
  --root QUERY        Substring search the root lexicon (Cr or stem).
  --affix QUERY       Substring search the affix lexicon.
  --biases [QUERY]    List every bias adjunct (optional filter).
  --trace WORDS...    Per-slot polygraph across a sentence.
  --diff A B          Slot-by-slot diff between two formatives.
  --diff A... -- B... Slot-by-slot diff between two sentences.
  --search QUERY      Unified search (grammar + roots + affixes).
  --compose ROOT FLAGS...
                      Build a formative from a root and grammar flags.

With no subcommand, arguments are glossed as a sentence. With no
arguments, sentences are read from stdin one line at a time.

Flags:
  -lex DIR            Directory holding roots.json and affixes.json
                      (default ./data).
  -validate           In default mode, also run phonotactic validation;
                      flag rule violations on stderr.
`)
}

// extractLexFlag walks args for `-lex DIR` (or --lex) and returns the
// value plus the args with the flag removed. Defaults to "./data".
func extractLexFlag(args []string) (string, []string) {
	def := "./data"
	out := make([]string, 0, len(args))
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "-lex", "--lex":
			if i+1 < len(args) {
				def = args[i+1]
				i++
				continue
			}
		default:
			out = append(out, args[i])
		}
	}
	return def, out
}

// extractFlag removes a boolean flag matching any of names from args
// and returns (true, leftover) if any was present.
func extractFlag(args []string, names ...string) (bool, []string) {
	out := make([]string, 0, len(args))
	hit := false
	for _, a := range args {
		matched := false
		for _, n := range names {
			if a == n {
				hit = true
				matched = true
				break
			}
		}
		if !matched {
			out = append(out, a)
		}
	}
	return hit, out
}

// loadLex returns the lexicon at the given directory. On error it
// returns nil and prints a warning to stderr — most subcommands work
// without a lexicon, just with less informative output.
func loadLex(dir string, stderr io.Writer) *lexicon.Lexicon {
	lex, err := lexicon.Load(
		filepath.Join(dir, "roots.json"),
		filepath.Join(dir, "affixes.json"),
	)
	if err != nil {
		fmt.Fprintf(stderr, "warning: lexicon load failed (%v); continuing without lexicon\n", err)
		return nil
	}
	return lex
}

// readStdin returns the trimmed contents of r.
func readStdin(r io.Reader) (string, error) {
	b, err := io.ReadAll(bufio.NewReader(r))
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(b)), nil
}
