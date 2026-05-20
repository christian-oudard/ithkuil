// Command ithkuil is the command-line interface for working with the
// Ithkuil V4 language: parse text, compose words, look up the grammar
// inventory, search the lexicon, diff formatives, validate
// phonotactics.
//
// Usage: ithkuil [-lex DIR] <subcommand> [args...]
//
// Subcommands (mirror the ithkuil-mcp tools one-for-one):
//
//	analyze TEXT...   Tokenize, parse, and gloss each word.
//	compose ROOT ...  Build a surface formative from grammar choices.
//	diff A B          Slot-by-slot diff between two formatives.
//	grammar [Q]       Look up the grammar inventory.
//	lexicon Q         Substring search the root and/or affix lexicons.
//	validate TEXT...  Phonotactic validation per word.
//	help              Show this usage block.
//
// Run `ithkuil <sub> --help` for per-subcommand flags.
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

// run is the testable entry point.
func run(args []string, stdin io.Reader, stdout, stderr io.Writer) int {
	lexDir, args := extractLexFlag(args)
	if len(args) == 0 {
		fmt.Fprint(stderr, usage)
		return 2
	}
	sub, rest := args[0], args[1:]
	switch sub {
	case "help", "-h", "--help":
		fmt.Fprint(stdout, usage)
		return 0
	case "analyze":
		return cmdAnalyze(rest, stdin, stdout, stderr, lexDir)
	case "compose":
		return cmdCompose(rest, stdout, stderr, lexDir)
	case "diff":
		return cmdDiff(rest, stdout, stderr)
	case "grammar":
		return cmdGrammar(rest, stdout, stderr)
	case "lexicon":
		return cmdLexicon(rest, stdout, stderr, lexDir)
	case "validate":
		return cmdValidate(rest, stdout, stderr)
	default:
		fmt.Fprintf(stderr, "unknown subcommand %q\n\n%s", sub, usage)
		return 2
	}
}

const usage = `usage: ithkuil [--lex DIR] <subcommand> [args...]

Subcommands:
  analyze TEXT...    Tokenize, parse, and gloss each word (detailed).
                       --short / -s        one-line surface/type/gloss
                       --polygraph / -p    multi-column slot polygraph
  compose ROOT ...   Build a surface formative from grammar choices.
                       --stem --version --function --specification
                       --context --case --aspect --valence --mood
                       --illocution --stress
  diff A B           Slot-by-slot diff (single word pair).
  diff A... -- B...  Slot-by-slot diff (aligned sentences).
  grammar [Q]        Look up grammar inventory.
                       --category / -c CAT
                       --exact    / -e
                       --form     / -f
  lexicon Q          Search root and/or affix lexicons.
                       --kind  / -k  root|affix|both
                       --limit / -n  N
  validate TEXT...   Phonotactic validation per word.
  help               Show this help.

Global flags:
  --lex / -l DIR     Lexicon directory (default ./data).
`

// extractLexFlag walks args for `--lex DIR` (or `-lex` or short `-l`)
// and returns the value plus the args with the flag removed. Defaults
// to "./data".
func extractLexFlag(args []string) (string, []string) {
	def := "./data"
	out := make([]string, 0, len(args))
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "-l", "-lex", "--lex":
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

// loadLex returns the lexicon at the given directory, or nil on
// failure (with a warning to stderr). Most subcommands tolerate a
// nil lexicon and degrade their output accordingly.
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
