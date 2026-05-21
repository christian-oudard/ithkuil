// Command ithkuil is the command-line interface for working with the
// Ithkuil V4 language: parse text, compose words, look up the grammar
// inventory, search the lexicon, validate phonotactics.
//
// Usage: ithkuil [-lex DIR] <subcommand> [args...]
//
// Subcommands (mirror the ithkuil-mcp tools one-for-one):
//
//	analyze TEXT...   Tokenize, parse, and gloss each word.
//	compose ROOT ...  Build a surface formative from grammar choices.
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

	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/surface"
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
  compose EXPR       Build a surface formative from a gloss-style
                     expression. Slots separated by "-"; sub-fields
                     by "/" or "." (for Ca). Examples: "ml",
                     "S2/CPT-ml-ERG", "S2/CPT-ml-DYN/OBJ-MSS.G-DEV/3-ERG".
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
  --lex / -l DIR     Override the embedded lexicon with one read from DIR
                     (expects roots.json and affixes.json).
`

// extractLexFlag walks args for `--lex DIR` (or `-lex` or short `-l`)
// and returns the value plus the args with the flag removed. An empty
// string means "use the embedded lexicon".
func extractLexFlag(args []string) (string, []string) {
	dir := ""
	out := make([]string, 0, len(args))
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "-l", "-lex", "--lex":
			if i+1 < len(args) {
				dir = args[i+1]
				i++
				continue
			}
		default:
			out = append(out, args[i])
		}
	}
	return dir, out
}

// loadLex returns the lexicon: the embedded copy when dir is empty,
// otherwise read from dir/roots.json and dir/affixes.json. Nil on
// failure (with a warning to stderr); most subcommands tolerate a nil
// lexicon and degrade their output accordingly.
func loadLex(dir string, stderr io.Writer) *lexicon.Lexicon {
	var lex *lexicon.Lexicon
	var err error
	if dir == "" {
		lex, err = lexicon.LoadDefault()
	} else {
		lex, err = lexicon.Load(
			filepath.Join(dir, "roots.json"),
			filepath.Join(dir, "affixes.json"),
		)
	}
	if err != nil {
		fmt.Fprintf(stderr, "warning: lexicon load failed (%v); continuing without lexicon\n", err)
		return nil
	}
	return lex
}

// normalizeASCII applies the ASCII input method per whitespace-
// separated token in text. Lets users type "maleeut,rqait" on the
// command line and have it parsed as "malëuţřait". FromASCII is
// idempotent on Unicode Ithkuil text, so Unicode input passes
// through unchanged.
func normalizeASCII(text string) string {
	fields := strings.Fields(text)
	for i, f := range fields {
		fields[i] = surface.FromASCII(f)
	}
	return strings.Join(fields, " ")
}

// readStdin returns the trimmed contents of r.
func readStdin(r io.Reader) (string, error) {
	b, err := io.ReadAll(bufio.NewReader(r))
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(b)), nil
}
