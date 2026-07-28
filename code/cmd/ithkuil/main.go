// Command ithkuil is the command-line interface for working with the
// Ithkuil V4 language: parse text, compose words, look up the grammar
// inventory, search the lexicon, validate phonotactics.
//
// Usage: ithkuil [-data FILE] <subcommand> [args...]
//
// Subcommands (mirror the ithkuil-mcp tools one-for-one):
//
//	analyze TEXT...   Tokenize, parse, and gloss each word.
//	compose ROOT ...  Build a surface formative from grammar choices.
//	define WORD...    Look up an English word as Ithkuil lexical cores.
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
	"strings"

	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/store"
	"github.com/christian-oudard/ithkuil/surface"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdin, os.Stdout, os.Stderr))
}

// run is the testable entry point.
func run(args []string, stdin io.Reader, stdout, stderr io.Writer) int {
	dataFile, args := extractDataFlag(args)
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
		return cmdAnalyze(rest, stdin, stdout, stderr, dataFile)
	case "compose":
		return cmdCompose(rest, stdout, stderr, dataFile)
	case "define":
		return cmdDefine(rest, stdout, stderr, dataFile)
	case "grammar":
		return cmdGrammar(rest, stdout, stderr)
	case "lexicon":
		return cmdLexicon(rest, stdout, stderr, dataFile)
	case "validate":
		return cmdValidate(rest, stdout, stderr)
	default:
		fmt.Fprintf(stderr, "unknown subcommand %q\n\n%s", sub, usage)
		return 2
	}
}

const usage = `usage: ithkuil [--data FILE] <subcommand> [args...]

Subcommands:
  analyze TEXT...    Tokenize, parse, and gloss each word (detailed).
                       --short / -s        one-line surface/type/gloss
  compose EXPR       Build a surface formative from a gloss-style
                     expression. Slots separated by "-"; sub-fields
                     by "/" or "." (for Ca). Affixes before the Ca
                     land in Slot V; write "{Ca}" for an all-default
                     Ca that still needs to mark that boundary.
                     Examples: "ml", "S2/CPT-ml-ERG",
                     "S2/CPT-ml-DYN/OBJ-MSS.G-DEV/3-ERG",
                     "m-SYS/5_2-{Ca}-DCD/1_2".
  define WORD...     Look up an English word as Ithkuil lexical cores.
                       --limit / -n  N
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
  --data / -d FILE   Path to data.db
                     (default: $XDG_DATA_HOME/ithkuil/data.db).
`

// extractDataFlag walks args for `--data FILE` (or `-data` or short `-d`)
// and returns the value plus the args with the flag removed.
func extractDataFlag(args []string) (string, []string) {
	file := ""
	out := make([]string, 0, len(args))
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "-d", "-data", "--data":
			if i+1 < len(args) {
				file = args[i+1]
				i++
				continue
			}
		default:
			out = append(out, args[i])
		}
	}
	if file == "" {
		file = store.DefaultPath()
	}
	return file, out
}

// openStore opens the SQLite database at file. Returns nil on error
// (with a warning to stderr).
func openStore(file string, stderr io.Writer) *store.Store {
	s, err := store.Open(file)
	if err != nil {
		fmt.Fprintf(stderr, "warning: cannot open data store %s (%v)\n", file, err)
		return nil
	}
	return s
}

// loadLex opens the store and reads all roots/affixes into memory.
// Returns nil on error (with a warning to stderr). Most subcommands
// tolerate a nil lexicon and degrade their output accordingly.
func loadLex(dataFile string, stderr io.Writer) *lexicon.Lexicon {
	s := openStore(dataFile, stderr)
	if s == nil {
		return nil
	}
	defer s.Close()
	lex, err := lexicon.LoadFromStore(s)
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
