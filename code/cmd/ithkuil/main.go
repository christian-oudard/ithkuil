// Command ithkuil is the command-line interface for working with the
// Ithkuil V4 language: parse text, compose words, search the grammar
// inventory and the lexicon.
//
// Usage: ithkuil [-data FILE] <subcommand> [args...]
//
// Subcommands:
//
//	parse TEXT...     Tokenize, parse, and gloss each word.
//	compare A B       Diff two words slot by slot.
//	compose EXPR      Build a word from grammar choices.
//	search [Q]        Look up a term in the grammar and the lexicon.
//	define WORD...    Look up an English word as Ithkuil lexical cores.
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

	"github.com/christian-oudard/ithkuil/api"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/store"
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
	case "parse":
		return cmdParse(rest, stdin, stdout, stderr, dataFile)
	case "compare":
		return cmdCompare(rest, stdout, stderr, dataFile)
	case "compose":
		return cmdCompose(rest, stdout, stderr, dataFile)
	case "search":
		return cmdSearch(rest, stdout, stderr, dataFile)
	case "define":
		return cmdDefine(rest, stdout, stderr, dataFile)
	default:
		fmt.Fprintf(stderr, "unknown subcommand %q\n\n%s", sub, usage)
		return 2
	}
}

const usage = `usage: ithkuil [--data FILE] <subcommand> [args...]

Subcommands:
  parse TEXT...      Tokenize, parse, and gloss each word (detailed).
                     Phonotactics are checked first: an unpronounceable
                     word is reported with the rule it breaks, and the
                     exit status is 1.
                       --short / -s        one-line romanization/type/gloss
  compare A B        Lay two words' slot breakdowns side by side and
                     mark what differs.
  compose EXPR       Build a word from a gloss-style
                     expression. "-" separates slots, "." joins
                     category values in a slot, "/" binds a degree
                     or a case to a head. Affixes before the Ca
                     land in Slot V; write "{Ca}" for an all-default
                     Ca that still needs to mark that boundary.
                     Examples: "ml", "S2.CPT-ml-ERG",
                     "S2.CPT-ml-DYN.OBJ-MSS.G-DEV/3-ERG",
                     "m-SYS/5_2-{Ca}-DCD/1_2".
  search [Q]         Look a term up in the grammar inventory and the
                     lexicon at once, grammar hits first. With no
                     query, lists the grammar categories.
                       --category / -c CAT
                       --exact    / -e
                       --form     / -f
                       --limit    / -n  N
  define WORD...     Look up an English word as Ithkuil lexical cores.
                       --limit / -n  N
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

// loadAPI builds the api every subcommand answers through, with the
// store's full-text index wired in. That index ranks by word where the
// in-memory scan matches a substring anywhere, so "cat" does not answer
// with "indicate"; only a browser has to do without it.
//
// The caller closes the store. A store that will not open is a warning
// rather than an exit, as it is for the MCP server: the grammar tables
// are compiled in, so everything but the lexicon half still answers.
func loadAPI(dataFile string, stderr io.Writer) (*api.API, func()) {
	a := api.New()
	st := openStore(dataFile, stderr)
	if st == nil {
		return a, func() {}
	}
	lex, err := store.LoadLexicon(st)
	if err != nil {
		fmt.Fprintf(stderr, "warning: lexicon load failed (%v); continuing without lexicon\n", err)
	} else {
		a.SetLexicon(lex)
	}
	if notes, topics, err := st.Notes(); err == nil {
		entries := make([]api.GrammarEntry, len(notes))
		for i, n := range notes {
			entries[i] = api.GrammarEntry{
				Abbrev: n.Abbrev, Explanation: n.Explanation, Guidance: n.Guidance,
			}
		}
		ts := make([]api.Topic, len(topics))
		for i, t := range topics {
			ts[i] = api.Topic{
				Key: t.Key, Category: t.Category, Name: t.Name,
				Explanation: t.Explanation, Guidance: t.Guidance,
			}
		}
		a.SetNotes(entries, ts)
	}
	a.SetLexiconSearch(store.NewSearcher(st))
	return a, func() { st.Close() }
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
	lex, err := store.LoadLexicon(s)
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
