package main

import (
	"fmt"
	"io"
	"strings"

	"github.com/coudard/ithkuil/go/gloss"
	"github.com/coudard/ithkuil/go/tokenize"
	"github.com/coudard/ithkuil/go/validation"
)

// defaultMode glosses a sentence read from stdin (one per line) or
// passed as a single argument. Each word is classified by tokenize
// and rendered by gloss.
func defaultMode(stdin io.Reader, stdout, stderr io.Writer, lexDir string, doValidate bool) int {
	g := &gloss.Glosser{Lex: loadLex(lexDir, stderr)}

	text, err := readStdin(stdin)
	if err != nil {
		fmt.Fprintf(stderr, "read stdin: %v\n", err)
		return 1
	}
	if text == "" {
		printUsage(stderr)
		return 2
	}
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		tokens := tokenize.Tokenize(line)
		width := maxSurfaceWidth(tokens)
		for _, t := range tokens {
			fmt.Fprintf(stdout, "%-*s  %s\n", width, t.Surface(), g.Token(t))
			if doValidate {
				r := validation.ValidateWord(t.Surface())
				for _, e := range r.Errors {
					fmt.Fprintf(stderr, "%s: %s\n", t.Surface(), e)
				}
			}
		}
	}
	return 0
}

func maxSurfaceWidth(tokens []tokenize.WordToken) int {
	w := 0
	for _, t := range tokens {
		if n := len([]rune(t.Surface())); n > w {
			w = n
		}
	}
	return w
}
