package main

import (
	"fmt"
	"io"
	"strings"

	"github.com/coudard/ithkuil/go/gloss"
	"github.com/coudard/ithkuil/go/inspect"
	"github.com/coudard/ithkuil/go/tokenize"
)

// cmdAnalyze tokenizes the input and renders a learner-oriented
// breakdown of each formative: phonetic segmentation paired with a
// glossary that expands every code. --short collapses each word to a
// single surface/type/gloss line; --polygraph renders the multi-column
// trace table.
func cmdAnalyze(args []string, stdin io.Reader, stdout, stderr io.Writer, lexDir string) int {
	fs := newFlagSet("analyze", stderr)
	fs.describe("Tokenize, parse, and gloss each word (detailed by default).", "TEXT...")
	short := fs.Bool("short", "s", false, "one-line surface · type · gloss view")
	polygraph := fs.Bool("polygraph", "p", false, "render as a multi-column slot polygraph")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	rest := fs.Args()

	text := strings.Join(rest, " ")
	if text == "" {
		s, err := readStdin(stdin)
		if err != nil {
			fmt.Fprintf(stderr, "stdin: %v\n", err)
			return 2
		}
		text = s
	}
	if text == "" {
		fmt.Fprintln(stderr, "usage: ithkuil analyze TEXT... (or pipe via stdin)")
		return 2
	}

	tokens := tokenize.Tokenize(text)
	if len(tokens) == 0 {
		return 0
	}
	lex := loadLex(lexDir, stderr)
	glosser := gloss.Glosser{Lex: lex}

	switch {
	case *polygraph:
		inspect.Polygraph(stdout, tokens)
		return 0
	case *short:
		for _, t := range tokens {
			fmt.Fprintf(stdout, "%s  %s  %s\n", t.Surface(), inspect.Type(t), glosser.Token(t))
		}
		return 0
	}

	// Detailed view.
	for i, t := range tokens {
		if i > 0 {
			fmt.Fprintln(stdout)
		}
		renderDetailed(stdout, t, lex, glosser)
	}
	return 0
}

func renderDetailed(w io.Writer, t tokenize.WordToken, lex interface{}, glosser gloss.Glosser) {
	fw, ok := t.(tokenize.FormativeWord)
	if !ok {
		fmt.Fprintf(w, "%s  %s  %s\n", t.Surface(), inspect.Type(t), glosser.Token(t))
		return
	}
	head := inspect.Headword(fw.Formative, glosser.Lex)
	segs := inspect.Segments(fw.Text, fw.Formative, glosser.Lex)
	glossary := inspect.Glossary(fw.Text, fw.Formative, segs, glosser.Lex)

	fmt.Fprintln(w, strings.ToLower(fw.Text))
	if head.Code != "" {
		if head.Meaning != "" {
			fmt.Fprintf(w, "  %s — %s\n", head.Code, head.Meaning)
		} else {
			fmt.Fprintf(w, "  %s\n", head.Code)
		}
	}
	fmt.Fprintln(w)
	renderPhoneticTable(w, segs)
	fmt.Fprintln(w)
	renderGlossaryTable(w, glossary)
}

func renderPhoneticTable(w io.Writer, segs []inspect.Segment) {
	phW, slW := len("PHONETIC"), len("SLOT")
	for _, s := range segs {
		if n := runeWidth(s.Chunk); n > phW {
			phW = n
		}
		if n := runeWidth(s.Slot); n > slW {
			slW = n
		}
	}
	fmt.Fprintf(w, "%-*s  %-*s  %s\n",
		phW, "PHONETIC", slW, "SLOT", "ENCODES")
	for _, s := range segs {
		encodes := strings.Join(s.Encodes, " / ")
		fmt.Fprintf(w, "%-*s  %-*s  %s\n",
			phW, padRunes(s.Chunk, phW), slW, padRunes(s.Slot, slW), encodes)
	}
}

func renderGlossaryTable(w io.Writer, entries []inspect.GlossaryEntry) {
	catW, codeW, nameW := len("CATEGORY"), len("CODE"), len("NAME")
	for _, e := range entries {
		if n := runeWidth(e.Category); n > catW {
			catW = n
		}
		if n := runeWidth(e.Code); n > codeW {
			codeW = n
		}
		if n := runeWidth(e.Name); n > nameW {
			nameW = n
		}
	}
	fmt.Fprintf(w, "%-*s  %-*s  %-*s  %s\n",
		catW, "CATEGORY", codeW, "CODE", nameW, "NAME", "MEANING")
	for _, e := range entries {
		fmt.Fprintf(w, "%-*s  %-*s  %-*s  %s\n",
			catW, padRunes(e.Category, catW),
			codeW, padRunes(e.Code, codeW),
			nameW, padRunes(e.Name, nameW),
			e.Meaning)
	}
}

// runeWidth returns the visible width (rune count) of s, treating each
// rune as one column. Good enough for ASCII + common Latin diacritics
// + subscripts.
func runeWidth(s string) int {
	return len([]rune(s))
}

// padRunes right-pads s with spaces to width w, measured in runes.
func padRunes(s string, w int) string {
	r := runeWidth(s)
	if r >= w {
		return s
	}
	return s + strings.Repeat(" ", w-r)
}

