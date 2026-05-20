package main

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/inspect"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// indentedWriter wraps an io.Writer and prefixes every non-empty
// line with the configured indent. Blank lines pass through clean
// so the output doesn't carry trailing whitespace.
type indentedWriter struct {
	w           io.Writer
	indent      []byte
	atLineStart bool
}

func indented(w io.Writer, indent string) *indentedWriter {
	return &indentedWriter{w: w, indent: []byte(indent), atLineStart: true}
}

func (iw *indentedWriter) Write(p []byte) (int, error) {
	total := 0
	for len(p) > 0 {
		if iw.atLineStart {
			if p[0] != '\n' {
				if _, err := iw.w.Write(iw.indent); err != nil {
					return total, err
				}
			}
			iw.atLineStart = false
		}
		idx := bytes.IndexByte(p, '\n')
		if idx < 0 {
			n, err := iw.w.Write(p)
			return total + n, err
		}
		n, err := iw.w.Write(p[:idx+1])
		total += n
		if err != nil {
			return total, err
		}
		p = p[idx+1:]
		iw.atLineStart = true
	}
	return total, nil
}

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
	switch tt := t.(type) {
	case tokenize.FormativeWord:
		renderFormativeBlock(w, tt.Text, tt.Formative, glosser)
	case tokenize.ConcatenatedFormativeWord:
		renderConcatenated(w, tt, glosser)
	case tokenize.ModularWord:
		renderModular(w, tt)
	default:
		fmt.Fprintf(w, "%s  %s  %s\n", t.Surface(), inspect.Type(t), glosser.Token(t))
	}
}

// renderModular prints the phonetic + glossary tables for a modular
// adjunct. The surface word sits at column 0; the body is indented
// two spaces so consecutive word blocks are visually separated.
func renderModular(w io.Writer, mw tokenize.ModularWord) {
	segs := inspect.SegmentsModular(mw.Text, mw.Modular, mw.MarksMood)
	glossary := inspect.GlossaryModular(segs)

	fmt.Fprintln(w, strings.ToLower(mw.Text))
	iw := indented(w, "  ")
	fmt.Fprintln(iw, "(modular adjunct)")
	fmt.Fprintln(iw)
	renderPhoneticTable(iw, segs)
	if len(glossary) > 0 {
		fmt.Fprintln(iw)
		renderGlossaryTable(iw, glossary)
	}
}

// renderFormativeBlock prints the surface, headword, phonetic table,
// and glossary for one formative. The surface word sits at column 0
// and everything below is indented under it.
func renderFormativeBlock(w io.Writer, text string, f g.Formative, glosser gloss.Glosser) {
	head := inspect.Headword(f, glosser.Lex)
	segs := inspect.Segments(text, f, glosser.Lex)
	glossary := inspect.Glossary(text, f, segs, glosser.Lex)

	fmt.Fprintln(w, strings.ToLower(text))
	iw := indented(w, "  ")
	if head.Code != "" {
		if head.Meaning != "" {
			fmt.Fprintf(iw, "%s — %s\n", head.Code, head.Meaning)
		} else {
			fmt.Fprintf(iw, "%s\n", head.Code)
		}
	}
	fmt.Fprintln(iw)
	renderPhoneticTable(iw, segs)
	fmt.Fprintln(iw)
	renderGlossaryTable(iw, glossary)
}

// renderConcatenated walks every formative in a concatenation chain,
// rendering each as its own block with a section marker. The chain's
// surface is hyphen-joined; we split on "-" to recover each piece's
// individual surface for the phonetic table.
func renderConcatenated(w io.Writer, cw tokenize.ConcatenatedFormativeWord, glosser gloss.Glosser) {
	fmt.Fprintln(w, strings.ToLower(cw.Text))
	iw := indented(w, "  ")
	fmt.Fprintln(iw, "(concatenated chain)")
	parts := strings.Split(cw.Text, "-")
	for i, f := range cw.Chain.Formatives() {
		fmt.Fprintln(iw)
		label := "[head]"
		if i > 0 && f.Concat != nil {
			label = fmt.Sprintf("[%s dependent]", f.Concat.String())
		}
		fmt.Fprintln(iw, label)
		surface := ""
		if i < len(parts) {
			surface = parts[i]
		}
		renderFormativeBlock(iw, surface, f, glosser)
	}
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

