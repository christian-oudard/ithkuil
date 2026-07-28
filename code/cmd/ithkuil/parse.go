package main

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/slots"
	"github.com/christian-oudard/ithkuil/tokenize"
	"github.com/christian-oudard/ithkuil/validation"
	"github.com/christian-oudard/ithkuil/view"
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

// cmdParse tokenizes the input and renders a learner-oriented
// breakdown of each formative: phonetic segmentation paired with a
// glossary that expands every code. --short collapses each word to a
// single surface/type/gloss line. Phonotactics are checked first, so
// parsing a word is also how you validate it.
func cmdParse(args []string, stdin io.Reader, stdout, stderr io.Writer, dataFile string) int {
	fs := newFlagSet("parse", stderr)
	fs.describe("Tokenize, parse, and gloss each word (detailed by default).", "TEXT...")
	short := fs.Bool("short", "s", false, "one-line surface · type · gloss view")
	color := fs.String("color", "", "auto", "MODE", "when to use ANSI color: auto|always|never")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	setColorMode(stdout, *color)
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
		fmt.Fprintln(stderr, "usage: ithkuil parse TEXT... (or pipe via stdin)")
		return 2
	}

	typed := text
	text = normalizeASCII(text)

	// What the user typed, keyed by the surface a token will carry, so
	// an error can name their input rather than a form only we ever
	// saw: "aaaa" normalizes to "ää", and reporting the rule against
	// "ää" describes a word they never wrote. normalizeASCII works one
	// whitespace-separated word at a time, so the two agree in length.
	asTyped := map[string]string{}
	if o, n := strings.Fields(typed), strings.Fields(text); len(o) == len(n) {
		for i := range n {
			asTyped[strings.ToLower(n[i])] = o[i]
		}
	}

	tokens := tokenize.Tokenize(text)
	if len(tokens) == 0 {
		return 0
	}
	lex := loadLex(dataFile, stderr)
	glosser := gloss.Glosser{Lex: lex}

	// Phonotactics are checked per token rather than through a map
	// built from strings.Fields. The map was keyed by the raw word but
	// read back with t.Surface(), which is lower-cased, so every
	// capitalized word missed the lookup and skipped validation in
	// silence — "cskava" was rejected while "Cskava" sailed through,
	// and sentences start with a capital.
	exit := 0
	if *short {
		for _, t := range tokens {
			if res := validation.ValidateWord(t.Surface()); !res.Valid {
				renderValidationError(stderr, t.Surface(), asTyped[t.Surface()], res)
				exit = 1
				continue
			}
			fmt.Fprintf(stdout, "%s  %s  %s\n", t.Surface(), view.Type(t), glosser.Token(t))
		}
		return exit
	}

	// Detailed view.
	for i, t := range tokens {
		if i > 0 {
			fmt.Fprintln(stdout)
		}
		if res := validation.ValidateWord(t.Surface()); !res.Valid {
			renderValidationError(stderr, t.Surface(), asTyped[t.Surface()], res)
			exit = 1
			continue
		}
		renderDetailed(stdout, t, lex, glosser)
	}
	return exit
}

// renderValidationError reports a phonotactically invalid word: the
// rule it breaks and the cluster that breaks it, in place of a slot
// breakdown that would be nonsense.
//
// typed is what the user actually wrote. When the ASCII input method
// rewrote it — "aaaa" into "ää" — both are shown, so the message names
// their input and shows what we read it as instead of silently
// substituting a word they never typed.
func renderValidationError(w io.Writer, word, typed string, res validation.Result) {
	subject := word
	if typed != "" && typed != word {
		subject = fmt.Sprintf("%s → %s", typed, word)
	}
	for _, e := range res.Errors {
		fmt.Fprintf(w, "%s  %s: %s", subject, e.Rule, e.Reason)
		if e.Cluster != "" {
			fmt.Fprintf(w, " (cluster %s)", e.Cluster)
		}
		fmt.Fprintln(w)
	}
}

func renderDetailed(w io.Writer, t tokenize.WordToken, lex interface{}, glosser gloss.Glosser) {
	switch tt := t.(type) {
	case tokenize.FormativeWord:
		renderFormativeBlock(w, tt.Text, tt.Formative, glosser)
	case tokenize.ConcatenatedFormativeWord:
		renderConcatenated(w, tt, glosser)
	case tokenize.ModularWord:
		renderModular(w, tt)
	case tokenize.UnknownWord:
		renderUnknown(w, tt.Text)
	default:
		fmt.Fprintf(w, "%s  %s  %s\n", t.Surface(), view.Type(t), glosser.Token(t))
	}
}

// renderUnknown reports why no classifier claimed a word. The
// formative decoder gets furthest into a word of any of them, so its
// complaint is the most specific description of the shape available;
// it is a diagnostic, not a claim that the word was meant to be a
// formative.
func renderUnknown(w io.Writer, word string) {
	fmt.Fprintln(w, stylize(ansiBold, strings.ToLower(word)))
	iw := indented(w, "  ")
	fmt.Fprintln(iw, stylize(ansiDim, "(unclassified)"))
	fmt.Fprintln(iw)

	layout, err := slots.Parse(word)
	if err != nil {
		// slots.Parse errors already name their own package; a second
		// label here just reads as "shape: slots: ...".
		fmt.Fprintf(iw, "%v\n", err)
		return
	}
	if _, err := slots.ToGrammar(layout); err != nil {
		fmt.Fprintf(iw, "as a formative: %v\n", err)
	}
	fmt.Fprintln(iw)
	fmt.Fprintln(iw, stylize(ansiDim, "slot shape, for the word as a whole:"))
	renderPhoneticTable(iw, view.LayoutSegments(layout))
}

// renderModular prints the phonetic + glossary tables for a modular
// adjunct. The surface word sits at column 0; the body is indented
// two spaces so consecutive word blocks are visually separated.
func renderModular(w io.Writer, mw tokenize.ModularWord) {
	segs := view.SegmentsModular(mw.Text, mw.Modular, mw.MarksMood)
	glossary := view.GlossaryModular(segs)

	fmt.Fprintln(w, stylize(ansiBold, strings.ToLower(mw.Text)))
	iw := indented(w, "  ")
	renderPhoneticTable(iw, segs)
	if len(glossary) > 0 {
		renderGlossaryTable(iw, glossary)
	}
}

// renderFormativeBlock prints the surface, headword, phonetic table,
// and glossary for one formative. The surface word sits at column 0
// and everything below is indented under it.
func renderFormativeBlock(w io.Writer, text string, f g.Formative, glosser gloss.Glosser) {
	head := view.Headword(f, glosser.Lex)
	segs := view.Segments(text, f, glosser.Lex)
	glossary := view.Glossary(text, f, segs, glosser.Lex)

	fmt.Fprintln(w, stylize(ansiBold, strings.ToLower(text)))
	iw := indented(w, "  ")
	renderPhoneticTable(iw, segs)
	if head.Code != "" {
		fmt.Fprintln(iw, stylize(ansiDim, "ROOT"))
		styledCode := styleHeadwordCode(head.Code)
		if head.Meaning != "" {
			fmt.Fprintf(iw, "%s — %s\n", styledCode, stylize(ansiDim, head.Meaning))
		} else {
			fmt.Fprintf(iw, "%s\n", styledCode)
		}
	}
	renderGlossaryTable(iw, glossary)
}

// renderConcatenated walks every formative in a concatenation chain,
// rendering each as its own block with a section marker. The chain's
// surface is hyphen-joined; we split on "-" to recover each piece's
// individual surface for the phonetic table.
func renderConcatenated(w io.Writer, cw tokenize.ConcatenatedFormativeWord, glosser gloss.Glosser) {
	fmt.Fprintln(w, stylize(ansiBold, strings.ToLower(cw.Text)))
	iw := indented(w, "  ")
	fmt.Fprintln(iw, stylize(ansiDim, "(concatenated chain)"))
	parts := strings.Split(cw.Text, "-")
	for i, f := range cw.Chain.Formatives() {
		fmt.Fprintln(iw)
		// Dependents lead and the parent comes last (§3.1.7), so the
		// Cc marker alone tells them apart; position does not.
		label := "[head]"
		if f.Concat != g.ConcatNone {
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

func renderPhoneticTable(w io.Writer, segs []view.Segment) {
	phW, slW := len("PHONETIC"), len("SLOT")
	for _, s := range segs {
		if n := runeWidth(s.Chunk); n > phW {
			phW = n
		}
		if n := runeWidth(s.Slot); n > slW {
			slW = n
		}
	}
	// A Layout carries no Encodes, so drop the column entirely rather
	// than printing a header over blanks and trailing whitespace.
	encoded := false
	for _, s := range segs {
		if len(s.Encodes) > 0 {
			encoded = true
			break
		}
	}
	if !encoded {
		fmt.Fprintf(w, "%s  %s\n",
			stylize(ansiDim, padRunes("PHONETIC", phW)),
			stylize(ansiDim, "SLOT"))
		for _, s := range segs {
			fmt.Fprintf(w, "%s  %s\n",
				stylize(ansiCyan, padRunes(s.Chunk, phW)),
				stylize(ansiYellow, s.Slot))
		}
		return
	}
	fmt.Fprintf(w, "%s  %s  %s\n",
		stylize(ansiDim, padRunes("PHONETIC", phW)),
		stylize(ansiDim, padRunes("SLOT", slW)),
		stylize(ansiDim, "ENCODES"))
	for _, s := range segs {
		encodes := strings.Join(s.Encodes, " / ")
		fmt.Fprintf(w, "%s  %s  %s\n",
			stylize(ansiCyan, padRunes(s.Chunk, phW)),
			stylize(ansiYellow, padRunes(s.Slot, slW)),
			stylize(ansiMagenta, encodes))
	}
}

func renderGlossaryTable(w io.Writer, entries []view.GlossaryEntry) {
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
	fmt.Fprintf(w, "%s  %s  %s  %s\n",
		stylize(ansiDim, padRunes("CATEGORY", catW)),
		stylize(ansiDim, padRunes("CODE", codeW)),
		stylize(ansiDim, padRunes("NAME", nameW)),
		stylize(ansiDim, "MEANING"))
	for _, e := range entries {
		fmt.Fprintf(w, "%s  %s  %s  %s\n",
			stylize(ansiGreen, padRunes(e.Category, catW)),
			stylize(ansiMagenta, padRunes(e.Code, codeW)),
			padRunes(e.Name, nameW),
			stylize(ansiDim, e.Meaning))
	}
}

// runeWidth returns the visible width (rune count) of s, treating each
// rune as one column. Good enough for ASCII + common Latin diacritics
// + subscripts.
func runeWidth(s string) int {
	return len([]rune(s))
}

// styleHeadwordCode colors the parts of a headword like
// `"ḑx" / S2 / BSC` distinctly: the leading root identifier stays
// cyan (it's a phonetic form), the grammatical codes after each
// " / " separator switch to magenta to match every other code
// rendering. The separators themselves go dim.
func styleHeadwordCode(code string) string {
	parts := strings.Split(code, " / ")
	if len(parts) == 0 {
		return code
	}
	out := stylize(ansiCyan, parts[0])
	for _, p := range parts[1:] {
		out += stylize(ansiDim, " / ") + stylize(ansiMagenta, p)
	}
	return out
}

// padRunes right-pads s with spaces to width w, measured in runes.
func padRunes(s string, w int) string {
	r := runeWidth(s)
	if r >= w {
		return s
	}
	return s + strings.Repeat(" ", w-r)
}
