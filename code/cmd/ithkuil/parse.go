package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"strings"

	"github.com/christian-oudard/ithkuil/fault"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/roman"
	"github.com/christian-oudard/ithkuil/slots"
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
// single romanization/type/gloss line. Phonotactics are checked first, so
// parsing a word is also how you validate it.
func cmdParse(args []string, stdin io.Reader, stdout, stderr io.Writer, dataFile string) int {
	fs := newFlagSet("parse", stderr)
	fs.describe("Tokenize, parse, and gloss each word (detailed by default).", "TEXT...")
	short := fs.Bool("short", "s", false, "one-line romanization · type · gloss view")
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
	// Accept the ASCII digraph notation everywhere a word is taken.
	text = phonology.FromASCII(text)

	// What the user typed, keyed by the romanization a token will carry, so
	// an error can name their input rather than a form only we ever
	// saw: "aaaa" normalizes to "ää", and reporting the rule against
	// "ää" describes a word they never wrote. FromASCII never adds or
	// drops whitespace, so the two agree word for word.
	asTyped := map[string]string{}
	if o, n := strings.Fields(typed), strings.Fields(text); len(o) == len(n) {
		for i := range n {
			asTyped[strings.ToLower(n[i])] = o[i]
		}
	}

	results := roman.Tokenize(text)
	if len(results) == 0 {
		return 0
	}
	lex := loadLex(dataFile, stderr)
	span := roman.Words(results)
	// The gloss this command prints is the canonical one, the same
	// string compose reads back. There is a second, prettier rendering
	// with Unicode subscripts and spelled-out English, but it is not
	// the gloss syntax and nothing can parse it, so showing it here
	// would be showing the user a form they cannot use.
	canonical := gloss.Glosser{Lex: lex, Canonical: true}

	// Phonotactics are checked per token rather than through a map
	// built from strings.Fields. The map was keyed by the raw word but
	// read back with t.Romanization(), which is lower-cased, so every
	// capitalized word missed the lookup and skipped validation in
	// silence — "cskava" was rejected while "Cskava" sailed through,
	// and sentences start with a capital.
	exit := 0
	if *short {
		for i, r := range results {
			var ill fault.Faults
			if errors.As(phonology.CheckText(r.Romanization), &ill) {
				renderValidationError(stderr, r.Romanization, asTyped[r.Romanization], ill)
				exit = 1
				continue
			}
			// One gloss per line and nothing else. The romanization is
			// what the user just typed, and the word class is legible
			// from the gloss itself now that each punctuation mark has
			// one job: "[CAR]" is a carrier, "1m-THM" a referential.
			// A column repeating either is noise in output whose point
			// is to be pasted back into compose.
			//
			// A word that could not be read is the exception: it has
			// no gloss, so give the reason instead. Named with a colon
			// rather than dressed up with a "?" — a line here is a
			// gloss you can paste back, and this one is not.
			if r.Err != nil {
				var fs fault.Faults
				errors.As(r.Err, &fs)
				fmt.Fprintf(stdout, "%s: %s\n", r.Romanization,
					strings.Join(faultLines(fs, r.Err), "; "))
				continue
			}
			fmt.Fprintln(stdout, canonical.Word(r.Word, span, i))
		}
		return exit
	}

	// Detailed view.
	for i, r := range results {
		if i > 0 {
			fmt.Fprintln(stdout)
		}
		var ill fault.Faults
		if errors.As(phonology.CheckText(r.Romanization), &ill) {
			renderValidationError(stderr, r.Romanization, asTyped[r.Romanization], ill)
			exit = 1
			continue
		}
		if r.Err != nil {
			renderUnknown(stdout, r.Romanization, r.Err)
			continue
		}
		renderDetailed(stdout, r, span, i, canonical)
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
func renderValidationError(w io.Writer, word, typed string, ill fault.Faults) {
	subject := word
	if typed != "" && typed != word {
		subject = fmt.Sprintf("%s → %s", typed, word)
	}
	for _, v := range ill.List {
		fmt.Fprintf(w, "%s  %s: %s", subject, v.Code, v.Fix)
		if v.Found != "" && v.Found != word {
			fmt.Fprintf(w, " (in %s)", v.Found)
		}
		fmt.Fprintln(w)
	}
}

// renderDetailed prints one word: its romanization, the canonical
// gloss, and then the working underneath. Leading with the gloss makes
// the detailed view the short view plus evidence, rather than a
// separate answer in a notation the short view never shows.
func renderDetailed(w io.Writer, r roman.Result, span g.Text, i int,
	canonical gloss.Glosser,
) {
	gl := canonical.Word(r.Word, span, i)
	switch tt := r.Word.(type) {
	case g.Formative:
		renderFormativeBlock(w, r.Romanization, tt, canonical, gl)
	case *g.Chain:
		renderConcatenated(w, r.Romanization, tt, canonical, gl)
	case g.ModularAdjunct:
		var marksMood *bool
		if verbal, found := roman.ModularIsVerbal(span, i); found {
			marksMood = &verbal
		}
		renderModular(w, r.Romanization, tt, marksMood, gl)
	default:
		wordHeader(w, r.Romanization, gl)
		fmt.Fprintln(indented(w, "  "), view.Type(r.Word))
	}
}

// wordHeader prints the romanization and, under it, the canonical
// gloss — the same string --short prints and compose reads back.
func wordHeader(w io.Writer, romanization, gl string) {
	fmt.Fprintln(w, stylize(ansiBold, strings.ToLower(romanization)))
	if gl != "" {
		fmt.Fprintln(indented(w, "  "), stylize(ansiMagenta, gl))
		fmt.Fprintln(w)
	}
}

// renderUnknown reports why no word class claimed a word.
//
// A value-stage failure is shown against the split rather than beside
// it: the word divided into slots cleanly and the reader needs to see
// which of them is the problem, which is a property of a row and not
// of the word. Marking the row puts the correct slots and the wrong
// one in the same picture, so "everything but the Ca is fine" is
// legible without reading a sentence and counting conjuncts.
//
// A shape failure has no split to mark, since the cut is what failed,
// so it prints alone.
func renderUnknown(w io.Writer, word string, err error) {
	fmt.Fprintln(w, stylize(ansiBold, strings.ToLower(word)))
	iw := indented(w, "  ")
	fmt.Fprintln(iw, stylize(ansiDim, "(unclassified)"))
	fmt.Fprintln(iw)

	var fs fault.Faults
	errors.As(err, &fs)

	layout, perr := slots.Parse(word)
	if perr != nil {
		for _, f := range faultLines(fs, err) {
			fmt.Fprintln(iw, f)
		}
		return
	}
	byslot := map[string]fault.Fault{}
	for _, f := range fs.List {
		byslot[f.Code] = f
	}
	segs := view.LayoutSegments(layout)
	renderFaultedTable(iw, segs, byslot)
	// A fault that names no slot in the split — a whole-word one, or a
	// slot this view does not draw — still has to be said, or it is
	// lost entirely.
	drawn := drawnSlots(segs)
	for _, f := range fs.List {
		if !drawn[f.Code] {
			fmt.Fprintf(iw, "\n%s\n", f.Fix)
		}
	}
}

func drawnSlots(segs []view.Segment) map[string]bool {
	out := map[string]bool{}
	for _, s := range segs {
		out[s.Slot] = true
	}
	return out
}

// faultLines renders faults as plain sentences, for the cases with no
// table to hang them on.
func faultLines(fs fault.Faults, err error) []string {
	if len(fs.List) == 0 {
		return []string{err.Error()}
	}
	out := make([]string, len(fs.List))
	for i, f := range fs.List {
		out[i] = f.Fix
	}
	return out
}

// renderModular prints the phonetic + glossary tables for a modular
// adjunct. The romanization sits at column 0; the body is indented
// two spaces so consecutive word blocks are visually separated.
func renderModular(w io.Writer, rom string, m g.ModularAdjunct, marksMood *bool, gl string) {
	segs := view.SegmentsModular(rom, m, marksMood)
	glossary := view.GlossaryModular(segs)

	wordHeader(w, rom, gl)
	iw := indented(w, "  ")
	renderPhoneticTable(iw, segs)
	if len(glossary) > 0 {
		renderGlossaryTable(iw, glossary)
	}
}

// renderFormativeBlock prints the romanization, headword, phonetic table,
// and glossary for one formative. The romanization sits at column 0
// and everything below is indented under it.
func renderFormativeBlock(w io.Writer, text string, f g.Formative, canonical gloss.Glosser, gl string) {
	head := view.Headword(f, canonical.Lex)
	segs := view.Segments(text, f, canonical.Lex)
	glossary := view.Glossary(text, f, segs, canonical.Lex)

	wordHeader(w, text, gl)
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
// romanization is hyphen-joined; we split on "-" to recover each piece's
// individual romanization for the phonetic table.
func renderConcatenated(w io.Writer, rom string, cw *g.Chain, canonical gloss.Glosser, gl string) {
	wordHeader(w, rom, gl)
	iw := indented(w, "  ")
	fmt.Fprintln(iw, stylize(ansiDim, "(concatenated chain)"))
	for _, f := range cw.Formatives() {
		fmt.Fprintln(iw)
		// Dependents lead and the parent comes last (§3.1.7), so the
		// Cc marker alone tells them apart; position does not.
		label := "[head]"
		if f.Concat != g.ConcatNone {
			label = fmt.Sprintf("[%s dependent]", f.Concat.String())
		}
		fmt.Fprintln(iw, label)
		renderFormativeBlock(iw, roman.Formative(f), f, canonical, canonical.Formative(f))
	}
}

// renderFaultedTable draws the slot split with a marker against each
// row whose slot did not decode, and that slot's fix beside it. The
// unmarked rows are the reading that succeeded, and showing them is
// half the diagnosis: a reader has to know how much of the word was
// understood before "the Ca is wrong" means anything.
func renderFaultedTable(w io.Writer, segs []view.Segment, byslot map[string]fault.Fault) {
	phW, slW := len("PHONETIC"), len("SLOT")
	for _, s := range segs {
		if n := runeWidth(s.Chunk); n > phW {
			phW = n
		}
		if n := runeWidth(s.Slot); n > slW {
			slW = n
		}
	}
	fmt.Fprintf(w, "   %s  %s  %s\n",
		stylize(ansiDim, padRunes("PHONETIC", phW)),
		stylize(ansiDim, padRunes("SLOT", slW)),
		stylize(ansiDim, "READS AS"))
	for _, s := range segs {
		f, bad := byslot[s.Slot]
		mark := "   "
		reads := stylize(ansiDim, "ok")
		if bad {
			mark = stylize(ansiRed, "✗  ")
			reads = f.Fix
		}
		fmt.Fprintf(w, "%s%s  %s  %s\n",
			mark,
			stylize(ansiCyan, padRunes(s.Chunk, phW)),
			stylize(ansiYellow, padRunes(s.Slot, slW)),
			reads)
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
