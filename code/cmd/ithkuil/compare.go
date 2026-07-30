package main

import (
	"errors"
	"fmt"
	"io"
	"strings"

	"github.com/christian-oudard/ithkuil/fault"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/view"
)

// diffMark sits in the left margin of every row whose two sides
// disagree. Rows that match carry two spaces instead.
const diffMark = "≠"

// indent lines up section labels with the tables, whose first column is
// the one-rune diff marker plus the two-space column separator.
const indent = "   "

// cmdCompare parses two words and lays their slot breakdowns side by
// side, marking every slot where they diverge and following up with a
// glossary of just the codes that changed. Answers "what does this one
// letter do?" without making the reader diff two parse dumps by eye.
func cmdCompare(args []string, stdout, stderr io.Writer, dataFile string) int {
	fs := newFlagSet("compare", stderr)
	fs.describe("Lay two words' slot breakdowns side by side and mark what differs.", "WORD WORD")
	color := fs.String("color", "", "auto", "MODE", "when to use ANSI color: auto|always|never")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	setColorMode(stdout, *color)
	rest := fs.Args()
	if len(rest) != 2 {
		fmt.Fprintln(stderr, "usage: ithkuil compare WORD WORD")
		return 2
	}

	lex := loadLex(dataFile, stderr)
	sides := make([]view.Side, 2)
	for i, arg := range rest {
		s, ok := buildSide(phonology.FromASCII(arg), arg, lex, stderr)
		if !ok {
			return 1
		}
		sides[i] = s
	}
	renderCompare(stdout, sides[0], sides[1])
	return 0
}

// buildSide validates a word and analyzes it into comparable blocks,
// reporting either failure on stderr. typed is the argument as the
// user wrote it, before the ASCII input method rewrote it, so a rule
// broken by "aaaa" names "aaaa".
func buildSide(word, typed string, lex *lexicon.Lexicon, stderr io.Writer) (view.Side, bool) {
	var ill fault.Faults
	if err := phonology.CheckText(word); errors.As(err, &ill) {
		renderValidationError(stderr, word, typed, ill)
		return view.Side{}, false
	}
	s, err := view.BuildSide(word, lex)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return view.Side{}, false
	}
	return s, true
}

// renderCompare lays out each pair of blocks in turn, then whatever a
// longer chain left unpaired.
func renderCompare(w io.Writer, a, b view.Side) {
	pairs, extra := view.PairSides(a, b)
	if len(a.Blocks) > 1 || len(b.Blocks) > 1 {
		fmt.Fprintln(w, indent+stylize(ansiBold, a.Word)+stylize(ansiDim, " vs ")+stylize(ansiBold, b.Word))
		fmt.Fprintln(w)
	}
	for i, p := range pairs {
		if i > 0 {
			fmt.Fprintln(w)
		}
		renderPair(w, p.A, p.B)
	}
	if len(extra) > 0 {
		fmt.Fprintln(w)
		renderUnpaired(w, extra)
	}
}

func renderUnpaired(w io.Writer, extra []view.Unpaired) {
	fmt.Fprintln(w, indent+stylize(ansiDim, "UNPAIRED"))
	wordW := 0
	for _, e := range extra {
		wordW = max(wordW, runeWidth(e.Block.Word))
	}
	for _, e := range extra {
		fmt.Fprintln(w, line(
			cell{"", 1, ""},
			cell{e.Block.Word, wordW, ansiBold},
			cell{fmt.Sprintf("%s of %s", e.Block.Role, e.Owner), 0, ansiDim}))
	}
}

func renderPair(w io.Writer, a, b view.Block) {
	rows := view.SlotDiff(a, b)
	changed := renderSlotTable(w, a, b, rows)

	rootChanged := view.RootDiffers(a, b)
	if rootChanged {
		fmt.Fprintln(w)
		renderRootDiff(w, a, b)
	}

	if notes := notedBlocks(a, b); len(notes) > 0 {
		fmt.Fprintln(w)
		renderNotes(w, notes)
	}

	diffs := view.GlossDiff(a, b)
	switch {
	case len(diffs) > 0:
		fmt.Fprintln(w)
		renderGlossaryDiff(w, a, b, diffs)
	case a.Decoded && b.Decoded && !changed && !rootChanged:
		fmt.Fprintln(w)
		fmt.Fprintln(w, indent+stylize(ansiDim, "identical"))
	}
}

// notedBlocks returns the blocks that carry a decoder complaint.
func notedBlocks(blocks ...view.Block) []view.Block {
	var out []view.Block
	for _, s := range blocks {
		if s.Note != "" {
			out = append(out, s)
		}
	}
	return out
}

// renderNotes prints why a word wouldn't decode, under the shape table
// that is all we could show for it.
func renderNotes(w io.Writer, notes []view.Block) {
	fmt.Fprintln(w, indent+stylize(ansiDim, "UNCLASSIFIED"))
	wordW := 0
	for _, s := range notes {
		wordW = max(wordW, runeWidth(s.Header()))
	}
	for _, s := range notes {
		fmt.Fprintln(w, line(
			cell{"", 1, ""},
			cell{s.Header(), wordW, ansiBold},
			cell{s.Note, 0, ansiDim}))
	}
}

// renderSlotTable prints the aligned slot breakdown. The two words head
// their own column groups; the phonetic columns share a width so the
// groups line up under them.
// Returns whether any row differed.
func renderSlotTable(w io.Writer, a, b view.Block, rows []view.SlotRow) bool {
	slotW, phW, encW := len("SLOT"), 0, 0
	for _, r := range rows {
		slotW = max(slotW, runeWidth(r.Slot))
		phW = max(phW, runeWidth(r.A.Chunk), runeWidth(r.B.Chunk))
		encW = max(encW, runeWidth(view.Encodes(r.A)))
	}
	groupW := phW + 2 + encW

	fmt.Fprintln(w, line(
		cell{"", 1, ""},
		cell{"SLOT", slotW, ansiDim},
		cell{a.Header(), groupW, ansiBold},
		cell{b.Header(), 0, ansiBold}))

	changed := false
	for _, r := range rows {
		mark, ph, enc := "", ansiDim, ansiDim
		if r.Differs {
			mark, ph, enc = diffMark, ansiCyan, ansiMagenta
			changed = true
		}
		fmt.Fprintln(w, line(
			cell{mark, 1, ansiBold},
			cell{r.Slot, slotW, ansiYellow},
			cell{r.A.Chunk, phW, ph},
			cell{view.Encodes(r.A), encW, enc},
			cell{r.B.Chunk, phW, ph},
			cell{view.Encodes(r.B), 0, enc}))
	}
	return changed
}

// renderRootDiff prints the two headwords when the words don't share a
// lexical identity: different root, stem, or specification.
func renderRootDiff(w io.Writer, a, b view.Block) {
	fmt.Fprintln(w, indent+stylize(ansiDim, "ROOT"))
	wordW := max(runeWidth(a.Header()), runeWidth(b.Header()))
	for _, s := range []view.Block{a, b} {
		fmt.Fprintf(w, "%s%s  %s", indent, stylize(ansiBold, padRunes(s.Header(), wordW)), styleHeadwordCode(s.Head.Code))
		if s.Head.Meaning != "" {
			fmt.Fprintf(w, " — %s", stylize(ansiDim, s.Head.Meaning))
		}
		fmt.Fprintln(w)
	}
}

// renderGlossaryDiff prints one row per category whose code changed,
// with each side's code and name.
func renderGlossaryDiff(w io.Writer, a, b view.Block, diffs []view.GlossDiffRow) {
	fmt.Fprintln(w, indent+stylize(ansiDim, "DIFFERENCES"))

	catW, codeW, nameW := len("CATEGORY"), 0, 0
	for _, d := range diffs {
		catW = max(catW, runeWidth(d.Category))
		codeW = max(codeW, runeWidth(d.A.Code), runeWidth(d.B.Code))
		nameW = max(nameW, runeWidth(d.A.Name), runeWidth(d.B.Name))
	}
	groupW := codeW + 2 + nameW

	fmt.Fprintln(w, line(
		cell{"", 1, ""},
		cell{"CATEGORY", catW, ansiDim},
		cell{a.Header(), groupW, ansiBold},
		cell{b.Header(), 0, ansiBold}))
	for _, d := range diffs {
		fmt.Fprintln(w, line(
			cell{"", 1, ""},
			cell{d.Category, catW, ansiGreen},
			cell{d.A.Code, codeW, ansiMagenta},
			cell{d.A.Name, nameW, ""},
			cell{d.B.Code, codeW, ansiMagenta},
			cell{d.B.Name, 0, ""}))
	}
}

// cell is one column of an output row: the text, the width it pads to,
// and the ANSI style it wears.
type cell struct {
	text  string
	width int
	style string
}

// line renders cells separated by two spaces. Padding after the last
// non-empty cell is dropped, so a row whose right-hand word has no
// counterpart for this slot carries no trailing whitespace.
func line(cells ...cell) string {
	last := -1
	for i, c := range cells {
		if c.text != "" {
			last = i
		}
	}
	var b strings.Builder
	for i, c := range cells[:last+1] {
		if i > 0 {
			b.WriteString("  ")
		}
		text := c.text
		if i < last {
			text = padRunes(text, c.width)
		}
		b.WriteString(stylize(c.style, text))
	}
	return b.String()
}
