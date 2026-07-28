package main

import (
	"fmt"
	"io"
	"strings"

	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/tokenize"
	"github.com/christian-oudard/ithkuil/validation"
	"github.com/christian-oudard/ithkuil/view"
)

// diffMark sits in the left margin of every row whose two sides
// disagree. Rows that match carry two spaces instead.
const diffMark = "≠"

// indent lines up section labels with the tables, whose first column is
// the one-rune diff marker plus the two-space column separator.
const indent = "   "

// side is one word's parsed breakdown, in the same three pieces the
// detailed analyze view uses.
type side struct {
	word  string
	segs  []view.Segment
	gloss []view.GlossaryEntry
	head  view.RootHead
}

// cmdCompare parses two words and lays their slot breakdowns side by
// side, marking every slot where they diverge and following up with a
// glossary of just the codes that changed. Answers "what does this one
// letter do?" without making the reader diff two analyze dumps by eye.
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
	sides := make([]side, 2)
	for i, arg := range rest {
		s, ok := buildSide(normalizeASCII(arg), lex, stderr)
		if !ok {
			return 1
		}
		sides[i] = s
	}
	renderCompare(stdout, sides[0], sides[1])
	return 0
}

// buildSide validates, tokenizes, and analyzes one word. The word must
// be a single token with a slot breakdown: formatives and modular
// adjuncts qualify, referentials and the rest don't.
func buildSide(word string, lex *lexicon.Lexicon, stderr io.Writer) (side, bool) {
	if r := validation.ValidateWord(word); !r.Valid {
		renderValidationError(stderr, word, r)
		return side{}, false
	}
	tokens := tokenize.Tokenize(word)
	if len(tokens) != 1 {
		fmt.Fprintf(stderr, "%s: expected one word, got %d tokens\n", word, len(tokens))
		return side{}, false
	}
	switch t := tokens[0].(type) {
	case tokenize.FormativeWord:
		segs := view.Segments(t.Text, t.Formative, lex)
		return side{
			word:  strings.ToLower(t.Text),
			segs:  segs,
			gloss: view.Glossary(t.Text, t.Formative, segs, lex),
			head:  view.Headword(t.Formative, lex),
		}, true
	case tokenize.ModularWord:
		segs := view.SegmentsModular(t.Text, t.Modular, t.MarksMood)
		return side{
			word:  strings.ToLower(t.Text),
			segs:  segs,
			gloss: view.GlossaryModular(segs),
		}, true
	default:
		fmt.Fprintf(stderr, "%s: %s has no slot breakdown to compare\n", word, view.Type(tokens[0]))
		return side{}, false
	}
}

func renderCompare(w io.Writer, a, b side) {
	rows := alignByKey(segKeys(a.segs), segKeys(b.segs))
	changed := renderSlotTable(w, a, b, rows)

	// A root block only makes sense when both words have a headword;
	// one modular adjunct in the pair means there's nothing to line up.
	rootChanged := a.head.Code != "" && b.head.Code != "" && a.head.Code != b.head.Code
	if rootChanged {
		fmt.Fprintln(w)
		renderRootDiff(w, a, b)
	}

	diffs := glossDiffs(a, b)
	switch {
	case len(diffs) > 0:
		fmt.Fprintln(w)
		renderGlossaryDiff(w, a, b, diffs)
	case !changed && !rootChanged:
		fmt.Fprintln(w)
		fmt.Fprintln(w, indent+stylize(ansiDim, "identical"))
	}
}

// renderSlotTable prints the aligned slot breakdown. The two words head
// their own column groups; the phonetic columns share a width so the
// groups line up under them.
// Returns whether any row differed.
func renderSlotTable(w io.Writer, a, b side, rows [][2]int) bool {
	slotW, phW, encW := len("SLOT"), 0, 0
	for _, r := range rows {
		sa, sb := segAt(a.segs, r[0]), segAt(b.segs, r[1])
		slotW = max(slotW, runeWidth(sa.Slot), runeWidth(sb.Slot))
		phW = max(phW, runeWidth(sa.Chunk), runeWidth(sb.Chunk))
		encW = max(encW, runeWidth(encodes(sa)))
	}
	groupW := phW + 2 + encW

	fmt.Fprintln(w, line(
		cell{"", 1, ""},
		cell{"SLOT", slotW, ansiDim},
		cell{a.word, groupW, ansiBold},
		cell{b.word, 0, ansiBold}))

	changed := false
	for _, r := range rows {
		sa, sb := segAt(a.segs, r[0]), segAt(b.segs, r[1])
		differs := r[0] < 0 || r[1] < 0 || sa.Chunk != sb.Chunk || encodes(sa) != encodes(sb)
		slot := sa.Slot
		if slot == "" {
			slot = sb.Slot
		}
		mark, ph, enc := "", ansiDim, ansiDim
		if differs {
			mark, ph, enc = diffMark, ansiCyan, ansiMagenta
			changed = true
		}
		fmt.Fprintln(w, line(
			cell{mark, 1, ansiBold},
			cell{slot, slotW, ansiYellow},
			cell{sa.Chunk, phW, ph},
			cell{encodes(sa), encW, enc},
			cell{sb.Chunk, phW, ph},
			cell{encodes(sb), 0, enc}))
	}
	return changed
}

// renderRootDiff prints the two headwords when the words don't share a
// lexical identity — different root, stem, or specification.
func renderRootDiff(w io.Writer, a, b side) {
	fmt.Fprintln(w, indent+stylize(ansiDim, "ROOT"))
	wordW := max(runeWidth(a.word), runeWidth(b.word))
	for _, s := range []side{a, b} {
		fmt.Fprintf(w, "%s%s  %s", indent, stylize(ansiBold, padRunes(s.word, wordW)), styleHeadwordCode(s.head.Code))
		if s.head.Meaning != "" {
			fmt.Fprintf(w, " — %s", stylize(ansiDim, s.head.Meaning))
		}
		fmt.Fprintln(w)
	}
}

// diffRow is one glossary category whose code changed between the two
// words; a zero-valued side means the category is absent there.
type diffRow struct {
	category string
	a, b     view.GlossaryEntry
}

// glossDiffs aligns the two glossaries by category and keeps only the
// rows where the code changed.
func glossDiffs(a, b side) []diffRow {
	var diffs []diffRow
	for _, r := range alignByKey(glossKeys(a.gloss), glossKeys(b.gloss)) {
		ea, eb := glossAt(a.gloss, r[0]), glossAt(b.gloss, r[1])
		if ea.Code == eb.Code {
			continue
		}
		category := ea.Category
		if category == "" {
			category = eb.Category
		}
		diffs = append(diffs, diffRow{category, ea, eb})
	}
	return diffs
}

// renderGlossaryDiff prints one row per category whose code changed,
// with each side's code and name.
func renderGlossaryDiff(w io.Writer, a, b side, diffs []diffRow) {
	fmt.Fprintln(w, indent+stylize(ansiDim, "DIFFERENCES"))

	catW, codeW, nameW := len("CATEGORY"), 0, 0
	for _, d := range diffs {
		catW = max(catW, runeWidth(d.category))
		codeW = max(codeW, runeWidth(d.a.Code), runeWidth(d.b.Code))
		nameW = max(nameW, runeWidth(d.a.Name), runeWidth(d.b.Name))
	}
	groupW := codeW + 2 + nameW

	fmt.Fprintln(w, line(
		cell{"", 1, ""},
		cell{"CATEGORY", catW, ansiDim},
		cell{a.word, groupW, ansiBold},
		cell{b.word, 0, ansiBold}))
	for _, d := range diffs {
		fmt.Fprintln(w, line(
			cell{"", 1, ""},
			cell{d.category, catW, ansiGreen},
			cell{d.a.Code, codeW, ansiMagenta},
			cell{d.a.Name, nameW, ""},
			cell{d.b.Code, codeW, ansiMagenta},
			cell{d.b.Name, 0, ""}))
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

func encodes(s view.Segment) string { return strings.Join(s.Encodes, " / ") }

func segKeys(segs []view.Segment) []string {
	keys := make([]string, len(segs))
	for i, s := range segs {
		keys[i] = s.Slot
	}
	return keys
}

func glossKeys(entries []view.GlossaryEntry) []string {
	keys := make([]string, len(entries))
	for i, e := range entries {
		keys[i] = e.Category
	}
	return keys
}

// segAt returns the segment at i, or a blank one when i is a gap.
func segAt(segs []view.Segment, i int) view.Segment {
	if i < 0 {
		return view.Segment{}
	}
	return segs[i]
}

func glossAt(entries []view.GlossaryEntry, i int) view.GlossaryEntry {
	if i < 0 {
		return view.GlossaryEntry{}
	}
	return entries[i]
}

// alignByKey pairs up two sequences by their keys, preserving order and
// matching as many keys as possible (a longest-common-subsequence
// walk). The result holds index pairs; -1 marks a gap on that side, so
// a slot present in only one of the words still gets its own row.
func alignByKey(ka, kb []string) [][2]int {
	n, m := len(ka), len(kb)
	// lcs[i][j] is the longest common subsequence of ka[i:] and kb[j:].
	lcs := make([][]int, n+1)
	for i := range lcs {
		lcs[i] = make([]int, m+1)
	}
	for i := n - 1; i >= 0; i-- {
		for j := m - 1; j >= 0; j-- {
			switch {
			case ka[i] == kb[j]:
				lcs[i][j] = lcs[i+1][j+1] + 1
			case lcs[i+1][j] >= lcs[i][j+1]:
				lcs[i][j] = lcs[i+1][j]
			default:
				lcs[i][j] = lcs[i][j+1]
			}
		}
	}

	var out [][2]int
	i, j := 0, 0
	for i < n && j < m {
		switch {
		case ka[i] == kb[j]:
			out = append(out, [2]int{i, j})
			i++
			j++
		case lcs[i+1][j] >= lcs[i][j+1]:
			out = append(out, [2]int{i, -1})
			i++
		default:
			out = append(out, [2]int{-1, j})
			j++
		}
	}
	for ; i < n; i++ {
		out = append(out, [2]int{i, -1})
	}
	for ; j < m; j++ {
		out = append(out, [2]int{-1, j})
	}
	return out
}
