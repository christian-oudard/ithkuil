package main

import (
	"fmt"
	"io"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/slots"
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

// block is one formative's parsed breakdown, in the same three pieces
// the detailed analyze view uses. An unclassified word fills in segs
// from the shape split alone: decoded is false, gloss and head are
// empty, and note carries the decoder's complaint. role names a chain
// member's part ("head", "Type1 dependent") and is empty otherwise.
type block struct {
	word    string
	role    string
	segs    []view.Segment
	gloss   []view.GlossaryEntry
	head    view.RootHead
	decoded bool
	note    string
}

// header is how a block titles its column: the surface, plus the part
// it plays when it is one member of a chain.
func (b block) header() string {
	if b.role == "" {
		return b.word
	}
	return b.word + " [" + b.role + "]"
}

// side is one command-line argument: a single formative or adjunct is
// one block, a concatenation chain is one block per member, in surface
// order (dependents first, parent last, §3.1.7).
type side struct {
	word   string
	blocks []block
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

// buildSide validates, tokenizes, and analyzes one word. Formatives
// and modular adjuncts give a full breakdown; a chain gives one per
// member; an unclassified word gives the shape split, which is what
// makes a good word comparable to a bad one. Referentials and the rest
// have no slot structure at all.
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
	surface := strings.ToLower(tokens[0].Surface())
	switch t := tokens[0].(type) {
	case tokenize.FormativeWord:
		return side{surface, []block{formativeBlock(t.Text, t.Formative, "", lex)}}, true
	case tokenize.ModularWord:
		segs := view.SegmentsModular(t.Text, t.Modular, t.MarksMood)
		return side{surface, []block{{
			word:    strings.ToLower(t.Text),
			segs:    segs,
			gloss:   view.GlossaryModular(segs),
			decoded: true,
		}}}, true
	case tokenize.ConcatenatedFormativeWord:
		return side{surface, chainBlocks(t, lex)}, true
	case tokenize.UnknownWord:
		bl, ok := unknownBlock(t.Text, stderr)
		return side{surface, []block{bl}}, ok
	default:
		fmt.Fprintf(stderr, "%s: %s has no slot breakdown to compare\n", word, view.Type(tokens[0]))
		return side{}, false
	}
}

func formativeBlock(text string, f g.Formative, role string, lex *lexicon.Lexicon) block {
	segs := view.Segments(text, f, lex)
	return block{
		word:    strings.ToLower(text),
		role:    role,
		segs:    segs,
		gloss:   view.Glossary(text, f, segs, lex),
		head:    view.Headword(f, lex),
		decoded: true,
	}
}

// chainBlocks splits a concatenation chain into one block per member.
// The chain's surface is hyphen-joined, so splitting on "-" recovers
// each member's own surface. Dependents lead and the parent comes last
// (§3.1.7), but the Cc marker is what tells them apart, not position.
func chainBlocks(cw tokenize.ConcatenatedFormativeWord, lex *lexicon.Lexicon) []block {
	parts := strings.Split(cw.Text, "-")
	formatives := cw.Chain.Formatives()
	blocks := make([]block, 0, len(formatives))
	for i, f := range formatives {
		role := "head"
		if f.Concat != g.ConcatNone {
			role = f.Concat.String() + " dependent"
		}
		surface := ""
		if i < len(parts) {
			surface = parts[i]
		}
		blocks = append(blocks, formativeBlock(surface, f, role, lex))
	}
	return blocks
}

// unknownBlock builds a block from the shape split of a word no
// classifier claimed. slots.Parse assigns conjuncts to slots by shape
// alone, so it still succeeds where ToGrammar rejects a value, and the
// formative decoder's complaint says which value that was. Only a word
// whose shape won't split at all is beyond comparing.
func unknownBlock(word string, stderr io.Writer) (block, bool) {
	layout, err := slots.Parse(word)
	if err != nil {
		fmt.Fprintf(stderr, "%s: %v\n", word, err)
		return block{}, false
	}
	note := ""
	if _, err := slots.ToGrammar(layout); err != nil {
		note = fmt.Sprintf("as a formative: %v", err)
	}
	return block{
		word: strings.ToLower(word),
		segs: view.LayoutSegments(layout),
		note: note,
	}, true
}

// renderCompare pairs the two sides' formatives off from the parent
// end and lays each pair out in turn. A chain's parent is its last
// member (§3.1.7), so that is what a standalone word is the
// counterpart of; a longer chain's leading dependents go unpaired.
func renderCompare(w io.Writer, a, b side) {
	n := min(len(a.blocks), len(b.blocks))
	if len(a.blocks) > 1 || len(b.blocks) > 1 {
		fmt.Fprintln(w, indent+stylize(ansiBold, a.word)+stylize(ansiDim, " vs ")+stylize(ansiBold, b.word))
		fmt.Fprintln(w)
	}
	for i := range n {
		if i > 0 {
			fmt.Fprintln(w)
		}
		renderPair(w, a.blocks[len(a.blocks)-n+i], b.blocks[len(b.blocks)-n+i])
	}

	var extra []unpaired
	for _, bl := range a.blocks[:len(a.blocks)-n] {
		extra = append(extra, unpaired{bl, a.word})
	}
	for _, bl := range b.blocks[:len(b.blocks)-n] {
		extra = append(extra, unpaired{bl, b.word})
	}
	if len(extra) > 0 {
		fmt.Fprintln(w)
		renderUnpaired(w, extra)
	}
}

// unpaired is a chain member with nothing on the other side to compare
// it against, and the word it came from.
type unpaired struct {
	block block
	owner string
}

func renderUnpaired(w io.Writer, extra []unpaired) {
	fmt.Fprintln(w, indent+stylize(ansiDim, "UNPAIRED"))
	wordW := 0
	for _, e := range extra {
		wordW = max(wordW, runeWidth(e.block.word))
	}
	for _, e := range extra {
		fmt.Fprintln(w, line(
			cell{"", 1, ""},
			cell{e.block.word, wordW, ansiBold},
			cell{fmt.Sprintf("%s of %s", e.block.role, e.owner), 0, ansiDim}))
	}
}

func renderPair(w io.Writer, a, b block) {
	// A shape split lists only the conjuncts that are there, with no
	// placeholder for a slot the surface elides. Drop the other side's
	// placeholders too, or every elision reads as a difference.
	if !a.decoded || !b.decoded {
		a.segs, b.segs = dropElided(a.segs), dropElided(b.segs)
	}
	rows := alignByKey(segKeys(a.segs), segKeys(b.segs))
	changed := renderSlotTable(w, a, b, rows)

	// A root block only makes sense when both words have a headword;
	// one modular adjunct in the pair means there's nothing to line up.
	rootChanged := a.head.Code != "" && b.head.Code != "" && a.head.Code != b.head.Code
	if rootChanged {
		fmt.Fprintln(w)
		renderRootDiff(w, a, b)
	}

	if notes := notedBlocks(a, b); len(notes) > 0 {
		fmt.Fprintln(w)
		renderNotes(w, notes)
	}

	// An undecoded word has no glossary, so every code on the other
	// side would read as a difference. Nothing to say beyond the shape.
	if !a.decoded || !b.decoded {
		return
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

// notedBlocks returns the blocks that carry a decoder complaint.
func notedBlocks(blocks ...block) []block {
	var out []block
	for _, s := range blocks {
		if s.note != "" {
			out = append(out, s)
		}
	}
	return out
}

// renderNotes prints why a word wouldn't decode, under the shape table
// that is all we could show for it.
func renderNotes(w io.Writer, notes []block) {
	fmt.Fprintln(w, indent+stylize(ansiDim, "UNCLASSIFIED"))
	wordW := 0
	for _, s := range notes {
		wordW = max(wordW, runeWidth(s.header()))
	}
	for _, s := range notes {
		fmt.Fprintln(w, line(
			cell{"", 1, ""},
			cell{s.header(), wordW, ansiBold},
			cell{s.note, 0, ansiDim}))
	}
}

// renderSlotTable prints the aligned slot breakdown. The two words head
// their own column groups; the phonetic columns share a width so the
// groups line up under them.
// Returns whether any row differed.
func renderSlotTable(w io.Writer, a, b block, rows [][2]int) bool {
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
		cell{a.header(), groupW, ansiBold},
		cell{b.header(), 0, ansiBold}))

	// One undecoded word means one side has no codes at all, so the
	// shape is the only thing the two have in common to diff on.
	byShape := !a.decoded || !b.decoded

	changed := false
	for _, r := range rows {
		sa, sb := segAt(a.segs, r[0]), segAt(b.segs, r[1])
		differs := r[0] < 0 || r[1] < 0 || sa.Chunk != sb.Chunk
		if !byShape {
			differs = differs || encodes(sa) != encodes(sb)
		}
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
func renderRootDiff(w io.Writer, a, b block) {
	fmt.Fprintln(w, indent+stylize(ansiDim, "ROOT"))
	wordW := max(runeWidth(a.header()), runeWidth(b.header()))
	for _, s := range []block{a, b} {
		fmt.Fprintf(w, "%s%s  %s", indent, stylize(ansiBold, padRunes(s.header(), wordW)), styleHeadwordCode(s.head.Code))
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
func glossDiffs(a, b block) []diffRow {
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
func renderGlossaryDiff(w io.Writer, a, b block, diffs []diffRow) {
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
		cell{a.header(), groupW, ansiBold},
		cell{b.header(), 0, ansiBold}))
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

func dropElided(segs []view.Segment) []view.Segment {
	var out []view.Segment
	for _, s := range segs {
		if !s.Elided {
			out = append(out, s)
		}
	}
	return out
}

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
