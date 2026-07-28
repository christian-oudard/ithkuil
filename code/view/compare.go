package view

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/slots"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// Block is one formative's parsed breakdown, in the same three pieces
// the detailed parse view uses. An unclassified word fills in Segs from
// the shape split alone: Decoded is false, Gloss and Head are empty,
// and Note carries the decoder's complaint. Role names a chain member's
// part ("head", "Type1 dependent") and is empty otherwise.
type Block struct {
	Word    string
	Role    string
	Segs    []Segment
	Gloss   []GlossaryEntry
	Head    RootHead
	Decoded bool
	Note    string
}

// Header is how a Block titles itself: the surface, plus the part it
// plays when it is one member of a chain.
func (b Block) Header() string {
	if b.Role == "" {
		return b.Word
	}
	return b.Word + " [" + b.Role + "]"
}

// Side is one word to compare: a single formative or adjunct is one
// block, a concatenation chain is one block per member, in surface
// order (dependents first, parent last, §3.1.7).
type Side struct {
	Word   string
	Blocks []Block
}

// BuildSide tokenizes and analyzes one word into the blocks that can be
// compared. Formatives and modular adjuncts give a full breakdown; a
// chain gives one per member; an unclassified word gives the shape
// split, which is what makes a good word comparable to a bad one.
// Referentials and the rest have no slot structure at all, and are an
// error here. Phonotactic validation is the caller's to run first.
func BuildSide(word string, lex *lexicon.Lexicon) (Side, error) {
	tokens := tokenize.Tokenize(word)
	if len(tokens) != 1 {
		return Side{}, fmt.Errorf("%s: expected one word, got %d tokens", word, len(tokens))
	}
	surface := strings.ToLower(tokens[0].Surface())
	switch t := tokens[0].(type) {
	case tokenize.FormativeWord:
		return Side{surface, []Block{FormativeBlock(t.Text, t.Formative, "", lex)}}, nil
	case tokenize.ModularWord:
		segs := SegmentsModular(t.Text, t.Modular, t.MarksMood)
		return Side{surface, []Block{{
			Word:    strings.ToLower(t.Text),
			Segs:    segs,
			Gloss:   GlossaryModular(segs),
			Decoded: true,
		}}}, nil
	case tokenize.ConcatenatedFormativeWord:
		return Side{surface, chainBlocks(t, lex)}, nil
	case tokenize.UnknownWord:
		bl, err := unknownBlock(t.Text)
		if err != nil {
			return Side{}, err
		}
		return Side{surface, []Block{bl}}, nil
	default:
		return Side{}, fmt.Errorf("%s: %s has no slot breakdown to compare", word, Type(tokens[0]))
	}
}

// FormativeBlock builds the breakdown of one formative.
func FormativeBlock(text string, f g.Formative, role string, lex *lexicon.Lexicon) Block {
	segs := Segments(text, f, lex)
	return Block{
		Word:    strings.ToLower(text),
		Role:    role,
		Segs:    segs,
		Gloss:   Glossary(text, f, segs, lex),
		Head:    Headword(f, lex),
		Decoded: true,
	}
}

// chainBlocks splits a concatenation chain into one block per member.
// The chain's surface is hyphen-joined, so splitting on "-" recovers
// each member's own surface. Dependents lead and the parent comes last
// (§3.1.7), but the Cc marker is what tells them apart, not position.
func chainBlocks(cw tokenize.ConcatenatedFormativeWord, lex *lexicon.Lexicon) []Block {
	parts := strings.Split(cw.Text, "-")
	formatives := cw.Chain.Formatives()
	blocks := make([]Block, 0, len(formatives))
	for i, f := range formatives {
		role := "head"
		if f.Concat != g.ConcatNone {
			role = f.Concat.String() + " dependent"
		}
		surface := ""
		if i < len(parts) {
			surface = parts[i]
		}
		blocks = append(blocks, FormativeBlock(surface, f, role, lex))
	}
	return blocks
}

// unknownBlock builds a block from the shape split of a word no
// classifier claimed. slots.Parse assigns conjuncts to slots by shape
// alone, so it still succeeds where ToGrammar rejects a value, and the
// formative decoder's complaint says which value that was. Only a word
// whose shape won't split at all is beyond comparing.
func unknownBlock(word string) (Block, error) {
	layout, err := slots.Parse(word)
	if err != nil {
		return Block{}, fmt.Errorf("%s: %v", word, err)
	}
	note := ""
	if _, err := slots.ToGrammar(layout); err != nil {
		note = fmt.Sprintf("as a formative: %v", err)
	}
	return Block{
		Word: strings.ToLower(word),
		Segs: LayoutSegments(layout),
		Note: note,
	}, nil
}

// Pair is two blocks lined up against each other, one from each side.
type Pair struct {
	A, B Block
}

// Unpaired is a chain member with nothing on the other side to compare
// it against, and the word it came from.
type Unpaired struct {
	Block Block
	Owner string
}

// PairSides pairs the two sides' formatives off from the parent end. A
// chain's parent is its last member (§3.1.7), so that is what a
// standalone word is the counterpart of; a longer chain's leading
// dependents go unpaired.
func PairSides(a, b Side) ([]Pair, []Unpaired) {
	n := min(len(a.Blocks), len(b.Blocks))
	pairs := make([]Pair, 0, n)
	for i := range n {
		pairs = append(pairs, Pair{
			A: a.Blocks[len(a.Blocks)-n+i],
			B: b.Blocks[len(b.Blocks)-n+i],
		})
	}
	var extra []Unpaired
	for _, bl := range a.Blocks[:len(a.Blocks)-n] {
		extra = append(extra, Unpaired{bl, a.Word})
	}
	for _, bl := range b.Blocks[:len(b.Blocks)-n] {
		extra = append(extra, Unpaired{bl, b.Word})
	}
	return pairs, extra
}

// SlotRow is one row of a slot comparison: the slot label and each
// side's segment, with a blank segment where that side has no such
// slot.
type SlotRow struct {
	Slot    string
	A, B    Segment
	Differs bool
}

// SlotDiff aligns two blocks' slot breakdowns and marks the rows that
// disagree. When either side failed to decode, only the shape is
// compared: one side has no codes at all, so comparing them would
// report every slot as a difference.
func SlotDiff(a, b Block) []SlotRow {
	segsA, segsB := a.Segs, b.Segs
	byShape := !a.Decoded || !b.Decoded
	if byShape {
		// A shape split lists only the conjuncts that are there, with
		// no placeholder for a slot the surface elides. Drop the other
		// side's placeholders too, or every elision reads as a
		// difference.
		segsA, segsB = dropElided(segsA), dropElided(segsB)
	}
	var rows []SlotRow
	for _, r := range AlignByKey(segKeys(segsA), segKeys(segsB)) {
		sa, sb := segAt(segsA, r[0]), segAt(segsB, r[1])
		differs := r[0] < 0 || r[1] < 0 || sa.Chunk != sb.Chunk
		if !byShape {
			differs = differs || Encodes(sa) != Encodes(sb)
		}
		slot := sa.Slot
		if slot == "" {
			slot = sb.Slot
		}
		rows = append(rows, SlotRow{Slot: slot, A: sa, B: sb, Differs: differs})
	}
	return rows
}

// RootDiffers reports whether the two blocks name different lexical
// identities. A root block only makes sense when both words have a
// headword; one modular adjunct in the pair means there is nothing to
// line up.
func RootDiffers(a, b Block) bool {
	return a.Head.Code != "" && b.Head.Code != "" && a.Head.Code != b.Head.Code
}

// GlossDiffRow is one glossary category whose code changed between the
// two words; a zero-valued side means the category is absent there.
type GlossDiffRow struct {
	Category string
	A, B     GlossaryEntry
}

// GlossDiff aligns the two glossaries by category and keeps only the
// rows where the code changed. An undecoded word has no glossary, so
// there is nothing to say beyond the shape.
func GlossDiff(a, b Block) []GlossDiffRow {
	if !a.Decoded || !b.Decoded {
		return nil
	}
	var diffs []GlossDiffRow
	for _, r := range AlignByKey(glossKeys(a.Gloss), glossKeys(b.Gloss)) {
		ea, eb := glossAt(a.Gloss, r[0]), glossAt(b.Gloss, r[1])
		if ea.Code == eb.Code {
			continue
		}
		category := ea.Category
		if category == "" {
			category = eb.Category
		}
		diffs = append(diffs, GlossDiffRow{category, ea, eb})
	}
	return diffs
}

// Encodes joins a segment's codes the way both views print them.
func Encodes(s Segment) string { return strings.Join(s.Encodes, " / ") }

func dropElided(segs []Segment) []Segment {
	var out []Segment
	for _, s := range segs {
		if !s.Elided {
			out = append(out, s)
		}
	}
	return out
}

func segKeys(segs []Segment) []string {
	keys := make([]string, len(segs))
	for i, s := range segs {
		keys[i] = s.Slot
	}
	return keys
}

func glossKeys(entries []GlossaryEntry) []string {
	keys := make([]string, len(entries))
	for i, e := range entries {
		keys[i] = e.Category
	}
	return keys
}

// segAt returns the segment at i, or a blank one when i is a gap.
func segAt(segs []Segment, i int) Segment {
	if i < 0 {
		return Segment{}
	}
	return segs[i]
}

func glossAt(entries []GlossaryEntry, i int) GlossaryEntry {
	if i < 0 {
		return GlossaryEntry{}
	}
	return entries[i]
}

// AlignByKey pairs up two sequences by their keys, preserving order and
// matching as many keys as possible (a longest-common-subsequence
// walk). The result holds index pairs; -1 marks a gap on that side, so
// a slot present in only one of the words still gets its own row.
func AlignByKey(ka, kb []string) [][2]int {
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
