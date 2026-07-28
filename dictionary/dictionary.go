// Package dictionary reads the lexicon's English glosses backwards: it
// indexes them by headword so an English word can be looked up as the
// Ithkuil lexical cores that express it.
//
// The index is derived, not stored. It says what the lexicon already
// happens to name in English, which is a fraction of the language.
package dictionary

import (
	"regexp"
	"sort"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
)

// Sense is one Ithkuil lexical core that an English headword names: the
// root plus the Slot II/IV coordinates that select this reading of it.
// Case, illocution, and the rest of the formative belong to the
// sentence, not to a dictionary entry, and are absent here.
type Sense struct {
	Cr      string
	Stem    g.Stem
	Version g.Version
	SlotIV  g.SlotIV
	// Gloss is the whole source cell the headword was read out of,
	// including the parts that are explanation rather than headword.
	Gloss string
}

// Formative builds the minimal renderable formative for the sense: the
// lexical core in the thematic case.
func (s Sense) Formative() g.Formative {
	f := g.MinimalFormative(s.Cr)
	f.Root = g.CrRoot{
		Cluster: s.Cr,
		Stem:    s.Stem,
		Version: s.Version,
		SlotIV:  s.SlotIV,
	}
	return f
}

// Index maps a lowercase English headword to the senses naming it.
type Index map[string][]Sense

// Lookup returns the senses for word, or nil. Matching is
// case-insensitive against the whole headword.
func (ix Index) Lookup(word string) []Sense {
	return ix[strings.ToLower(strings.TrimSpace(word))]
}

// Build indexes every gloss cell of every root by headword.
func Build(roots map[string]lexicon.RootEntry) Index {
	ix := Index{}
	for cr, e := range roots {
		for _, c := range cells(cr, e) {
			for _, h := range Headwords(c.Gloss) {
				ix[h] = append(ix[h], c)
			}
		}
	}
	for _, senses := range ix {
		sort.Slice(senses, func(i, j int) bool { return less(senses[i], senses[j]) })
	}
	return ix
}

// cells enumerates a root entry's filled gloss cells, each carrying the
// grammatical coordinates its column stands for.
func cells(cr string, e lexicon.RootEntry) []Sense {
	var out []Sense
	add := func(gloss string, s Sense) {
		if gloss == "" {
			return
		}
		s.Cr, s.Gloss = cr, gloss
		out = append(out, s)
	}
	add(e.Stem0, Sense{Stem: g.S0})
	add(e.Stem1, Sense{Stem: g.S1})
	add(e.Stem2, Sense{Stem: g.S2})
	add(e.Stem3, Sense{Stem: g.S3})
	add(e.Contential, Sense{Stem: g.S0, SlotIV: g.SlotIV{Specification: g.CTE}})
	add(e.Constitutive, Sense{Stem: g.S0, SlotIV: g.SlotIV{Specification: g.CSV}})
	add(e.Dynamic, Sense{Stem: g.S0, SlotIV: g.SlotIV{Function: g.DYN}})
	stems := [...]g.Stem{g.S1, g.S2, g.S3}
	for i, gloss := range e.Objective {
		if i < len(stems) {
			add(gloss, Sense{Stem: stems[i], SlotIV: g.SlotIV{Specification: g.OBJ}})
		}
	}
	for i, gloss := range e.Completive {
		if i < len(stems) {
			add(gloss, Sense{Stem: stems[i], Version: g.CPT})
		}
	}
	return out
}

func less(a, b Sense) bool {
	if len(a.Cr) != len(b.Cr) {
		return len(a.Cr) < len(b.Cr)
	}
	if a.Cr != b.Cr {
		return a.Cr < b.Cr
	}
	if a.Stem != b.Stem {
		return a.Stem < b.Stem
	}
	if a.Version != b.Version {
		return a.Version < b.Version
	}
	if a.SlotIV.Specification != b.SlotIV.Specification {
		return a.SlotIV.Specification < b.SlotIV.Specification
	}
	return a.SlotIV.Function < b.SlotIV.Function
}

var (
	// Parenthesised and bracketed material is explanation, not headword.
	// A gloss that is nothing but a bracketed placeholder, e.g.
	// "[carrier root]", is left with no headword at all.
	aside = regexp.MustCompile(`\([^)]*\)|\[[^\]]*\]`)
	// Footnote marks the spreadsheet hangs off the end of a gloss.
	footnote = regexp.MustCompile(`[†*?\d\s]+$`)
	infin    = regexp.MustCompile(`^(?:to be|to)\s+`)
)

// Headwords reads the English headwords out of one gloss cell. A
// semicolon separates distinct senses; a slash separates interchangeable
// wordings of one sense. Commas do not separate senses: in these glosses
// they almost always separate modifiers within one.
func Headwords(gloss string) []string {
	var out []string
	for _, sense := range strings.Split(aside.ReplaceAllString(gloss, " "), ";") {
		for _, w := range strings.Split(sense, "/") {
			w = strings.Join(strings.Fields(strings.ToLower(w)), " ")
			w = footnote.ReplaceAllString(strings.Trim(w, " ."), "")
			w = infin.ReplaceAllString(w, "")
			if w != "" {
				out = append(out, w)
			}
		}
	}
	return out
}
