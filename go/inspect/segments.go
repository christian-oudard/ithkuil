package inspect

import (
	"fmt"
	"strings"

	"github.com/coudard/ithkuil/go/compose"
	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/lexicon"
	"github.com/coudard/ithkuil/go/parse"
)

// ElidedMark is the symbol shown for an elided slot.
const ElidedMark = "∅"

// Segment is one chunk of a surface formative paired with the slot it
// occupies and the grammatical codes it encodes.
type Segment struct {
	Chunk    string   // hyphen-decorated surface chunk
	Raw      string   // bare surface chunk (no hyphens)
	Slot     string   // "Cr", "Vr", "Ca", "Vx₁", "Cs₁", "Vc", "Vv", …
	Encodes  []string // codes encoded by this chunk
	Defaults bool     // every code is at its grammatical default
	Elided   bool     // chunk is a placeholder for an absent slot
	Ordinal  int      // 1-based index for affix Cs/Vx pairs; 0 otherwise
	Cluster  string   // affix Cs cluster (raw form), for lexicon lookup
	Degree   int      // affix degree (1-9) extracted from the paired Vx
}

// GlossaryEntry is one row of the glossary that follows the phonetic
// breakdown. CodeKind identifies the bucket: "function", "case",
// "affix", etc., used as the first column. The root identifier is
// returned separately by RootHeadword, not as a glossary row.
type GlossaryEntry struct {
	Category string
	Code     string
	Name     string
	Meaning  string
}

// RootHead summarizes the lexical content of a formative: the root
// cluster, stem, and specification together pick out the referent,
// and Meaning is the stem-selected lexicon entry.
type RootHead struct {
	Code    string // "\"m\" / S1 / BSC"
	Meaning string // "stem 1: linguistic utterance for communication"
}

// Headword returns the root identifier and its meaning for a parsed
// formative. Stem and Specification are folded into Code; everything
// else stays in the regular glossary returned by Glossary.
func Headword(f g.Formative, lex *lexicon.Lexicon) RootHead {
	root := strings.ToLower(string(f.SlotIII))
	if root == "" {
		return RootHead{}
	}
	stem := f.SlotII.Stem.String()
	spec := f.SlotIV.Specification.String()
	code := fmt.Sprintf("%q / %s / %s", root, stem, spec)
	meaning := ""
	if lex != nil {
		if e, ok := lex.Roots[root]; ok {
			meaning = stemMeaning(e, f.SlotII.Stem)
		}
	}
	return RootHead{Code: code, Meaning: meaning}
}

// Segments walks the surface conjuncts in lock-step with the parsed
// Formative and labels each chunk with its slot and encoded codes.
// Trailing elided slots (Vv when consonant-initial, Vc when no final
// vowel) are emitted as Elided segments.
//
// When lex is non-nil, affix Cs chunks have their abbreviation looked
// up (so Encodes carries "SYS" not the raw "ţř" cluster) and the
// paired Vx segment fills in the degree.
//
// Only the canonical consonant-initial and vowel-initial shapes are
// handled in detail. Slot I (Cc) shortcuts and ç-prefixed sentence
// starters fall through to a coarser labeling.
func Segments(word string, f g.Formative, lex *lexicon.Lexicon) []Segment {
	conjs := parse.SplitConjuncts(word)
	if len(conjs) == 0 {
		return nil
	}

	var segs []Segment
	i := 0

	// Slot II (Vv) — present only when the word is vowel-initial.
	vowelInitial := parse.IsVowelConjunct(conjs[0])
	if vowelInitial {
		segs = append(segs, Segment{
			Raw:     strings.ToLower(conjs[i]),
			Slot:    "Vv",
			Encodes: slotIICodes(f.SlotII),
			Defaults: f.SlotII == g.DefaultSlotII,
		})
		i++
	} else {
		segs = append(segs, Segment{
			Raw:      ElidedMark,
			Slot:     "Vv",
			Encodes:  slotIICodes(f.SlotII),
			Defaults: f.SlotII == g.DefaultSlotII,
			Elided:   true,
		})
	}

	// Slot III (Cr) — root.
	if i < len(conjs) {
		root := strings.ToLower(string(f.SlotIII))
		segs = append(segs, Segment{
			Raw:     strings.ToLower(conjs[i]),
			Slot:    "Cr",
			Encodes: []string{fmt.Sprintf("Root %q", root)},
		})
		i++
	}

	// Slot IV (Vr) — function · specification · context.
	if i < len(conjs) {
		segs = append(segs, Segment{
			Raw:      conjs[i],
			Slot:     "Vr",
			Encodes:  slotIVCodes(f.SlotIV),
			Defaults: f.SlotIV == g.DefaultSlotIV,
		})
		i++
	}

	// Slot V affixes (pre-Ca), CsVx reversed order: Cs first, then Vx.
	for ax, a := range f.SlotV {
		if i+1 >= len(conjs) {
			break
		}
		_, deg := parse.ClassifyAffixVowel(conjs[i+1])
		abbrev := affixAbbrev(a.Consonant, lex)
		segs = append(segs, Segment{
			Raw:     strings.ToLower(conjs[i]),
			Slot:    fmt.Sprintf("Cs₅%s", subscript(ax+1)),
			Encodes: []string{abbrev},
			Ordinal: ax + 1,
			Cluster: a.Consonant,
			Degree:  deg,
		})
		i++
		segs = append(segs, Segment{
			Raw:     strings.ToLower(conjs[i]),
			Slot:    fmt.Sprintf("Vx₅%s", subscript(ax+1)),
			Encodes: []string{fmt.Sprintf("DEG%d", deg)},
			Ordinal: ax + 1,
		})
		i++
	}

	// Slot VI (Ca).
	if i < len(conjs) {
		segs = append(segs, Segment{
			Raw:      conjs[i],
			Slot:     "Ca",
			Encodes:  slotVICodes(f.SlotVI),
			Defaults: f.SlotVI == g.DefaultSlotVI,
		})
		i++
	}

	// Slot VII affixes (post-Ca), VxCs standard order: Vx first, then Cs.
	for ax, a := range f.SlotVII {
		if i+1 >= len(conjs) {
			break
		}
		_, deg := parse.ClassifyAffixVowel(conjs[i])
		abbrev := affixAbbrev(a.Consonant, lex)
		segs = append(segs, Segment{
			Raw:     strings.ToLower(conjs[i]),
			Slot:    fmt.Sprintf("Vx%s", subscript(ax+1)),
			Encodes: []string{fmt.Sprintf("DEG%d", deg)},
			Ordinal: ax + 1,
		})
		i++
		segs = append(segs, Segment{
			Raw:     strings.ToLower(conjs[i]),
			Slot:    fmt.Sprintf("Cs%s", subscript(ax+1)),
			Encodes: []string{abbrev},
			Ordinal: ax + 1,
			Cluster: a.Consonant,
			Degree:  deg,
		})
		i++
	}

	// Slot VIII (VnCn) — handled as a single pair if remaining
	// conjuncts look like one.
	if f.SlotVIII != nil && i+1 <= len(conjs) {
		// Best effort: pull next V and C if available.
		if i < len(conjs) && parse.IsVowelConjunct(conjs[i]) {
			segs = append(segs, Segment{
				Raw:     strings.ToLower(conjs[i]),
				Slot:    "Vn",
				Encodes: []string{slotVIIIVnCode(f.SlotVIII)},
			})
			i++
		}
		if i < len(conjs) && parse.IsConsonantConjunct(conjs[i]) {
			segs = append(segs, Segment{
				Raw:     strings.ToLower(conjs[i]),
				Slot:    "Cn",
				Encodes: []string{slotVIIICnCode(f.SlotVIII)},
			})
			i++
		}
	}

	// Slot IX (Vc or Vk).
	if i < len(conjs) && parse.IsVowelConjunct(conjs[i]) {
		segs = append(segs, Segment{
			Raw:      conjs[i],
			Slot:     slotIXLabel(f),
			Encodes:  slotIXCodes(f.SlotIX),
			Defaults: slotIXIsDefault(f.SlotIX),
		})
		i++
	} else {
		segs = append(segs, Segment{
			Raw:      ElidedMark,
			Slot:     slotIXLabel(f),
			Encodes:  slotIXCodes(f.SlotIX),
			Defaults: slotIXIsDefault(f.SlotIX),
			Elided:   true,
		})
	}

	// Decorate each non-elided Segment.Chunk with hyphens based on
	// position. First non-elided gets trailing hyphen only; last gets
	// leading only; middle gets both.
	decorateHyphens(segs)
	return segs
}

// decorateHyphens fills Segment.Chunk with hyphens: word-initial
// chunks have only a trailing hyphen, word-final chunks only a
// leading one, and medial chunks have both. The leading edge is a
// single space (not a hyphen) for word-initial and elided rows so
// every chunk's first letter sits at the same column.
func decorateHyphens(segs []Segment) {
	first, last := -1, -1
	for i, s := range segs {
		if s.Elided {
			continue
		}
		if first < 0 {
			first = i
		}
		last = i
	}
	for i := range segs {
		if segs[i].Elided {
			segs[i].Chunk = " " + segs[i].Raw
			continue
		}
		left, right := "-", "-"
		if i == first {
			left = " "
		}
		if i == last {
			right = ""
		}
		segs[i].Chunk = left + segs[i].Raw + right
	}
}

func subscript(n int) string {
	digits := []rune("₀₁₂₃₄₅₆₇₈₉")
	if n < 0 || n > 9 {
		return fmt.Sprintf("%d", n)
	}
	return string(digits[n])
}

// slotIICodes returns the codes encoded by Vv (stem, version).
func slotIICodes(s g.SlotII) []string {
	return []string{s.Stem.String(), s.Version.String()}
}

// slotIVCodes returns the codes encoded by Vr (function/spec/context).
func slotIVCodes(s g.SlotIV) []string {
	return []string{s.Function.String(), s.Specification.String(), s.Context.String()}
}

// slotVICodes returns the codes encoded by Ca (config/aff/persp/ext/ess).
func slotVICodes(s g.SlotVI) []string {
	return []string{
		s.Configuration.String(), s.Affiliation.String(),
		s.Perspective.String(), s.Extension.String(), s.Essence.String(),
	}
}

func slotVIIIVnCode(s g.SlotVIII) string {
	switch v := s.(type) {
	case g.VnCnAspect:
		return v.Aspect.String()
	case g.VnCnValence:
		return v.Valence.String()
	case g.VnCnPhase:
		return v.Phase.String()
	case g.VnCnEffect:
		return v.Effect.String()
	case g.VnCnLevel:
		return v.Level.String()
	}
	return ""
}

func slotVIIICnCode(s g.SlotVIII) string {
	switch v := s.(type) {
	case g.VnCnAspect:
		return moodOrScopeShort(v.MS)
	case g.VnCnValence:
		return moodOrScopeShort(v.MS)
	case g.VnCnPhase:
		return moodOrScopeShort(v.MS)
	case g.VnCnEffect:
		return moodOrScopeShort(v.MS)
	case g.VnCnLevel:
		return moodOrScopeShort(v.MS)
	}
	return ""
}

func slotIXLabel(f g.Formative) string {
	if _, ok := f.SlotIX.(g.IllocValSlot); ok {
		return "Vk"
	}
	return "Vc"
}

func slotIXCodes(s g.SlotIX) []string {
	switch v := s.(type) {
	case g.CaseSlot:
		return []string{v.Case.String()}
	case g.IllocValSlot:
		return []string{v.Illocution.String(), v.Validation.String()}
	}
	return nil
}

func slotIXIsDefault(s g.SlotIX) bool {
	switch v := s.(type) {
	case g.CaseSlot:
		return v.Case == g.THM
	case g.IllocValSlot:
		return v.Validation == g.OBS
	}
	return true
}

// Glossary returns one row per unique grammar code referenced by
// segs, in the order codes first appear. The root cluster, stem, and
// specification are intentionally omitted — they belong to Headword,
// not to the per-code glossary.
func Glossary(word string, f g.Formative, segs []Segment, lex *lexicon.Lexicon) []GlossaryEntry {
	type seenKey struct{ cat, code string }
	seen := map[seenKey]bool{}
	var out []GlossaryEntry

	add := func(cat, code, name, meaning string) {
		k := seenKey{cat, code}
		if seen[k] {
			return
		}
		seen[k] = true
		out = append(out, GlossaryEntry{Category: cat, Code: code, Name: name, Meaning: meaning})
	}

	// Skip codes already represented by the Headword (stem,
	// specification) so they don't appear twice.
	skipCode := map[string]bool{
		f.SlotII.Stem.String():          true,
		f.SlotIV.Specification.String(): true,
	}

	for _, s := range segs {
		if s.Slot == "Cr" {
			continue // root lives in the headword
		}
		for _, code := range s.Encodes {
			if strings.HasPrefix(code, "Root ") {
				continue
			}
			if strings.HasPrefix(code, "DEG") {
				continue // degree pairs with a Cs row
			}
			if skipCode[code] {
				continue
			}
			if strings.HasPrefix(s.Slot, "Cs") {
				name, meaning := affixDegreeGloss(s.Cluster, s.Degree, lex)
				if meaning == "" {
					meaning = name
				}
				codeWithDegree := code
				if s.Degree > 0 {
					codeWithDegree = fmt.Sprintf("%s/%d", code, s.Degree)
				}
				add("affix", codeWithDegree, name, meaning)
				continue
			}
			cat := categoryForCode(code, s.Slot)
			add(cat, code, g.Name(code), g.Meaning(code))
		}
	}
	return out
}

// categoryForCode returns the human-readable category name for a
// grammar code ("STA" → "function"), falling back to a slot-derived
// label when no precise category is known.
func categoryForCode(code, slot string) string {
	hits := compose.LookupGrammar(code)
	if len(hits) > 0 {
		cat := hits[0].Category
		// Trim sub-category suffix like "Case/Transrelative".
		if i := strings.Index(cat, "/"); i >= 0 {
			cat = cat[:i]
		}
		return strings.ToLower(cat)
	}
	return categoryForSlot(slot)
}

func categoryForSlot(slot string) string {
	switch {
	case slot == "Vv":
		return "version" // we'll add stem-by-side
	case slot == "Cr":
		return "root"
	case slot == "Vr":
		return "Vr" // expanded below
	case slot == "Ca":
		return "Ca"
	case strings.HasPrefix(slot, "Vx"):
		return "affix degree"
	case strings.HasPrefix(slot, "Cs"):
		return "affix"
	case slot == "Vn":
		return "slot VIII"
	case slot == "Cn":
		return "mood/scope"
	case slot == "Vc":
		return "case"
	case slot == "Vk":
		return "illocution"
	}
	return slot
}

// affixAbbrev returns the lexicon abbreviation for an affix consonant
// cluster, or the cluster itself when the lexicon has no entry.
func affixAbbrev(cs string, lex *lexicon.Lexicon) string {
	if lex != nil {
		if e, ok := lex.Affixes[cs]; ok {
			return e.Abbrev
		}
	}
	return cs
}

// affixDegreeGloss returns the degree-specific meaning for an affix
// at the given degree (1-9), or empty string when unavailable.
func affixDegreeGloss(cs string, degree int, lex *lexicon.Lexicon) (name, meaning string) {
	if lex == nil {
		return "", ""
	}
	e, ok := lex.Affixes[cs]
	if !ok {
		return "", ""
	}
	name = e.Description
	if degree >= 1 && degree <= len(e.Degrees) {
		meaning = e.Degrees[degree-1]
	}
	return name, meaning
}

// stemMeaning returns the stem-selected meaning string from a root
// entry. Falls back to stem 0 / stem 1 if the requested stem is empty.
func stemMeaning(e lexicon.RootEntry, stem g.Stem) string {
	stems := []string{e.Stem1, e.Stem2, e.Stem3, e.Stem0}
	idx := 0
	switch stem {
	case g.S1:
		idx = 0
	case g.S2:
		idx = 1
	case g.S3:
		idx = 2
	case g.S0:
		idx = 3
	}
	if stems[idx] != "" {
		return fmt.Sprintf("stem %d: %s", stemNum(stem), stems[idx])
	}
	for i, s := range stems {
		if s != "" {
			return fmt.Sprintf("stem %d (fallback): %s", i, s)
		}
	}
	return ""
}

func stemNum(s g.Stem) int {
	switch s {
	case g.S0:
		return 0
	case g.S1:
		return 1
	case g.S2:
		return 2
	case g.S3:
		return 3
	}
	return 1
}
