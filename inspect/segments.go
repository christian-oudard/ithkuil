package inspect

import (
	"fmt"
	"strings"

	"github.com/christian-oudard/ithkuil/compose"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/surface"
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
// formative. Code combines the root, stem, and specification — the
// three pieces that together identify the lexical content.
func Headword(f g.Formative, lex *lexicon.Lexicon) RootHead {
	switch r := f.Root.(type) {
	case g.CrRoot:
		root := strings.ToLower(r.Cluster)
		if root == "" {
			return RootHead{}
		}
		code := fmt.Sprintf("%q / %s / %s", root, r.Stem, r.SlotIV.Specification)
		meaning := ""
		if lex != nil {
			if e, ok := lex.Roots[root]; ok {
				meaning = stemMeaning(e, r.Stem)
			}
		}
		return RootHead{Code: code, Meaning: meaning}
	case g.CsRoot:
		cs := strings.ToLower(r.Cs)
		code := fmt.Sprintf("(Cs)%q / DEG%d", cs, r.Degree)
		meaning := ""
		if lex != nil {
			if e, ok := lex.Affixes[r.Cs]; ok && r.Degree >= 1 && r.Degree <= len(e.Degrees) {
				meaning = fmt.Sprintf("%s (degree %d): %s", e.Description, r.Degree, e.Degrees[r.Degree-1])
			}
		}
		return RootHead{Code: code, Meaning: meaning}
	case g.RefRoot:
		code := fmt.Sprintf("(Ref)%q / %s", strings.ToLower(r.C1), r.SlotIV.Specification)
		return RootHead{Code: code, Meaning: "referential root"}
	}
	return RootHead{}
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
// CrRoot is handled in full detail; CsRoot and RefRoot fall through
// to a placeholder rendering. Slot I shortcut prefixes (w/y/hl/hm/
// hr/hn) are recognized when the conjuncts don't begin where the
// parser says the root starts.
func Segments(word string, f g.Formative, lex *lexicon.Lexicon) []Segment {
	conjs := parse.SplitConjuncts(word)
	if len(conjs) == 0 {
		return nil
	}
	switch r := f.Root.(type) {
	case g.CrRoot:
		return segmentsCrRoot(conjs, f, r, lex)
	case g.CsRoot:
		return segmentsCsRoot(conjs, f, r, lex)
	case g.RefRoot:
		return segmentsRefRoot(conjs, f, r, lex)
	}
	return nil
}

func segmentsCrRoot(conjs []string, f g.Formative, cr g.CrRoot, lex *lexicon.Lexicon) []Segment {
	var segs []Segment
	i := 0
	rootLower := strings.ToLower(cr.Cluster)
	shortcut := false

	// Detect a Slot I shortcut. When the first consonant conjunct
	// isn't the parser's root cluster, the leading conjunct(s) form
	// a Cc prefix (h-, hw-, w-, y-, hl-, hm-, hr-, hn-). For these
	// forms Vr is elided and Ca is encoded in the (Cc, Vv) pair, so
	// the table after Cr skips straight to Slot V/VII affixes.
	if parse.IsConsonantConjunct(conjs[0]) && strings.ToLower(conjs[0]) != rootLower {
		segs = append(segs, Segment{
			Raw:     strings.ToLower(conjs[0]),
			Slot:    "Cc",
			Encodes: []string{"Slot I shortcut"},
		})
		shortcut = true
		i++
	}

	// Slot II (Vv) — explicit when vowel-initial (at position i) or
	// elided otherwise.
	stemVer := []string{cr.Stem.String(), cr.Version.String()}
	if i < len(conjs) && parse.IsVowelConjunct(conjs[i]) {
		segs = append(segs, Segment{
			Raw:      strings.ToLower(conjs[i]),
			Slot:     "Vv",
			Encodes:  stemVer,
			Defaults: cr.Stem == g.S1 && cr.Version == g.PRC,
		})
		i++
	} else {
		segs = append(segs, Segment{
			Raw:      ElidedMark,
			Slot:     "Vv",
			Encodes:  stemVer,
			Defaults: cr.Stem == g.S1 && cr.Version == g.PRC,
			Elided:   true,
		})
	}

	// Slot III (Cr) — root. Use the parser's authoritative cluster.
	segs = append(segs, Segment{
		Raw:     rootLower,
		Slot:    "Cr",
		Encodes: []string{fmt.Sprintf("Root %q", rootLower)},
	})
	if i < len(conjs) && strings.ToLower(conjs[i]) == rootLower {
		i++
	}

	// Slot IV (Vr) — elided for Slot I shortcut forms (Vr defaults
	// to STA/BSC/EXS, encoded into the Cc choice instead).
	if !shortcut {
		if i < len(conjs) && parse.IsVowelConjunct(conjs[i]) {
			segs = append(segs, Segment{
				Raw:      strings.ToLower(conjs[i]),
				Slot:     "Vr",
				Encodes:  []string{cr.SlotIV.Function.String(), cr.SlotIV.Specification.String(), cr.SlotIV.Context.String()},
				Defaults: cr.SlotIV == g.DefaultSlotIV,
			})
			i++
		}
	}

	// Slot V affixes (pre-Ca), CsVx reversed order.
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

	// Slot VI (Ca) — elided for shortcut forms (Ca derived from
	// the Cc+Vv pair). For regular forms, the next conjunct is Ca.
	if !shortcut && i < len(conjs) && parse.IsConsonantConjunct(conjs[i]) {
		segs = append(segs, Segment{
			Raw:      strings.ToLower(conjs[i]),
			Slot:     "Ca",
			Encodes:  slotVICodes(f.SlotVI),
			Defaults: f.SlotVI == g.DefaultSlotVI,
		})
		i++
	}

	// Slot VII affixes (post-Ca), VxCs standard order.
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
	if f.SlotVIII != nil {
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
				Encodes: []string{slotVIIICnCode(f.SlotVIII, f.Final)},
			})
			i++
		}
	}

	// Slot IX (Vc or Vk).
	slot, codes, isDefault := slotIXLabelCodes(f.Final)
	if i < len(conjs) && parse.IsVowelConjunct(conjs[i]) {
		segs = append(segs, Segment{
			Raw:      strings.ToLower(conjs[i]),
			Slot:     slot,
			Encodes:  codes,
			Defaults: isDefault,
		})
		i++
	} else {
		segs = append(segs, Segment{
			Raw:      ElidedMark,
			Slot:     slot,
			Encodes:  codes,
			Defaults: isDefault,
			Elided:   true,
		})
	}

	decorateHyphens(segs)
	return segs
}

// SegmentsModular returns one segment per phonetic chunk of a
// modular adjunct: an optional w/y prefix, zero-to-three (Vn, Cn)
// pairs, and an optional final vowel. The label "Vn₁/Cn₁/Vn₂/…"
// makes each pair's slot inside the adjunct visible.
//
// marksMood disambiguates the Cn surface form (which is shared between
// Mood and Case-Scope). When nil, no adjacent formative was found and
// Cn defaults to Mood — matching the spec's verbal-formative reading.
func SegmentsModular(word string, ma g.ModularAdjunct, marksMood *bool) []Segment {
	asMood := marksMood == nil || *marksMood
	var segs []Segment
	if ma.Prefix != "" {
		segs = append(segs, Segment{
			Raw:     strings.ToLower(ma.Prefix),
			Slot:    "scope",
			Encodes: []string{prefixCode(ma.Prefix)},
		})
	}
	for i, p := range ma.Pairs {
		idx := subscript(i + 1)
		segs = append(segs, Segment{
			Raw:     strings.ToLower(p.Vn),
			Slot:    fmt.Sprintf("Vn%s", idx),
			Encodes: []string{vnAsCode(p.Vn, p.Cn)},
		})
		segs = append(segs, Segment{
			Raw:     strings.ToLower(p.Cn),
			Slot:    fmt.Sprintf("Cn%s", idx),
			Encodes: []string{cnAsCode(p.Cn, asMood)},
		})
	}
	if ma.Final != "" {
		// Per §4.3, slot 4 of a modular adjunct is V_H (a scope
		// marker) only when the adjunct has ultimate stress AND
		// there are affixes in slots 2/3 for V_H to scope over.
		// Otherwise slot 4 is V_N — another aspect/valence/etc.
		// position with default Cn (which we render as a lone
		// aspect at default FAC mood / CCN case-scope).
		_, stress := surface.Strip(word)
		isVH := stress == surface.Ultimate && len(ma.Pairs) > 0
		if isVH {
			segs = append(segs, Segment{
				Raw:     strings.ToLower(ma.Final),
				Slot:    "Vh",
				Encodes: []string{vhCode(ma.Final)},
			})
		} else {
			segs = append(segs, Segment{
				Raw:     strings.ToLower(ma.Final),
				Slot:    "Vn",
				Encodes: []string{vnAsCode(ma.Final, "")},
			})
		}
	}
	decorateHyphens(segs)
	return segs
}

// GlossaryModular returns one row per unique code referenced by a
// modular adjunct's segments. Scope-prefix and V_H rows get prose
// meanings since they don't map to the standard grammar.Name table.
func GlossaryModular(segs []Segment) []GlossaryEntry {
	seen := map[string]bool{}
	var out []GlossaryEntry
	for _, s := range segs {
		for _, code := range s.Encodes {
			if code == "" || seen[s.Slot+"|"+code] {
				continue
			}
			seen[s.Slot+"|"+code] = true
			// Default: derive the category from the code itself
			// (PRG → "aspect", SUB → "mood", etc.), not from the
			// slot label (Vn₁/Cn₁ are slot positions, not
			// categories).
			cat := categoryForCode(code, s.Slot)
			name := g.Name(code)
			meaning := g.Meaning(code)
			switch s.Slot {
			case "scope":
				cat = "scope"
				name = ""
				meaning = prefixMeaning(s.Raw)
			case "Vh":
				cat = "scope"
				name = ""
				meaning = vhMeaning(s.Raw)
			case "Cn₁", "Cn₂", "Cn₃":
				if code == "CmAspect" || code == "CmOther" {
					cat = "marker"
					name = cmName(code)
					meaning = cmMeaning(code)
				}
			}
			out = append(out, GlossaryEntry{
				Category: cat, Code: code, Name: name, Meaning: meaning,
			})
		}
	}
	return out
}

// prefixCode returns a short tag for a w/y scope prefix that's
// distinct from the bare letter shown in PHONETIC.
func prefixCode(p string) string {
	switch p {
	case "w":
		return "→parent"
	case "y":
		return "→concat"
	}
	return p
}

// vhCode returns a short tag for the Vh scope vowel indicating its
// scope reach. Distinct from the bare letter shown in PHONETIC. The
// vowel may carry an acute (ultimate stress mark) — strip it before
// the lookup so "ó" matches "o".
func vhCode(v string) string {
	switch parse.NormalizeAccents(v) {
	case "a":
		return "→Case/Mood/Val/Illoc"
	case "e":
		return "→Case/Mood"
	case "i", "u":
		return "→formative"
	case "o":
		return "→formative+adjuncts"
	}
	return "→" + v
}

func prefixMeaning(p string) string {
	switch p {
	case "w":
		return "applies to parent formative only"
	case "y":
		return "applies to concatenated formative only"
	}
	return ""
}

func cmName(code string) string {
	switch code {
	case "CmAspect":
		return "Cm (n)"
	case "CmOther":
		return "Cm (ň)"
	}
	return ""
}

func cmMeaning(code string) string {
	switch code {
	case "CmAspect":
		return "marks the previous Vn as an aspect"
	case "CmOther":
		return "marks the previous Vn as valence/phase/effect/level"
	}
	return ""
}

// vnAsCode identifies the grammatical category a modular Vn encodes.
// Spec rules:
//
//   - Slot 2 Cn = w/y/hw/hrw/hmw/hnw/hňw → Vn is an Aspect.
//   - Slot 2 Cn = h/hl/hr/hm/hn/hň → Vn is one of Valence/Phase/
//     Effect/Level (determined by Vn's vowel-series).
//   - Slot 3 Cm = "n" → preceding Vn is an Aspect.
//   - Slot 3 Cm = "ň" → preceding Vn is Valence/Phase/Effect/Level.
//   - Lone final vowel (no Cn) → Aspect.
func vnAsCode(vn, cn string) string {
	if cn == "" || isAspectCn(cn) || cn == "n" {
		if a, ok := parse.ParseVnAspect(vn); ok {
			return a.String()
		}
		return "Aspect?"
	}
	if v, ok := parse.ParseVnValence(vn); ok {
		return v.String()
	}
	if p, ok := parse.ParseVnPhase(vn); ok {
		return p.String()
	}
	if e, ok := parse.ParseVnEffect(vn); ok {
		return e.String()
	}
	if l, ok := parse.ParseVnLevel(vn); ok {
		return l.String()
	}
	return "Vn?"
}

func isAspectCn(cn string) bool {
	switch cn {
	case "w", "y", "hw", "hrw", "hmw", "hnw", "hňw":
		return true
	}
	return false
}

func cnAsCode(cn string, asMood bool) string {
	if asMood {
		if isAspectCn(cn) {
			if m, ok := parse.ParseCnMoodP2(cn); ok {
				return m.String()
			}
		}
		if m, ok := parse.ParseCnMood(cn); ok {
			return m.String()
		}
	} else if cs, ok := parse.ParseCnCaseScope(cn); ok {
		return cs.String()
	}
	switch cn {
	case "n":
		return "CmAspect"
	case "ň":
		return "CmOther"
	}
	return "Cn?"
}

func vhMeaning(v string) string {
	switch parse.NormalizeAccents(v) {
	case "a":
		return "scope over Case/Mood + Validation+Illocution"
	case "e":
		return "scope over Case/Mood"
	case "i", "u":
		return "scope over formative only"
	case "o":
		return "scope over formative + adjacent affixual adjuncts"
	}
	return "V_H " + v
}

// segmentsCsRoot handles Cs-root formatives (§4.2): special Vv
// (ëi/eë/ëu/oë) + Cs + Vr-as-affix-degree + Ca + post-Ca tail. The
// "root" here is the affix cluster Cs at a specific degree.
func segmentsCsRoot(conjs []string, f g.Formative, cs g.CsRoot, lex *lexicon.Lexicon) []Segment {
	var segs []Segment
	if len(conjs) < 4 {
		return nil
	}
	csLower := strings.ToLower(cs.Cs)

	// Vv (special) — encodes (Version, Function).
	segs = append(segs, Segment{
		Raw:     strings.ToLower(conjs[0]),
		Slot:    "Vv",
		Encodes: []string{cs.Version.String(), cs.Function.String()},
	})

	// Cs as root.
	segs = append(segs, Segment{
		Raw:     csLower,
		Slot:    "Cs (root)",
		Encodes: []string{fmt.Sprintf("(Cs)%q at degree %d", csLower, cs.Degree)},
	})

	// Vr (special) — encodes (Degree, Context).
	segs = append(segs, Segment{
		Raw:     strings.ToLower(conjs[2]),
		Slot:    "Vr",
		Encodes: []string{fmt.Sprintf("DEG%d", cs.Degree), cs.Context.String()},
	})

	// Ca.
	segs = append(segs, Segment{
		Raw:      strings.ToLower(conjs[3]),
		Slot:     "Ca",
		Encodes:  slotVICodes(f.SlotVI),
		Defaults: f.SlotVI == g.DefaultSlotVI,
	})

	// Post-Ca tail: Slot VII affixes, Slot VIII, Slot IX.
	postCa(&segs, conjs, 4, f, lex)
	decorateHyphens(segs)
	return segs
}

// segmentsRefRoot handles reference-root formatives (§5.3): special
// Vv (ae/ea) + C1 referential + normal Vr + Ca + post-Ca tail.
func segmentsRefRoot(conjs []string, f g.Formative, rr g.RefRoot, lex *lexicon.Lexicon) []Segment {
	var segs []Segment
	if len(conjs) < 4 {
		return nil
	}
	c1Lower := strings.ToLower(rr.C1)

	// Vv (special) — encodes Version (Function implicit).
	segs = append(segs, Segment{
		Raw:     strings.ToLower(conjs[0]),
		Slot:    "Vv",
		Encodes: []string{rr.Version.String()},
	})

	// C1 as root.
	segs = append(segs, Segment{
		Raw:     c1Lower,
		Slot:    "C1 (ref)",
		Encodes: []string{fmt.Sprintf("Ref %q", c1Lower)},
	})

	// Vr — normal SlotIV.
	segs = append(segs, Segment{
		Raw:      strings.ToLower(conjs[2]),
		Slot:     "Vr",
		Encodes:  []string{rr.SlotIV.Function.String(), rr.SlotIV.Specification.String(), rr.SlotIV.Context.String()},
		Defaults: rr.SlotIV == g.DefaultSlotIV,
	})

	// Ca.
	segs = append(segs, Segment{
		Raw:      strings.ToLower(conjs[3]),
		Slot:     "Ca",
		Encodes:  slotVICodes(f.SlotVI),
		Defaults: f.SlotVI == g.DefaultSlotVI,
	})

	postCa(&segs, conjs, 4, f, lex)
	decorateHyphens(segs)
	return segs
}

// postCa appends Slot VII affixes, optional Slot VIII (VnCn), and
// Slot IX (Vc/Vk) starting from index i in conjs.
func postCa(segs *[]Segment, conjs []string, i int, f g.Formative, lex *lexicon.Lexicon) {
	for ax, a := range f.SlotVII {
		if i+1 >= len(conjs) {
			break
		}
		_, deg := parse.ClassifyAffixVowel(conjs[i])
		abbrev := affixAbbrev(a.Consonant, lex)
		*segs = append(*segs, Segment{
			Raw:     strings.ToLower(conjs[i]),
			Slot:    fmt.Sprintf("Vx%s", subscript(ax+1)),
			Encodes: []string{fmt.Sprintf("DEG%d", deg)},
			Ordinal: ax + 1,
		})
		i++
		*segs = append(*segs, Segment{
			Raw:     strings.ToLower(conjs[i]),
			Slot:    fmt.Sprintf("Cs%s", subscript(ax+1)),
			Encodes: []string{abbrev},
			Ordinal: ax + 1,
			Cluster: a.Consonant,
			Degree:  deg,
		})
		i++
	}
	if f.SlotVIII != nil {
		if i < len(conjs) && parse.IsVowelConjunct(conjs[i]) {
			*segs = append(*segs, Segment{
				Raw:     strings.ToLower(conjs[i]),
				Slot:    "Vn",
				Encodes: []string{slotVIIIVnCode(f.SlotVIII)},
			})
			i++
		}
		if i < len(conjs) && parse.IsConsonantConjunct(conjs[i]) {
			*segs = append(*segs, Segment{
				Raw:     strings.ToLower(conjs[i]),
				Slot:    "Cn",
				Encodes: []string{slotVIIICnCode(f.SlotVIII, f.Final)},
			})
			i++
		}
	}
	slot, codes, isDefault := slotIXLabelCodes(f.Final)
	if i < len(conjs) && parse.IsVowelConjunct(conjs[i]) {
		*segs = append(*segs, Segment{
			Raw:      strings.ToLower(conjs[i]),
			Slot:     slot,
			Encodes:  codes,
			Defaults: isDefault,
		})
		i++
	} else {
		*segs = append(*segs, Segment{
			Raw:      ElidedMark,
			Slot:     slot,
			Encodes:  codes,
			Defaults: isDefault,
			Elided:   true,
		})
	}
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

// slotVIIICnCode renders the Slot VIII Cn as either a Mood label
// (for verbal formatives) or a CaseScope label (for nominal/framed
// formatives). The underlying MoodScope field carries the Mood-typed
// value either way.
func slotVIIICnCode(s g.SlotVIII, fin g.Final) string {
	var m g.Mood
	switch v := s.(type) {
	case g.VnCnValence:
		m = v.MoodScope
	case g.VnCnPhase:
		m = v.MoodScope
	case g.VnCnEffect:
		m = v.MoodScope
	case g.VnCnLevel:
		m = v.MoodScope
	case g.VnCnAspect:
		m = v.MoodScope
	default:
		return ""
	}
	if _, verbal := fin.(g.UnframedVerbal); verbal {
		return m.String()
	}
	return moodToCaseScope(m).String()
}

func moodToCaseScope(m g.Mood) g.CaseScope {
	return [...]g.CaseScope{g.CCN, g.CCA, g.CCS, g.CCQ, g.CCP, g.CCV}[m]
}

// slotIXLabelCodes returns the slot label ("Vc" or "Vk"), the codes
// it encodes, and whether they are at the grammatical default.
func slotIXLabelCodes(fin g.Final) (slot string, codes []string, isDefault bool) {
	switch v := fin.(type) {
	case g.UnframedNominal:
		return "Vc", []string{v.Case.String()}, v.Case == g.THM
	case g.FramedVerbal:
		return "Vc", []string{v.Case.String()}, v.Case == g.THM
	case g.UnframedVerbal:
		if asr, ok := v.Vk.(g.Assertive); ok {
			if asr.Validation == g.OBS {
				return "Vk", []string{"ASR"}, false
			}
			return "Vk", []string{"ASR", asr.Validation.String()}, false
		}
		return "Vk", []string{v.Vk.Tag()}, false
	}
	return "Vc", nil, true
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
	skipCode := map[string]bool{}
	if cr, ok := f.Root.(g.CrRoot); ok {
		skipCode[cr.Stem.String()] = true
		skipCode[cr.SlotIV.Specification.String()] = true
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
