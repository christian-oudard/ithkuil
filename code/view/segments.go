package view

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/christian-oudard/ithkuil/allomorph"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/roman"
	"github.com/christian-oudard/ithkuil/search"
	"github.com/christian-oudard/ithkuil/semantics"
	"github.com/christian-oudard/ithkuil/slots"
)

// ElidedMark is the symbol shown for an elided slot.
const ElidedMark = "∅"

// Segment is one chunk of a romanized formative paired with the slot it
// occupies and the grammatical codes it encodes.
type Segment struct {
	Chunk    string   // hyphen-decorated written chunk
	Raw      string   // bare written chunk (no hyphens)
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
		code := fmt.Sprintf("(Ref)%q / %s", parse.RefCluster(r.Refs), r.SlotIV.Specification)
		return RootHead{Code: code, Meaning: "referential root"}
	}
	return RootHead{}
}

// Segments walks the written conjuncts in lock-step with the parsed
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
	l, err := slots.Parse(word)
	if err != nil {
		return nil
	}
	return segmentsFromLayout(l, f, lex)
}

// segmentsFromLayout walks the slot fields of a Layout and emits one
// Segment per written chunk. Layout already knows which conjunct is
// which slot, so this function doesn't have to re-derive shape.
func segmentsFromLayout(l slots.Layout, f g.Formative, lex *lexicon.Lexicon) []Segment {
	var segs []Segment

	if l.Cc != "" {
		segs = append(segs, Segment{
			Raw:     strings.ToLower(l.Cc),
			Slot:    "Cc",
			Encodes: ccCodes(l.Cc),
		})
	}

	segs = append(segs, vvSegment(l, f))
	segs = append(segs, rootSegment(l, f))
	if vr, ok := vrSegment(l, f); ok {
		segs = append(segs, vr)
	}

	for ax, a := range l.SlotV {
		segs = append(segs, slotVCsSegment(a, ax, f, lex))
		segs = append(segs, slotVVxSegment(a, ax, f))
	}

	if l.Ca != "" {
		segs = append(segs, caSegment(l, f))
	}

	for ax, a := range l.SlotVII {
		segs = append(segs, slotVIIVxSegment(a, ax, f))
		segs = append(segs, slotVIICsSegment(a, ax, f, lex))
	}

	if l.Cn != "" && f.SlotVIII != nil {
		segs = append(segs,
			Segment{Raw: strings.ToLower(l.Vn), Slot: "Vn", Encodes: []string{g.SlotVIIIVnLabel(f.SlotVIII)}},
			Segment{Raw: strings.ToLower(l.Cn), Slot: "Cn", Encodes: []string{slotVIIICnCode(f.SlotVIII, f.Final)}},
		)
	} else if l.Cn != "" {
		// VnCn that didn't decode as Slot VIII — folded into Slot VII
		// by ToGrammar. The trailing affix already appears in the
		// SlotVII loop above when present; nothing extra to do here.
	}

	slot, codes, isDefault := slotIXLabelCodes(f.Final)
	if l.Vc != "" {
		segs = append(segs, Segment{
			Raw:      strings.ToLower(l.Vc),
			Slot:     slot,
			Encodes:  codes,
			Defaults: isDefault,
		})
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

// ccCodes describes what a Cc consonant encodes — concat status,
// shortcut variant, or both.
func ccCodes(cc string) []string {
	r := parse.ParseCc(cc)
	var codes []string
	switch r.Concat {
	case g.Type1:
		codes = append(codes, "Type1 concat")
	case g.Type2:
		codes = append(codes, "Type2 concat")
	}
	if r.Shortcut != parse.ShortcutNone {
		codes = append(codes, "Slot I shortcut")
	}
	if len(codes) == 0 {
		codes = []string{cc}
	}
	return codes
}

// vvSegment emits the Slot II segment. For CrFormatives the Vv carries
// (Stem, Version); for Cs- and Ref-roots it's the special-Vv marker
// that selects the root kind and carries Version (and Function for Cs).
func vvSegment(l slots.Layout, f g.Formative) Segment {
	switch r := f.Root.(type) {
	case g.CrRoot:
		stemVer := []string{r.Stem.String(), r.Version.String()}
		isDefault := r.Stem == g.S1 && r.Version == g.PRC
		if l.Vv == "" {
			return Segment{Raw: ElidedMark, Slot: "Vv", Encodes: stemVer, Defaults: isDefault, Elided: true}
		}
		return Segment{Raw: strings.ToLower(displayVv(l)), Slot: "Vv", Encodes: stemVer, Defaults: isDefault}
	case g.CsRoot:
		return Segment{Raw: strings.ToLower(l.Vv), Slot: "Vv", Encodes: []string{r.Version.String(), r.Function.String()}}
	case g.RefRoot:
		return Segment{Raw: strings.ToLower(l.Vv), Slot: "Vv", Encodes: []string{r.Version.String()}}
	}
	return Segment{}
}

// displayVv returns the Vv as it appears on the romanization, re-inserting
// the §3.5.1 glottal-stop when Slot V has 2+ affixes.
func displayVv(l slots.Layout) string {
	if len(l.SlotV) < 2 || l.Vv == "" {
		return l.Vv
	}
	rs := []rune(l.Vv)
	if len(rs) == 1 {
		return string(rs[0]) + "'" + string(rs[0])
	}
	if len(rs) == 2 {
		return string(rs[0]) + "'" + string(rs[1])
	}
	return l.Vv + "'"
}

// rootSegment emits the Slot III segment — Cr for CrFormative, Cs for
// CsRootFormative, C1 for RefRootFormative.
func rootSegment(l slots.Layout, f g.Formative) Segment {
	cr := strings.ToLower(l.Cr)
	switch r := f.Root.(type) {
	case g.CrRoot:
		return Segment{Raw: cr, Slot: "Cr", Encodes: []string{fmt.Sprintf("Root %q", cr)}}
	case g.CsRoot:
		return Segment{Raw: cr, Slot: "Cs (root)", Encodes: []string{fmt.Sprintf("(Cs)%q at degree %d", cr, r.Degree)}}
	case g.RefRoot:
		return Segment{Raw: cr, Slot: "C1 (ref)", Encodes: []string{fmt.Sprintf("Ref %q", cr)}}
	}
	return Segment{}
}

// vrSegment emits the Slot IV segment when Vr is present. For shortcut
// CrFormatives Vr is elided (Ca is encoded in Cc+Vv), so no segment is
// emitted there.
func vrSegment(l slots.Layout, f g.Formative) (Segment, bool) {
	if l.Vr == "" {
		return Segment{}, false
	}
	raw := strings.ToLower(l.Vr)
	switch r := f.Root.(type) {
	case g.CrRoot:
		return Segment{
			Raw:      raw,
			Slot:     "Vr",
			Encodes:  []string{r.SlotIV.Function.String(), r.SlotIV.Specification.String(), r.SlotIV.Context.String()},
			Defaults: r.SlotIV == g.DefaultSlotIV,
		}, true
	case g.CsRoot:
		return Segment{
			Raw:     raw,
			Slot:    "Vr",
			Encodes: []string{fmt.Sprintf("DEG%d", r.Degree), r.Context.String()},
		}, true
	case g.RefRoot:
		return Segment{
			Raw:      raw,
			Slot:     "Vr",
			Encodes:  []string{r.SlotIV.Function.String(), r.SlotIV.Specification.String(), r.SlotIV.Context.String()},
			Defaults: r.SlotIV == g.DefaultSlotIV,
		}, true
	}
	return Segment{}, false
}

// caSegment emits the Slot VI segment. When Slot V has any affixes the
// Ca is geminated on the romanization, so we re-apply gemination for display.
func caSegment(l slots.Layout, f g.Formative) Segment {
	raw := l.Ca
	if len(l.SlotV) > 0 {
		raw = allomorph.GeminateCa(raw)
	}
	return Segment{
		Raw:      strings.ToLower(raw),
		Slot:     "Ca",
		Encodes:  slotVICodes(f.SlotVI),
		Defaults: f.SlotVI == g.DefaultSlotVI,
	}
}

func slotVCsSegment(a slots.AffixChunk, idx int, f g.Formative, lex *lexicon.Lexicon) Segment {
	_, deg := parse.ClassifyAffixVowel(a.Vx)
	abbrev := ""
	if idx < len(f.SlotV) {
		abbrev = affixAbbrev(f.SlotV[idx].Consonant, lex)
	}
	return Segment{
		Raw:     strings.ToLower(a.Cs),
		Slot:    fmt.Sprintf("Cs₅%s", subscript(idx+1)),
		Encodes: []string{abbrev},
		Ordinal: idx + 1,
		Cluster: a.Cs,
		Degree:  deg,
	}
}

func slotVVxSegment(a slots.AffixChunk, idx int, _ g.Formative) Segment {
	_, deg := parse.ClassifyAffixVowel(a.Vx)
	return Segment{
		Raw:     strings.ToLower(a.Vx),
		Slot:    fmt.Sprintf("Vx₅%s", subscript(idx+1)),
		Encodes: []string{fmt.Sprintf("DEG%d", deg)},
		Ordinal: idx + 1,
	}
}

func slotVIIVxSegment(a slots.AffixChunk, idx int, _ g.Formative) Segment {
	_, deg := parse.ClassifyAffixVowel(a.Vx)
	return Segment{
		Raw:     strings.ToLower(a.Vx),
		Slot:    fmt.Sprintf("Vx%s", subscript(idx+1)),
		Encodes: []string{fmt.Sprintf("DEG%d", deg)},
		Ordinal: idx + 1,
	}
}

func slotVIICsSegment(a slots.AffixChunk, idx int, f g.Formative, lex *lexicon.Lexicon) Segment {
	_, deg := parse.ClassifyAffixVowel(a.Vx)
	abbrev := ""
	if idx < len(f.SlotVII) {
		abbrev = affixAbbrev(f.SlotVII[idx].Consonant, lex)
	}
	return Segment{
		Raw:     strings.ToLower(a.Cs),
		Slot:    fmt.Sprintf("Cs%s", subscript(idx+1)),
		Encodes: []string{abbrev},
		Ordinal: idx + 1,
		Cluster: a.Cs,
		Degree:  deg,
	}
}

// SegmentsModular returns one segment per phonetic chunk of a
// modular adjunct: an optional w/y prefix, zero-to-three (Vn, Cn)
// pairs, and an optional final vowel. The label "Vn₁/Cn₁/Vn₂/…"
// makes each pair's slot inside the adjunct visible.
//
// marksMood disambiguates the Cn romanization (which is shared between
// vnCnForms re-encodes a typed SlotVIII as the (Vn, Cn) pair the
// romanization uses. Inverse of parse.ParseVnCn.
func vnCnForms(s g.SlotVIII) (string, string) {
	return slots.VnCnFromSlotVIII(s)
}

// reachVH returns the canonical romanization V_H vowel for a non-None reach
// scope. "i" represents the formative reach by convention (could also
// be "u").
func reachVH(r g.ModularReach) string {
	switch r {
	case g.ModularReachCaseMoodIll:
		return "a"
	case g.ModularReachCaseMood:
		return "e"
	case g.ModularReachFormative:
		return "i"
	case g.ModularReachAdjacent:
		return "o"
	}
	return ""
}

// Mood and Case-Scope). When nil, no adjacent formative was found and
// Cn defaults to Mood — matching the spec's verbal-formative reading.
func SegmentsModular(word string, ma g.ModularAdjunct, marksMood *bool) []Segment {
	asMood := marksMood == nil || *marksMood
	// The segments are derived from the grammar, but the word decides
	// which of them were written: §4.3's bare Slot 4 form drops the C_N
	// of a final default entry, so "a" is a whole RTR adjunct with no
	// -w- in it. Walking the word alongside marks what is not there as
	// elided, the way the formative path does, rather than printing
	// letters the reader cannot find.
	bare, _ := phonology.Strip(strings.ToLower(word))
	written := func(raw string) bool {
		if raw == "" {
			return false
		}
		i := strings.Index(bare, raw)
		if i < 0 {
			return false
		}
		bare = bare[i+len(raw):]
		return true
	}
	var segs []Segment
	if raw := roman.ModularScopePrefix(ma.Scope); raw != "" {
		segs = append(segs, Segment{
			Raw:     raw,
			Slot:    "scope",
			Encodes: []string{semantics.PrefixCode(raw)},
		})
	}
	// The split comes from the arm that wrote the word. §4.3's slots are
	// not alike — Slot 2 writes a full C_N, Slot 3 spends its consonant
	// on C_M alone, Slot 4 has none — so asking slots.VnCnFromSlotVIII
	// for a (V_N, C_N) pair per entry, as this once did, invented a C_N
	// for Slot 3, found it absent, and called the slot elided while the
	// n that was there got no segment at all.
	parts, err := roman.ModularParts(ma)
	if err != nil {
		return segs
	}
	for i, p := range parts {
		idx := subscript(i + 1)
		written(strings.ToLower(p.Vn))
		vnSeg := Segment{
			Raw:  strings.ToLower(p.Vn),
			Slot: fmt.Sprintf("Vn%s", idx),
		}
		switch {
		case p.Value == nil:
			vnSeg.Slot = "Vh"
			vnSeg.Encodes = []string{ma.Reach.String()}
		case p.Slot == 4:
			// §4.3's Slot 4 is a bare vowel: it has no consonant
			// position at all, so there is no C_N here to write or to
			// elide. Its Mood/Case-Scope is the default, and that is
			// said by the shape of the slot rather than by a letter, so
			// it belongs to this vowel and not to a row of its own.
			// Naming a Cn here printed a slot §4.3 does not have.
			vnSeg.Encodes = []string{
				semantics.VnCategory(p.Vn, p.Cn),
				semantics.MoodOrCaseScope(g.SlotVIIIMoodScope(p.Value), asMood),
			}
		default:
			vnSeg.Encodes = []string{semantics.VnCategory(p.Vn, p.Cn)}
		}
		segs = append(segs, vnSeg)
		if p.Cn == "" {
			continue
		}
		seg := Segment{Raw: strings.ToLower(p.Cn)}
		if p.Slot == 3 {
			// §4.3 gives Slot 3 only C_M, "n if V_N represents an
			// Aspect, otherwise ň". It separates the slot from the one
			// before it and names the category; it is not a C_N and
			// carries no Mood/Case-Scope.
			seg.Slot = fmt.Sprintf("Cm%s", idx)
			seg.Encodes = []string{semantics.CmLabel(p.Cn)}
		} else {
			seg.Slot = fmt.Sprintf("Cn%s", idx)
			seg.Encodes = []string{semantics.CnLabel(p.Cn, asMood)}
		}
		if !written(seg.Raw) {
			seg.Raw, seg.Elided = ElidedMark, true
		}
		segs = append(segs, seg)
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
			switch {
			case s.Slot == "scope":
				cat = "scope"
				name = ""
				meaning = semantics.PrefixMeaning(s.Raw)
			case s.Slot == "Vh":
				cat = "scope"
				name = ""
				meaning = semantics.VhMeaning(s.Raw)
			// Keyed on the code rather than the slot label: the label
			// says where the marker sits and §4.3 puts C_M only in
			// Slot 3, but the code is what says it is one.
			case code == "CmAspect" || code == "CmOther":
				cat = "marker"
				name = semantics.CmName(code)
				meaning = semantics.CmMeaning(code)
			}
			out = append(out, GlossaryEntry{
				Category: cat, Code: code, Name: name, Meaning: meaning,
			})
		}
	}
	return out
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

// LayoutSegments renders a slots.Layout as phonetic-table rows, in
// written order, skipping empty slots. It carries no Encodes, because
// a Layout is the shape split alone — it is what Parse recovered
// before ToGrammar assigned any grammatical value, and is the only
// view available for a word that fails to decode.
//
// The split describes the whole word, not a prefix reached before
// giving up. parseFromCa scans ahead for a geminated Ca to decide
// whether the conjunct after Vr is the Ca or a Slot V affix, so a
// different ending re-splits everything before it.
func LayoutSegments(l slots.Layout) []Segment {
	var segs []Segment
	add := func(chunk, slot string) {
		if chunk != "" {
			segs = append(segs, Segment{Chunk: chunk, Raw: chunk, Slot: slot})
		}
	}
	add(l.Cc, "Cc")
	add(l.Vv, "Vv")
	add(l.Cr, "Cr")
	add(l.Vr, "Vr")
	for i, a := range l.SlotV {
		add(a.Cs, fmt.Sprintf("Cs₅%s", subscript(i+1)))
		add(a.Vx, fmt.Sprintf("Vx₅%s", subscript(i+1)))
	}
	add(l.Ca, "Ca")
	for i, a := range l.SlotVII {
		add(a.Vx, fmt.Sprintf("Vx%s", subscript(i+1)))
		add(a.Cs, fmt.Sprintf("Cs%s", subscript(i+1)))
	}
	add(l.Vn, "Vn")
	add(l.Cn, "Cn")
	add(l.Vc, "Vc")
	decorateHyphens(segs)
	return segs
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

// slotVIIICnCode renders the Slot VIII Cn as either a Mood label
// (for verbal formatives) or a CaseScope label (for nominal/framed
// formatives). Thin wrapper around semantics.SlotVIIICnLabel.
func slotVIIICnCode(s g.SlotVIII, fin g.Final) string {
	return semantics.SlotVIIICnLabel(s, fin)
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

	// The segments carry what each written chunk encodes, and that is
	// not everything the formative says. A §3.9.2 accessor is the plain
	// case: "mlaläswa" glosses ml-ACC/INS, but the Cs chunk carries the
	// raw cluster "sw", so the glossary listed a nameless affix and
	// explained neither the accessor nor the case it names. The codes
	// are in the gloss, which is derived from the grammar rather than
	// from the spelling, so anything the segments could not reach is
	// picked up here.
	for _, e := range GlossaryFromGloss((&gloss.Glosser{Lex: lex}).Formative(f), lex) {
		if skipCode[e.Code] {
			continue
		}
		add(e.Category, e.Code, e.Name, e.Meaning)
	}
	return out
}

// categoryForCode returns the human-readable category name for a
// grammar code ("STA" → "function"), falling back to a slot-derived
// label when no precise category is known.
func categoryForCode(code, slot string) string {
	hits := search.LookupGrammar(code)
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

// GlossaryFromGloss expands every code in a gloss line, whatever word
// class wrote it.
//
// The detailed view exists to say what the codes mean, and for most of
// the word classes it did not: a formative had Glossary and a modular
// adjunct had GlossaryModular, and an affixual adjunct, a carrier, a
// referential, a bias or a register marker printed their gloss and the
// name of their class and stopped. The reader was left to look VMC up
// by hand, which is the one thing this view is for.
//
// A glossary function per class is the obvious repair and the wrong
// shape. The codes come from three places — an affix abbreviation from
// the lexicon, a scope or carrier type from the grammar table, a
// referent from the grammar package — and per-class code would
// rediscover that split once per class, including for classes nobody
// has written yet.
//
// Instead the gloss is the input. gloss.Tokens knows which pieces are
// codes, because it is the arm that wrote them, and a code resolves
// through the same two lookups for every class. Referents are included
// even though they are not uppercase-initial: "1m" is as much a code as
// "THM" and a reader has the same question about it.
func GlossaryFromGloss(gl string, lex *lexicon.Lexicon) []GlossaryEntry {
	var out []GlossaryEntry
	seen := map[string]bool{}
	tokens := gloss.Tokens(gl)
	for i, t := range tokens {
		// A lowercase atom is a cluster, not a code, and must not be
		// looked up as one: search.LookupGrammar upper-cases its query,
		// so the root "m" would come back as Perspective M and a
		// formative would be told its root means "Monadic". The §4.6
		// referents are the exception — "1m" and "ma" are codes that
		// happen not to start with a capital.
		if t.Kind != gloss.KindCode && !isReferentCode(t.Text) {
			continue
		}
		// ACC is two things: the Accidental bias and the §3.9.2
		// case-accessor family. Nothing in the abbreviation separates
		// them, so the shape does, exactly as it does when the gloss is
		// read: a bias is a whole word, an accessor binds a case after
		// it. Looked up flat, "ml-ACC/INS" told the reader its accessor
		// meant "as luck would have it".
		if i+1 < len(tokens) && tokens[i+1].Text == "/" {
			if name, meaning, ok := g.AccessorFamily(t.Text); ok {
				if !seen[t.Text] {
					seen[t.Text] = true
					out = append(out, GlossaryEntry{
						Category: "accessor", Code: t.Text, Name: name, Meaning: meaning,
					})
				}
				continue
			}
		}
		e, ok := lookupCode(t.Text, degreeAfter(tokens, i), lex)
		if !ok || seen[e.Code] {
			continue
		}
		seen[e.Code] = true
		out = append(out, e)
	}
	return out
}

// lookupCode resolves one gloss atom, or reports that it names nothing.
// The grammar table is asked first: it is the smaller, closed space,
// and an affix abbreviation that collided with a grammatical one would
// be a defect in the lexicon rather than an ambiguity to resolve here.
// degreeAfter reads the "/9" that binds a degree to the affix before
// it, and reports 0 when what follows is not one. A slash also binds a
// case to an accessor, so the digits are what decide.
func degreeAfter(tokens []gloss.Token, i int) int {
	if i+2 >= len(tokens) || tokens[i+1].Text != "/" ||
		tokens[i+2].Kind != gloss.KindDegree {
		return 0
	}
	n, err := strconv.Atoi(tokens[i+2].Text)
	if err != nil {
		return 0
	}
	return n
}

func lookupCode(code string, degree int, lex *lexicon.Lexicon) (GlossaryEntry, bool) {
	// A degree binds to an affix and to nothing else, so a code
	// carrying one is an affix whatever else shares its spelling. Three
	// abbreviations do: ACC is a Bias and our accessor family, ANT is
	// an affix and our name for the framed relation, and CNT is both an
	// Aspect and an affix — that last pair comes from the sources and
	// cannot be renamed away. Asked in the other order, "ml-CNT/1" was
	// given a Continuative Aspect it does not have, beside the Degree
	// of Centrality it does.
	if degree >= 1 {
		if e, ok := affixEntry(code, degree, lex); ok {
			return e, true
		}
	}
	if hits := search.LookupGrammar(code); len(hits) > 0 {
		return GlossaryEntry{
			Category: categoryForCode(code, ""),
			Code:     code,
			Name:     g.Name(code),
			Meaning:  g.Meaning(code),
		}, true
	}
	// Notation the gloss writes that is not a grammatical value, so
	// search.Table does not hold it: "{Ca}" for the Slot V/VII
	// boundary, "T1" for a concatenation type, "ULT" for the relation
	// stress carries, "VIIDom" for an affix scope. These are named in
	// the grammar package and nowhere else, and a reader meets them in
	// a gloss exactly as they meet a case.
	if name := g.Name(code); name != "" {
		return GlossaryEntry{
			Category: "notation",
			Code:     code,
			Name:     name,
			Meaning:  g.Meaning(code),
		}, true
	}
	return affixEntry(code, degree, lex)
}

// affixEntry resolves a code against the affix lexicon.
func affixEntry(code string, degree int, lex *lexicon.Lexicon) (GlossaryEntry, bool) {
	if lex == nil {
		return GlossaryEntry{}, false
	}
	for _, a := range lex.Affixes {
		if a.Abbrev != code {
			continue
		}
		// The degree is what an affix actually says, and a formative's
		// breakdown already prints it: SYS/5 reads "a feedback-driven
		// system based on X", not "-ţř-". An adjunct carries exactly
		// one affix and its degree is right there in the gloss, so
		// there is no reason for it to read differently.
		if degree >= 1 && degree <= len(a.Degrees) && a.Degrees[degree-1] != "" {
			return GlossaryEntry{
				Category: "affix",
				Code:     code + "/" + strconv.Itoa(degree),
				Name:     a.Description,
				Meaning:  a.Degrees[degree-1],
			}, true
		}
		return GlossaryEntry{
			Category: "affix",
			Code:     code,
			Name:     a.Description,
			Meaning:  "-" + a.Cs + "-",
		}, true
	}
	return GlossaryEntry{}, false
}

// isReferentCode reports whether an atom is a §4.6 referent or referent
// Effect. They are written lowercase ("1m", "ma", "pa") or with one
// capital ("Mx", "Rdp"), so shape alone cannot tell them from a root
// cluster; the closed list can.
func isReferentCode(s string) bool {
	for _, r := range g.AllReferents {
		if r.String() == s {
			return true
		}
	}
	for _, e := range g.AllRefEffects {
		if e.String() == s {
			return true
		}
	}
	return false
}
