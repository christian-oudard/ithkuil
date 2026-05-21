package compose

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/referentials"
	"github.com/christian-oudard/ithkuil/surface"
)

// ParseString builds a grammar.Formative from a gloss-style authoring
// expression. The syntax is a strict subset of the gloss output: slots
// are separated by "-", sub-fields within a slot by "/". The root
// cluster is written in plain Ithkuil orthography or ASCII digraphs
// (aa→ä, t,→ţ, sq→š, cq→č, dz→ẓ). Every other token is either a
// grammatical abbreviation (Stem, Version, Function, Specification,
// Context, Case, Aspect, Valence, Mood, Illocution, Stress) or a
// Slot VII affix written "Cs/degree" or "ABBREV/degree".
//
// Examples
//
//	ml                              minimal nominal formative on root "ml"
//	S2/CPT-ml-ERG                   stem 2, completive, ergative case
//	S2/CPT-ml-DYN/OBJ-DEV/3-ERG     plus dynamic+objective and a DEV/3 affix
//	t,k-FNC                         ASCII digraph root "t,k" → "ţk"
//
// Lexicon-aware affix resolution requires passing an AffixMap; pass
// nil to accept only the bare Cs form.
//
// The MVP does not yet handle CsRoot/RefRoot, Ca complex spelled out
// in the slot, or concatenation. Those return an error so callers can
// detect when the input is beyond current coverage.
func ParseString(s string, affixes map[string]lexicon.AffixEntry) (g.Formative, error) {
	s = strings.TrimSpace(s)
	if s == "" {
		return g.Formative{}, fmt.Errorf("empty input")
	}
	// Collapse run of "-" into a single separator: gloss output writes
	// "S2/CPT--ml-…" with a double hyphen around the root marker.
	tokens := splitSlots(s)
	if len(tokens) == 0 {
		return g.Formative{}, fmt.Errorf("no slot tokens")
	}

	// Identify the root: the first token that's a Cr cluster, a
	// "(CTR)/1"-style CsRoot, or a "(1m+2p)"-style RefRoot.
	rootIdx := -1
	var root g.Root
	for i, tok := range tokens {
		r, ok, err := tryParseRoot(tok, affixes)
		if err != nil {
			return g.Formative{}, fmt.Errorf("root token %q: %w", tok, err)
		}
		if !ok {
			continue
		}
		if rootIdx >= 0 {
			return g.Formative{}, fmt.Errorf("multiple root candidates: token %q", tok)
		}
		rootIdx = i
		root = r
	}
	if rootIdx < 0 {
		return g.Formative{}, fmt.Errorf("no root in %q", s)
	}

	f := g.Formative{
		Root:   root,
		SlotVI: g.DefaultSlotVI,
		Final:  g.UnframedNominal{Case: g.THM},
	}
	for i, tok := range tokens {
		if i == rootIdx {
			continue
		}
		if err := applyToken(&f, tok, affixes); err != nil {
			return g.Formative{}, fmt.Errorf("token %q: %w", tok, err)
		}
	}
	return f, nil
}

// tryParseRoot inspects a single token and returns a parsed Root if it
// looks like one. Returns (nil, false, nil) when the token is clearly
// not a root candidate. An error means the token *looked* like a root
// but couldn't be decoded — that's a real parse failure.
func tryParseRoot(tok string, affixes map[string]lexicon.AffixEntry) (g.Root, bool, error) {
	if tok == "" {
		return nil, false, nil
	}
	// Parens-wrapped: CsRoot "(ABBREV)/degree" or RefRoot "(refs)".
	if strings.HasPrefix(tok, "(") {
		return parseParensRoot(tok, affixes)
	}
	// Bare cluster: CrRoot.
	if cluster, ok := isClusterToken(tok); ok {
		return g.DefaultCrRoot(cluster), true, nil
	}
	return nil, false, nil
}

// csRootToken matches "(ABBREV)/degree" where ABBREV is the
// uppercase affix abbreviation or the Cs cluster itself.
var csRootToken = regexp.MustCompile(`^\(([^)]+)\)/([0-9])$`)

func parseParensRoot(tok string, affixes map[string]lexicon.AffixEntry) (g.Root, bool, error) {
	// CsRoot first: "(X)/digit". If the slash form doesn't match, fall
	// through to RefRoot.
	if m := csRootToken.FindStringSubmatch(tok); m != nil {
		degree, _ := strconv.Atoi(m[2])
		cs := resolveAffixCs(m[1], affixes)
		if cs == "" {
			return nil, true, fmt.Errorf("unknown Cs-root affix %q", m[1])
		}
		return g.CsRoot{Cs: cs, Degree: degree, Version: g.PRC, Function: g.STA, Context: g.EXS}, true, nil
	}
	// RefRoot: "(refs)" where refs is "1m" or "1m+2p" or
	// "1m/BEN+2p/DET" — referent abbreviations joined by "+", with
	// an optional "/EFFECT" suffix per referent.
	if !strings.HasSuffix(tok, ")") {
		return nil, false, nil
	}
	inner := tok[1 : len(tok)-1]
	if inner == "" {
		return nil, true, fmt.Errorf("empty referential")
	}
	parts := strings.Split(inner, "+")
	var c1 strings.Builder
	for _, part := range parts {
		ref, eff, err := parseRefSpec(part)
		if err != nil {
			return nil, true, err
		}
		c1.WriteString(referentials.RefC1(referentials.PersonalRef{Referent: ref, Effect: eff}))
	}
	return g.RefRoot{C1: c1.String(), Version: g.PRC, SlotIV: g.DefaultSlotIV}, true, nil
}

// parseRefSpec decodes "1m" or "1m/BEN" into a Referent + Effect.
func parseRefSpec(s string) (referentials.Referent, referentials.Effect, error) {
	refName, effName, _ := strings.Cut(s, "/")
	var ref referentials.Referent
	matched := false
	for _, r := range referentials.AllReferents {
		if r.String() == refName {
			ref = r
			matched = true
			break
		}
	}
	if !matched {
		return 0, 0, fmt.Errorf("unknown referent %q", refName)
	}
	eff := referentials.NEU
	if effName != "" {
		matched = false
		for _, e := range referentials.AllEffects {
			if e.String() == effName {
				eff = e
				matched = true
				break
			}
		}
		if !matched {
			return 0, 0, fmt.Errorf("unknown effect %q", effName)
		}
	}
	return ref, eff, nil
}

// splitSlots splits the input on "-" while skipping empty slots that
// arise from "--" sequences (used in gloss output around the root).
func splitSlots(s string) []string {
	raw := strings.Split(s, "-")
	out := raw[:0]
	for _, t := range raw {
		t = strings.TrimSpace(t)
		if t != "" {
			out = append(out, t)
		}
	}
	return out
}

// affixToken matches a Slot VII affix written as Cs/degree or
// ABBREV/degree, with an optional ":2" or ":3" type tag.
var affixToken = regexp.MustCompile(`^([^/]+)/([1-9])(?::([123]))?$`)

// applyToken dispatches one inter-slot token to ApplyFlag for plain
// abbreviations, to the affix builder for "X/N" forms, or splits on
// "/" and recurses for compound slot groups like "S2/CPT".
func applyToken(f *g.Formative, tok string, affixes map[string]lexicon.AffixEntry) error {
	// Affix form takes precedence over the generic slash-split path:
	// "DEV/3" must mean affix DEV degree 3, not flag DEV plus flag 3.
	if m := affixToken.FindStringSubmatch(tok); m != nil {
		return appendAffix(f, m[1], m[2], m[3], affixes)
	}
	if strings.ContainsAny(tok, "/.") {
		// "/" groups sub-fields like S2/CPT or DYN/OBJ; "." separates
		// Ca-complex components like MSS.G.RPV. Both flatten to a list
		// of independent flags.
		fields := strings.FieldsFunc(tok, func(r rune) bool {
			return r == '/' || r == '.'
		})
		for _, part := range fields {
			if part == "" {
				continue
			}
			if err := ApplyFlag(f, part); err != nil {
				return err
			}
		}
		return nil
	}
	return ApplyFlag(f, tok)
}

func appendAffix(f *g.Formative, csOrAbbrev, degreeStr, typeStr string, affixes map[string]lexicon.AffixEntry) error {
	degree, _ := strconv.Atoi(degreeStr)
	atype := g.Type1Affix
	switch typeStr {
	case "", "1":
		atype = g.Type1Affix
	case "2":
		atype = g.Type2Affix
	case "3":
		atype = g.Type3Affix
	}
	cs := resolveAffixCs(csOrAbbrev, affixes)
	if cs == "" {
		return fmt.Errorf("unknown affix %q", csOrAbbrev)
	}
	f.SlotVII = append(f.SlotVII, g.Affix{Type: atype, Degree: degree, Consonant: cs})
	return nil
}

// resolveAffixCs returns the Cs cluster for a Slot VII affix
// identifier. It accepts either the cluster itself (any token with a
// lowercase/special character matched directly against affixes) or
// the all-caps abbreviation (looked up in affixes by .Abbrev).
func resolveAffixCs(id string, affixes map[string]lexicon.AffixEntry) string {
	if affixes == nil {
		// Without a lexicon we can only trust the literal form.
		return id
	}
	// Direct cluster match.
	if _, ok := affixes[id]; ok {
		return id
	}
	// Abbreviation lookup.
	upper := strings.ToUpper(id)
	for cs, a := range affixes {
		if a.Abbrev == upper {
			return cs
		}
	}
	return ""
}

// isClusterToken returns (cluster, true) if tok looks like an Ithkuil
// root cluster — that is, a single token (no "/" or ":"), at least
// one character is a lowercase ASCII letter or an Ithkuil special
// orthographic glyph. ASCII digraphs are folded via surface.FromASCII.
//
// All-caps tokens (with optional digits) are treated as abbreviations
// and rejected here so they flow through ApplyFlag instead.
func isClusterToken(tok string) (string, bool) {
	if tok == "" || strings.ContainsAny(tok, "/:") {
		return "", false
	}
	// Special: a CsRoot is written "(b)" in some glossing variants;
	// not supported in this MVP.
	if strings.ContainsAny(tok, "()") {
		return "", false
	}
	hasLower := false
	for _, r := range tok {
		switch r {
		case 'ä', 'ë', 'ö', 'ü',
			'ţ', 'ḑ', 'ļ', 'ç',
			'š', 'ž', 'č', 'ň',
			'ř', 'ẓ', '\'':
			hasLower = true
		}
		if r >= 'a' && r <= 'z' {
			hasLower = true
		}
	}
	if !hasLower {
		return "", false
	}
	return surface.FromASCII(tok), true
}
