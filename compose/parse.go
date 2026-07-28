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

// Formative builds a grammar.Formative from a gloss-style authoring
// expression. The syntax is a strict subset of the gloss output: slots
// are separated by "-", sub-fields within a slot by "/". The root
// cluster is written in plain Ithkuil orthography or ASCII digraphs
// (aa→ä, t,→ţ, sq→š, cq→č, dz→ẓ). Every other token is either a
// grammatical abbreviation (Stem, Version, Function, Specification,
// Context, Case, Aspect, Valence, Mood, Illocution, Stress) or an
// affix written "Cs/degree" or "ABBREV/degree".
//
// Affixes land in Slot VII unless they precede the Ca complex, which
// is written either as its components ("MSS.G") or, when every
// component is at its default, as the "{Ca}" marker. Slot V affixes
// apply to the stem alone; Slot VII affixes have scope over the Ca.
//
// Examples
//
//	ml                              minimal nominal formative on root "ml"
//	S2/CPT-ml-ERG                   stem 2, completive, ergative case
//	S2/CPT-ml-DYN/OBJ-DEV/3-ERG     plus dynamic+objective and a DEV/3 affix
//	m-ţř/5_2-{Ca}-t/1_2             SYS/5 in Slot V, DCD/1 in Slot VII
//	t,k-FNC                         ASCII digraph root "t,k" → "ţk"
//
// Lexicon-aware affix resolution requires passing an AffixMap; pass
// nil to accept only the bare Cs form.
//
// The MVP does not yet handle CsRoot/RefRoot, Ca complex spelled out
// in the slot, or concatenation. Those return an error so callers can
// detect when the input is beyond current coverage.
func Formative(s string, affixes map[string]lexicon.AffixEntry) (g.Formative, error) {
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

	// Identify the root. A bare Cr cluster (lowercase letters /
	// Ithkuil chars) beats a parenthesised form when both are
	// present: in "S3-tpl-(1m/BEN)/3" the `tpl` is the root and
	// `(1m/BEN)/3` is a Type-3 referential affix in Slot VII.
	rootIdx := -1
	var root g.Root
	for i, tok := range tokens {
		cluster, ok := isClusterToken(tok)
		if !ok {
			continue
		}
		if rootIdx >= 0 {
			return g.Formative{}, fmt.Errorf("multiple root candidates: %q and %q",
				tokens[rootIdx], tok)
		}
		rootIdx = i
		root = g.DefaultCrRoot(cluster)
	}
	// No bare cluster — try the parenthesised forms.
	if rootIdx < 0 {
		for i, tok := range tokens {
			if !strings.HasPrefix(tok, "(") {
				continue
			}
			r, ok, err := parseParensRoot(tok, affixes)
			if err != nil {
				return g.Formative{}, fmt.Errorf("root token %q: %w", tok, err)
			}
			if !ok {
				continue
			}
			if rootIdx >= 0 {
				return g.Formative{}, fmt.Errorf("multiple root candidates: %q and %q",
					tokens[rootIdx], tok)
			}
			rootIdx = i
			root = r
		}
	}
	if rootIdx < 0 {
		return g.Formative{}, fmt.Errorf("no root in %q", s)
	}

	f := g.Formative{
		Root:   root,
		SlotVI: g.DefaultSlotVI,
		Final:  g.UnframedNominal{Case: g.THM},
	}
	caIdx := caTokenIndex(tokens, rootIdx)
	for i, tok := range tokens {
		if i == rootIdx || tok == caMarker {
			continue
		}
		if err := applyToken(&f, tok, affixes, i < caIdx); err != nil {
			return g.Formative{}, fmt.Errorf("token %q: %w", tok, err)
		}
	}
	return f, nil
}

// caMarker is the placeholder the glosser emits for an all-default Ca
// complex that still needs to be shown, because Slot V is filled and
// the Ca is what separates Slot V affixes from Slot VII affixes.
const caMarker = "{Ca}"

// caTokenIndex returns the position of the Slot VI Ca token — either
// the explicit caMarker or a spelled-out Ca complex like "MSS.G".
// Affixes before it belong to Slot V, affixes after it to Slot VII.
// Returns 0 when there is no Ca token, putting every affix in Slot VII.
// rootIdx is skipped so a root cluster is never read as a Ca.
func caTokenIndex(tokens []string, rootIdx int) int {
	for i, tok := range tokens {
		if i == rootIdx {
			continue
		}
		if tok == caMarker || isCaToken(tok) {
			return i
		}
	}
	return 0
}

// isCaToken reports whether every dot-separated part of tok names a
// value from one of the five Ca categories. The comparison is
// case-sensitive: Ca abbreviations are uppercase, root clusters are
// lowercase, and "m" the root must not read as "M" the Monadic
// perspective.
func isCaToken(tok string) bool {
	if tok == "" || strings.ContainsAny(tok, "/:()") {
		return false
	}
	for _, part := range strings.Split(tok, ".") {
		if !caAbbrevs[part] {
			return false
		}
	}
	return true
}

// caAbbrevs holds every abbreviation belonging to a Ca-complex
// category. The five categories are disjoint from the rest of the
// grammar inventory, so membership alone identifies a Ca token.
var caAbbrevs = func() map[string]bool {
	out := map[string]bool{}
	for _, c := range g.AllConfigurations {
		out[c.String()] = true
	}
	for _, a := range g.AllAffiliations {
		out[a.String()] = true
	}
	for _, p := range g.AllPerspectives {
		out[p.String()] = true
	}
	for _, e := range g.AllExtensions {
		out[e.String()] = true
	}
	for _, e := range g.AllEssences {
		out[e.String()] = true
	}
	return out
}()

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
// ABBREV/degree, with an optional "_2" or "_3" Type suffix (Type 1
// silent — the unmarked default). Degree can be 0 (some affixes use
// it as a default/no-degree marker).
var affixToken = regexp.MustCompile(`^([^/_]+)/([0-9])(?:_([23]))?$`)

// applyToken dispatches one inter-slot token to ApplyFlag for plain
// abbreviations, to the affix builder for "X/N" forms, or splits on
// "/" and recurses for compound slot groups like "S2/CPT". slotV
// selects which affix slot the token lands in when it is an affix.
func applyToken(f *g.Formative, tok string, affixes map[string]lexicon.AffixEntry, slotV bool) error {
	// Type-3 referential affix: "(refs)/degree" where refs is a
	// referent list like "1m" or "1m/BEN+2p/DET". The cluster is
	// the concatenation of each ref's C1 form.
	if strings.HasPrefix(tok, "(") {
		return appendType3Affix(f, tok, slotV)
	}
	// Affix form takes precedence over the generic slash-split path:
	// "DEV/3" must mean affix DEV degree 3, not flag DEV plus flag 3.
	if m := affixToken.FindStringSubmatch(tok); m != nil {
		return appendAffix(f, m[1], m[2], m[3], affixes, slotV)
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

// type3AffixToken matches "(refs)/degree" — Type-3 referential affix.
var type3AffixToken = regexp.MustCompile(`^\(([^)]+)\)/([0-9])$`)

func appendType3Affix(f *g.Formative, tok string, slotV bool) error {
	m := type3AffixToken.FindStringSubmatch(tok)
	if m == nil {
		return fmt.Errorf("not a recognized Type-3 affix")
	}
	degree, _ := strconv.Atoi(m[2])
	var c1 strings.Builder
	for _, part := range strings.Split(m[1], "+") {
		ref, eff, err := parseRefSpec(part)
		if err != nil {
			return err
		}
		c1.WriteString(referentials.RefC1(referentials.PersonalRef{Referent: ref, Effect: eff}))
	}
	appendToAffixSlot(f, g.Affix{
		Type: g.Type3Affix, Degree: degree, Consonant: c1.String(),
	}, slotV)
	return nil
}

// appendToAffixSlot puts a parsed affix in Slot V (applies to the stem
// alone) or Slot VII (has scope over the Ca complex).
func appendToAffixSlot(f *g.Formative, a g.Affix, slotV bool) {
	if slotV {
		f.SlotV = append(f.SlotV, a)
		return
	}
	f.SlotVII = append(f.SlotVII, a)
}

func appendAffix(f *g.Formative, csOrAbbrev, degreeStr, typeStr string, affixes map[string]lexicon.AffixEntry, slotV bool) error {
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
	appendToAffixSlot(f, g.Affix{Type: atype, Degree: degree, Consonant: cs}, slotV)
	return nil
}

// resolveAffixCs returns the Cs cluster for a Slot VII affix
// identifier. Accepts the cluster itself, the all-caps abbreviation
// (looked up by .Abbrev), or any unknown lowercase cluster (the
// lexicon is a named subset, not the authoritative list of legal Cs
// clusters). An unknown cluster is folded through surface.FromASCII,
// mirroring the root: the canonical gloss writes it in ASCII digraphs
// so that it stays typable, and this is what reads that back.
func resolveAffixCs(id string, affixes map[string]lexicon.AffixEntry) string {
	if affixes == nil {
		return surface.FromASCII(id)
	}
	if _, ok := affixes[id]; ok {
		return id
	}
	// Abbreviation lookup only fires for all-uppercase identifiers.
	if id == strings.ToUpper(id) {
		for cs, a := range affixes {
			if a.Abbrev == id {
				return cs
			}
		}
		return ""
	}
	// Lowercase/mixed cluster not in the lexicon — accept it, folding
	// any ASCII digraphs back to their Ithkuil glyphs.
	return surface.FromASCII(id)
}

// isClusterToken returns (cluster, true) if tok looks like an Ithkuil
// root cluster — that is, a single token (no "/" or ":"), at least
// one character is a lowercase ASCII letter or an Ithkuil special
// orthographic glyph. ASCII digraphs are folded via surface.FromASCII.
//
// All-caps tokens (with optional digits) are treated as abbreviations
// and rejected here so they flow through ApplyFlag instead.
func isClusterToken(tok string) (string, bool) {
	if tok == "" || tok == caMarker || strings.ContainsAny(tok, "/:") {
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
