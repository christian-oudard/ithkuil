package compose

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/referentials"
	"github.com/christian-oudard/ithkuil/surface"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// ParseToken parses a single canonical-gloss token (whitespace-delimited
// unit) into a tokenize.WordToken. Inverse of gloss.Glosser.Token when
// Canonical=true.
//
// Dispatch is structural — each adjunct type has a distinctive shape
// (leading bracket, embedded colon, bare uppercase, etc.) — see the
// canonical-gloss memory for the full table.
func ParseToken(s string, lex *lexicon.Lexicon) (tokenize.WordToken, error) {
	s = strings.TrimSpace(s)
	if s == "" {
		return nil, fmt.Errorf("empty token")
	}

	// Foreign word: double-quoted text.
	if len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' {
		return tokenize.ForeignWord{Text: s[1 : len(s)-1]}, nil
	}

	// Parsing adjunct: "mono:" / "ulti:" / "penu:" / "ante:"
	if strings.HasSuffix(s, ":") {
		if pa, ok := parseParsingAdjunct(s); ok {
			return tokenize.ParsingAdjunctWord{Text: s, Adjunct: pa}, nil
		}
	}

	// Register end: "<NAME>_END"
	if strings.HasSuffix(s, "_END") {
		if r, ok := parseRegisterName(strings.TrimSuffix(s, "_END")); ok {
			return tokenize.RegisterEndWord{Text: s, Register: r}, nil
		}
	}

	// "*[TYPE]-CASE..." is the explicit carrier-headed referential
	// (§4.6.3 epenthesis disambiguator). Force the referential path
	// regardless of how many tail fields are present.
	if strings.HasPrefix(s, "*[") {
		return parseCarrierHeadedReferential(s[1:])
	}

	// Carrier: "[TYPE]" or "[TYPE]-CASE"
	if strings.HasPrefix(s, "[") {
		if w, ok, err := parseCarrierOrReferential(s); ok || err != nil {
			return w, err
		}
	}

	// Bare uppercase abbreviation: bias OR register-start.
	if isBareUppercase(s) {
		if b, ok := parseBiasName(s); ok {
			return tokenize.BiasWord{Text: s, Bias: b}, nil
		}
		if r, ok := parseRegisterName(s); ok {
			return tokenize.RegisterStartWord{Text: s, Register: r}, nil
		}
	}

	// Referential or combination-referential with bare-referent head.
	if looksLikeReferential(s) {
		return parseReferentialToken(s)
	}

	// Modular adjunct: dotted Vn.Cn shape or bare "MOD" with optional
	// scope/reach tails.
	if s == "MOD" || strings.HasPrefix(s, "MOD-{") || looksLikeModular(s) {
		return parseModularToken(s)
	}

	// Affixual adjunct: contains "/" and matches the Cs/N shape.
	if strings.Contains(s, "/") && looksLikeAffixual(s) {
		if lex == nil {
			return nil, fmt.Errorf("affixual adjunct %q: lexicon required", s)
		}
		return parseAffixualAdjunct(s, lex.Affixes)
	}

	// Default: try as a formative via the existing Formative.
	if lex != nil {
		f, err := Formative(s, lex.Affixes)
		if err == nil {
			return tokenize.FormativeWord{Text: s, Formative: f}, nil
		}
		return nil, fmt.Errorf("formative parse failed: %w", err)
	}
	return nil, fmt.Errorf("unrecognized token %q (no lexicon for formative fallback)", s)
}

// parseParsingAdjunct decodes "mono:" / "ulti:" / "penu:" / "ante:".
func parseParsingAdjunct(s string) (g.ParsingAdjunct, bool) {
	switch s {
	case "mono:":
		return g.ParsingAdjunct{Stress: surface.Monosyllabic}, true
	case "ulti:":
		return g.ParsingAdjunct{Stress: surface.Ultimate}, true
	case "penu:":
		return g.ParsingAdjunct{Stress: surface.Penultimate}, true
	case "ante:":
		return g.ParsingAdjunct{Stress: surface.Antepenultimate}, true
	}
	return g.ParsingAdjunct{}, false
}

// parseBiasName looks up the abbreviation against the Bias enum.
func parseBiasName(s string) (g.Bias, bool) {
	for _, b := range g.AllBiases {
		if b.String() == s {
			return b, true
		}
	}
	return 0, false
}

// parseRegisterName looks up the abbreviation against the Register enum.
func parseRegisterName(s string) (g.Register, bool) {
	for _, r := range g.AllRegisters {
		if r.String() == s {
			return r, true
		}
	}
	return 0, false
}

// parseCarrierOrReferential dispatches a "[...]"-leading token.
// Carriers are "[CAR|QUO|NAM|PHR]" optionally followed by "-CASE".
// A "[..]" leading a referent list (e.g. "[1m+2p]-ERG") is a multi-
// referent referential — deferred for now.
func parseCarrierOrReferential(s string) (tokenize.WordToken, bool, error) {
	close := strings.Index(s, "]")
	if close < 1 {
		return nil, false, nil
	}
	inner := s[1:close]
	tail := s[close+1:]
	// Carrier short type names: CAR/QUO/NAM/PHR.
	if ct, ok := parseCarrierTypeAbbrev(inner); ok {
		// A bare [TYPE](-CASE)? token with no further tail is a
		// CarrierWord. If extra tail fields (CASE2, RefB, RPV, Spec)
		// appear, the token must be a ReferentialWord/CombinationRefWord
		// whose head is a carrier suppletive — those carry richer
		// grammar that CarrierWord can't hold.
		caseChunk, restTail := splitFirstHyphenChunk(tail)
		if restTail == "" {
			cv, err := caseFromCanonicalTail(tail)
			if err != nil {
				return nil, true, fmt.Errorf("carrier %q: %w", s, err)
			}
			return tokenize.CarrierWord{
				Text:    s,
				Carrier: g.CarrierAdjunct{Type: ct, Case: cv},
			}, true, nil
		}
		// Extra fields present — build a Referential/CombinationRef.
		w, err := buildRefFromTail(s, &ct, nil, caseChunk, restTail)
		return w, true, err
	}
	// Otherwise: bracketed referent list, e.g. [1m+2p] or [1m/BEN+2p].
	refs, err := parseRefList(inner)
	if err != nil {
		return nil, true, fmt.Errorf("referential %q: %w", s, err)
	}
	caseChunk, restTail := splitFirstHyphenChunk(tail)
	if caseChunk == "" {
		return nil, true, fmt.Errorf("referential %q: no case after referent list", s)
	}
	w, err := buildRefFromTail(s, nil, refs, caseChunk, restTail)
	return w, true, err
}

// parseCarrierHeadedReferential decodes the "[TYPE]-CASE..." form
// stripped of its leading "*" sigil. The "*" was the explicit marker
// that we're looking at a referential (not a bare CarrierWord), so
// this path always builds a ReferentialWord/CombinationRefWord even
// when the tail has only a case slot.
func parseCarrierHeadedReferential(s string) (tokenize.WordToken, error) {
	close := strings.Index(s, "]")
	if close < 1 {
		return nil, fmt.Errorf("carrier-headed ref %q: missing closing bracket", s)
	}
	inner := s[1:close]
	tail := s[close+1:]
	ct, ok := parseCarrierTypeAbbrev(inner)
	if !ok {
		return nil, fmt.Errorf("carrier-headed ref %q: unknown carrier type %q", s, inner)
	}
	caseChunk, restTail := splitFirstHyphenChunk(tail)
	if caseChunk == "" {
		return nil, fmt.Errorf("carrier-headed ref %q: missing case", s)
	}
	return buildRefFromTail(s, &ct, nil, caseChunk, restTail)
}

// splitFirstHyphenChunk takes a "-CASE-rest..." tail and returns
// ("CASE", "rest..."). For "" returns ("", ""); for "-CASE" alone
// returns ("CASE", "").
func splitFirstHyphenChunk(tail string) (first, rest string) {
	if tail == "" || !strings.HasPrefix(tail, "-") {
		return "", ""
	}
	tail = tail[1:]
	if i := strings.Index(tail, "-"); i >= 0 {
		return tail[:i], tail[i+1:]
	}
	return tail, ""
}

// buildRefFromTail constructs a ReferentialWord or CombinationRefWord
// from the already-parsed head (carrier OR refs) and the slot tail
// after the first case. Used by both bracketed-head paths.
func buildRefFromTail(
	text string,
	carrier *g.CarrierType,
	refs []referentials.PersonalRef,
	caseChunk string,
	restTail string,
) (tokenize.WordToken, error) {
	c, ok := parseCaseName(caseChunk)
	if !ok {
		return nil, fmt.Errorf("referential %q: unknown case %q", text, caseChunk)
	}
	// Combination referential? Detect Specification name as next slot.
	parts := strings.Split(restTail, "-")
	if restTail != "" {
		if spec, ok := parseSpecName(parts[0]); ok {
			return tokenize.CombinationRefWord{
				Text:    text,
				Carrier: carrier,
				Refs:    refs,
				Case:    c,
				Spec:    spec,
			}, nil
		}
	}
	var case2 *g.Case
	var refB []referentials.PersonalRef
	rpv := false
	if restTail != "" {
		for _, p := range parts {
			switch {
			case p == "RPV":
				rpv = true
			case strings.HasPrefix(p, "[") && strings.HasSuffix(p, "]"):
				inner := p[1 : len(p)-1]
				rb, err := parseRefList(inner)
				if err != nil {
					return nil, fmt.Errorf("referential %q refB %q: %w", text, p, err)
				}
				refB = rb
			default:
				if cv, ok := parseCaseName(p); ok && case2 == nil {
					case2 = &cv
					continue
				}
				return nil, fmt.Errorf("referential %q: unexpected slot %q", text, p)
			}
		}
	}
	return tokenize.ReferentialWord{
		Text:       text,
		Carrier:    carrier,
		Refs:       refs,
		Case:       &c,
		Case2:      case2,
		RefB:       refB,
		RpvEssence: rpv,
	}, nil
}

// parseCarrierTypeAbbrev maps the 3-letter canonical form back to a
// CarrierType (inverse of carrierTypeAbbrev in gloss).
func parseCarrierTypeAbbrev(s string) (g.CarrierType, bool) {
	switch s {
	case "CAR":
		return g.Carrier, true
	case "QUO":
		return g.Quotative, true
	case "NAM":
		return g.Naming, true
	case "PHR":
		return g.Phrasal, true
	}
	return 0, false
}

// caseFromCanonicalTail decodes a "-CASE" tail into a typed Case,
// defaulting to THM when the tail is empty.
func caseFromCanonicalTail(tail string) (g.Case, error) {
	if tail == "" {
		return g.THM, nil
	}
	if !strings.HasPrefix(tail, "-") {
		return 0, fmt.Errorf("expected leading '-' before case, got %q", tail)
	}
	caseName := tail[1:]
	for _, c := range g.AllCases {
		if c.String() == caseName {
			return c, nil
		}
	}
	return 0, fmt.Errorf("unknown case %q", caseName)
}

// isBareUppercase reports whether s is composed entirely of uppercase
// ASCII letters and digits (no hyphens, slashes, brackets, etc.) — the
// shape of bias and register-start tokens.
func isBareUppercase(s string) bool {
	if s == "" {
		return false
	}
	for _, r := range s {
		if !((r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9')) {
			return false
		}
	}
	return true
}

// looksLikeReferential reports whether s starts with a single referent
// symbol followed by "-" (e.g. "1m-ERG", "ma-AFF-DAT-2m"). Multi-ref
// forms like "[1m+2p]-..." are detected by the "[" prefix instead.
func looksLikeReferential(s string) bool {
	dash := strings.Index(s, "-")
	if dash < 1 {
		return false
	}
	head := s[:dash]
	// Single-digit + lowercase letter: 1m, 2p, etc.
	if len(head) >= 2 && head[0] >= '0' && head[0] <= '9' {
		return true
	}
	// Two-letter personal-reference forms like "ma", "pu", "mi" — these
	// also occur as referential heads (§4.6.4).
	// TODO: refine this discriminator once the parser is built.
	return false
}

// looksLikeModular reports whether s has the shape of a modular
// adjunct: a Vn category abbreviation followed by "." and a Mood/
// CaseScope abbreviation (e.g. "RTR.SUB", "PRG.HYP"), optionally with
// a "-{parent}" / "-{concat}" scope suffix. The bare "MOD" case is
// handled before this check.
func looksLikeModular(s string) bool {
	// Strip the optional reach tail then the application-scope tail
	// before testing the body shape.
	body, _ := trimModularReach(s)
	body, _ = trimModularScope(body)
	if body == "MOD" {
		return true
	}
	// Vn.Cn form: a dot between two uppercase abbreviations.
	if dot := strings.Index(body, "."); dot > 0 {
		left, right := body[:dot], body[dot+1:]
		return isBareUppercase(left) && isBareUppercase(right)
	}
	// Vn-only form (Cn defaults to FAC): a bare uppercase abbreviation
	// recognised as one of the Vn categories. This shape only matters
	// when the original token had a scope/reach tail — otherwise the
	// bare-uppercase dispatch already handled it as bias/register.
	if body != s && isBareUppercase(body) {
		return isVnCategoryName(body)
	}
	return false
}

// isVnCategoryName reports whether s names any Valence/Phase/Effect/
// Level/Aspect value.
func isVnCategoryName(s string) bool {
	for _, v := range g.AllValences {
		if v.String() == s {
			return true
		}
	}
	for _, p := range g.AllPhases {
		if p.String() == s {
			return true
		}
	}
	for _, e := range g.AllEffects {
		if e.String() == s {
			return true
		}
	}
	for _, l := range g.AllLevels {
		if l.String() == s {
			return true
		}
	}
	for _, a := range g.AllAspects {
		if a.String() == s {
			return true
		}
	}
	return false
}

// looksLikeAffixual reports whether s has the shape of an affixual
// adjunct: contains "/" with a Cs prefix (lowercase letters), or an
// abbreviation followed by "/N" with optional "_2"/"_3" and "-{scope}".
func looksLikeAffixual(s string) bool {
	slash := strings.Index(s, "/")
	if slash < 1 {
		return false
	}
	// A formative slot like "S2.CPT" has no "/" at all; a slot that
	// does, like "DEV/3", sits among more hyphen-separated slots.
	// hyphens around it; an affixual adjunct is the whole token.
	// Heuristic: token has a "/" and either ends with "}" (scope) or
	// has only one hyphen-separated slot (just the affix).
	if strings.Contains(s, "{") {
		return true
	}
	parts := strings.Split(s, "-")
	if len(parts) == 1 {
		return true
	}
	// Multi-affix: all parts look like "Cs/N" or "{scope}".
	for _, p := range parts {
		if strings.Contains(p, "/") || (strings.HasPrefix(p, "{") && strings.HasSuffix(p, "}")) {
			continue
		}
		return false
	}
	return true
}

// ParseSentence splits a canonical-gloss sentence on whitespace and
// dispatches each token via ParseToken. Returns the slice of tokens
// in order; errors include the failing token's index and content.
func ParseSentence(s string, lex *lexicon.Lexicon) ([]tokenize.WordToken, error) {
	fields := strings.Fields(s)
	out := make([]tokenize.WordToken, 0, len(fields))
	for i, f := range fields {
		tok, err := ParseToken(f, lex)
		if err != nil {
			return nil, fmt.Errorf("token %d (%q): %w", i, f, err)
		}
		out = append(out, tok)
	}
	return out, nil
}

// parseAffixualAdjunct decodes the canonical form of an affixual
// adjunct — single or multi — into a tokenize.WordToken.
//
// Single: "Cs/N" optionally followed by "-{scope}".
// Multi:  "Cs1/N1[-{s1}]-Cs2/N2-Cs3/N3...[-{sN}]".
//
// Each affix segment is "Cs/N" with an optional "_2"/"_3" Type suffix.
// Scope segments are wrapped in "{}". Default-VDom scopes are absent.
func parseAffixualAdjunct(s string, affixes map[string]lexicon.AffixEntry) (tokenize.WordToken, error) {
	parts := strings.Split(s, "-")
	type element struct {
		kind  string // "affix" or "scope"
		affix g.Affix
		scope g.AffixScope
	}
	elems := make([]element, 0, len(parts))
	for _, p := range parts {
		if strings.HasPrefix(p, "{") && strings.HasSuffix(p, "}") {
			scope, ok := parseScopeName(p[1 : len(p)-1])
			if !ok {
				return nil, fmt.Errorf("affixual adjunct: unknown scope %q", p)
			}
			elems = append(elems, element{kind: "scope", scope: scope})
			continue
		}
		a, err := parseAffixField(p, affixes)
		if err != nil {
			return nil, fmt.Errorf("affixual adjunct: %w", err)
		}
		elems = append(elems, element{kind: "affix", affix: a})
	}
	// Count affixes and lay them out with scope tails.
	var first g.Affix
	var rest []g.Affix
	firstScope := g.ScopeVDom
	restScope := g.ScopeVDom
	firstAffixSeen := false
	scopeSeenAfterFirst := false
	for _, e := range elems {
		if e.kind == "affix" {
			if !firstAffixSeen {
				first = e.affix
				firstAffixSeen = true
			} else {
				rest = append(rest, e.affix)
			}
			continue
		}
		// scope
		if !firstAffixSeen {
			return nil, fmt.Errorf("affixual adjunct: scope before any affix")
		}
		if !scopeSeenAfterFirst && len(rest) == 0 {
			// First scope position: between first affix and rest affixes.
			firstScope = e.scope
			scopeSeenAfterFirst = true
			continue
		}
		// Otherwise treat as rest scope (the trailing one).
		restScope = e.scope
	}
	if !firstAffixSeen {
		return nil, fmt.Errorf("affixual adjunct: no affix found")
	}
	if !scopeSeenAfterFirst && len(rest) == 0 {
		// Single-affix form: collapses to SingleAffixAdjunct.
		return tokenize.SingleAffixWord{
			Text:  s,
			Affix: g.SingleAffixAdjunct{Affix: first, Scope: restScope},
		}, nil
	}
	if len(rest) == 0 {
		// First-scope tagged but no further affixes — still single-affix
		// with that scope.
		return tokenize.SingleAffixWord{
			Text:  s,
			Affix: g.SingleAffixAdjunct{Affix: first, Scope: firstScope},
		}, nil
	}
	// Multi-affix: when no explicit rest scope, it matches firstScope.
	if restScope == g.ScopeVDom && firstScope != g.ScopeVDom {
		restScope = firstScope
	}
	return tokenize.MultipleAffixWord{
		Text: s,
		Affixes: g.MultipleAffixAdjunct{
			First:      first,
			Rest:       rest,
			FirstScope: firstScope,
			RestScope:  restScope,
		},
	}, nil
}

// parseAffixField decodes a "Cs/N[_2|_3]" segment into a grammar.Affix.
// Accepts either a lexicon abbreviation or a raw Cs cluster.
func parseAffixField(s string, affixes map[string]lexicon.AffixEntry) (g.Affix, error) {
	slash := strings.Index(s, "/")
	if slash < 1 {
		return g.Affix{}, fmt.Errorf("not an affix field: %q", s)
	}
	csOrAbbrev := s[:slash]
	tail := s[slash+1:]
	atype := g.Type1Affix
	if i := strings.Index(tail, "_"); i >= 0 {
		switch tail[i+1:] {
		case "2":
			atype = g.Type2Affix
		case "3":
			atype = g.Type3Affix
		default:
			return g.Affix{}, fmt.Errorf("unknown Type suffix %q", tail[i:])
		}
		tail = tail[:i]
	}
	if len(tail) != 1 || tail[0] < '0' || tail[0] > '9' {
		return g.Affix{}, fmt.Errorf("expected single-digit degree, got %q", tail)
	}
	degree := int(tail[0] - '0')
	cs := resolveAffixCs(csOrAbbrev, affixes)
	if cs == "" {
		return g.Affix{}, fmt.Errorf("unknown affix %q", csOrAbbrev)
	}
	return g.Affix{Type: atype, Degree: degree, Consonant: cs}, nil
}

// parseScopeName maps an AffixScope name (the String() form) back to
// the enum value.
func parseScopeName(s string) (g.AffixScope, bool) {
	for _, sc := range g.AllAffixScopes {
		if sc.String() == s {
			return sc, true
		}
	}
	return 0, false
}

// parseReferentialToken decodes a canonical referential or combination
// referential of the form "refs-CASE[-...]" — bare referent head
// followed by hyphen-separated slot tails. The third chunk being one
// of {BSC, CTE, CSV, OBJ} discriminates a combination referential.
//
// MVP: single-referent head only (no "[a+b]" multi-ref lists or
// Category prefix). Carrier-headed referentials are also deferred
// here (they share the "[TYPE]" shape with CarrierWord and need
// further disambiguation).
func parseReferentialToken(s string) (tokenize.WordToken, error) {
	parts := strings.Split(s, "-")
	if len(parts) < 2 {
		return nil, fmt.Errorf("referential %q: need at least head and case", s)
	}
	ref, eff, err := parseRefSpec(parts[0])
	if err != nil {
		return nil, fmt.Errorf("referential %q head: %w", s, err)
	}
	caseName := parts[1]
	c, ok := parseCaseName(caseName)
	if !ok {
		return nil, fmt.Errorf("referential %q: unknown case %q", s, caseName)
	}
	rest := parts[2:]
	// Combination referential? Detect Specification name in next slot.
	if len(rest) > 0 {
		if spec, ok := parseSpecName(rest[0]); ok {
			return tokenize.CombinationRefWord{
				Text: s,
				Refs: []referentials.PersonalRef{{Referent: ref, Effect: eff}},
				Case: c,
				Spec: spec,
				// Trailing affixes / Case2 parsing deferred — common
				// simple cases just have Spec.
			}, nil
		}
	}
	// Plain referential. Parse optional Case2, [refB], and RPV trail.
	var case2 *g.Case
	var refB []referentials.PersonalRef
	rpv := false
	for _, p := range rest {
		switch {
		case p == "RPV":
			rpv = true
		case strings.HasPrefix(p, "[") && strings.HasSuffix(p, "]"):
			inner := p[1 : len(p)-1]
			refB, err = parseRefList(inner)
			if err != nil {
				return nil, fmt.Errorf("referential %q refB %q: %w", s, p, err)
			}
		default:
			if cv, ok := parseCaseName(p); ok && case2 == nil {
				case2 = &cv
				continue
			}
			return nil, fmt.Errorf("referential %q: unexpected trailing slot %q", s, p)
		}
	}
	return tokenize.ReferentialWord{
		Text:       s,
		Refs:       []referentials.PersonalRef{{Referent: ref, Effect: eff}},
		Case:       &c,
		Case2:      case2,
		RefB:       refB,
		RpvEssence: rpv,
	}, nil
}

// parseRefList decodes "a+b+c" into a slice of PersonalRefs.
func parseRefList(s string) ([]referentials.PersonalRef, error) {
	parts := strings.Split(s, "+")
	out := make([]referentials.PersonalRef, 0, len(parts))
	for _, p := range parts {
		ref, eff, err := parseRefSpec(p)
		if err != nil {
			return nil, err
		}
		out = append(out, referentials.PersonalRef{Referent: ref, Effect: eff})
	}
	return out, nil
}

// parseCaseName looks up a Case enum value by its String() form.
func parseCaseName(s string) (g.Case, bool) {
	for _, c := range g.AllCases {
		if c.String() == s {
			return c, true
		}
	}
	return 0, false
}

// parseSpecName decodes a Specification abbreviation (BSC/CTE/CSV/OBJ)
// into the typed enum value.
func parseSpecName(s string) (g.Specification, bool) {
	switch s {
	case "BSC":
		return g.BSC, true
	case "CTE":
		return g.CTE, true
	case "CSV":
		return g.CSV, true
	case "OBJ":
		return g.OBJ, true
	}
	return 0, false
}

// parseModularToken decodes the canonical form of a modular adjunct:
//
//	MOD                       — all-default (MNO Valence + FAC Mood/CaseScope)
//	RTR.SUB                   — typed Vn.Cn content
//	RTR.SUB-{parent}          — with non-default application scope
//	MOD-{concat}              — empty body with scope
//
// Returns a tokenize.ModularWord. Reach scope (V_H, §4.3 Slot 4) is
// not yet representable in our data model and falls out of round-trip.
func parseModularToken(s string) (tokenize.WordToken, error) {
	// The reach suffix comes after the application-scope suffix in
	// canonical output, so peel it first.
	body, reach := trimModularReach(s)
	body, scope := trimModularScope(body)
	ma := g.ModularAdjunct{Scope: scope, Reach: reach}
	if body != "MOD" {
		// One or more Vn.Cn entries joined by "-" in display, or just
		// "VN.CN" / "VN" alone for canonical single-pair.
		for _, entry := range strings.Split(body, "-") {
			dot := strings.Index(entry, ".")
			var vnName, cnName string
			if dot < 0 {
				vnName = entry
			} else {
				vnName, cnName = entry[:dot], entry[dot+1:]
			}
			sv, err := slotVIIIFromNames(vnName, cnName)
			if err != nil {
				return nil, fmt.Errorf("modular %q: %w", s, err)
			}
			ma.Content = append(ma.Content, sv)
		}
	}
	return tokenize.ModularWord{Text: s, Modular: ma}, nil
}

// trimModularReach and trimModularScope strip a trailing "-{name}"
// marker and return the value it names, or the default when there is
// none. Both invert the String forms rather than repeating the names,
// so a rename in grammar cannot leave the parser reading the old ones.
func trimModularReach(s string) (string, g.ModularReach) {
	for _, r := range g.AllModularReaches {
		if r == g.ModularReachNone {
			continue
		}
		if body, ok := strings.CutSuffix(s, "-{"+r.String()+"}"); ok {
			return body, r
		}
	}
	return s, g.ModularReachNone
}

func trimModularScope(s string) (string, g.ModularScope) {
	for _, sc := range g.AllModularScopes {
		if sc == g.ModularScopeDefault {
			continue
		}
		if body, ok := strings.CutSuffix(s, "-{"+sc.String()+"}"); ok {
			return body, sc
		}
	}
	return s, g.ModularScopeDefault
}

// slotVIIIFromNames builds a typed SlotVIII from the canonical Vn and
// Cn abbreviations (e.g. "RTR", "SUB"). Empty cn defaults to FAC.
func slotVIIIFromNames(vn, cn string) (g.SlotVIII, error) {
	mood := g.FAC
	if cn != "" {
		m, ok := lookupMoodOrCaseScope(cn)
		if !ok {
			return nil, fmt.Errorf("unknown Mood/CaseScope %q", cn)
		}
		mood = m
	}
	// Try each Vn category in order: Valence, Phase, Effect, Level, Aspect.
	for _, v := range g.AllValences {
		if v.String() == vn {
			return g.VnCnValence{Valence: v, MoodScope: mood}, nil
		}
	}
	for _, p := range g.AllPhases {
		if p.String() == vn {
			return g.VnCnPhase{Phase: p, MoodScope: mood}, nil
		}
	}
	for _, e := range g.AllEffects {
		if e.String() == vn {
			return g.VnCnEffect{Effect: e, MoodScope: mood}, nil
		}
	}
	for _, lv := range g.AllLevels {
		if lv.String() == vn {
			return g.VnCnLevel{Level: lv, MoodScope: mood}, nil
		}
	}
	for _, a := range g.AllAspects {
		if a.String() == vn {
			return g.VnCnAspect{Aspect: a, MoodScope: mood}, nil
		}
	}
	return nil, fmt.Errorf("unknown Vn category %q", vn)
}

// lookupMoodOrCaseScope accepts either a Mood (FAC/SUB/...) or a
// CaseScope (CCN/CCA/...) abbreviation; both map to the same set of
// six values internally stored as grammar.Mood.
func lookupMoodOrCaseScope(s string) (g.Mood, bool) {
	for _, m := range g.AllMoods {
		if m.String() == s {
			return m, true
		}
	}
	for _, cs := range g.AllCaseScopes {
		if cs.String() == s {
			return g.CaseScopeToMood(cs), true
		}
	}
	return 0, false
}

// Silence unused-import warning until all parsers land.
var _ = parse.ClassifyAffixVowel
