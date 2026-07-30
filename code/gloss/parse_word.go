package gloss

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/parse"
)

// ParseWord parses a single canonical-gloss token (whitespace-delimited
// unit) into a g.Word. Inverse of Glosser.Word when Canonical=true.
//
// Dispatch is structural: each adjunct type has a distinctive shape
// (leading bracket, trailing colon, bare uppercase, and so on), which
// is what the one-job-per-mark rule in SPEC.md buys. No lookup is
// needed to decide which kind of word a token is.
func ParseWord(s string, lex *lexicon.Lexicon) (g.Word, error) {
	s = strings.TrimSpace(s)
	if s == "" {
		return nil, fmt.Errorf("empty token")
	}
	// Affix names resolve through the lexicon when there is one. A nil
	// lexicon still parses everything else, and affixes written as a
	// raw Cs cluster rather than an abbreviation.
	var affixes map[string]lexicon.AffixEntry
	if lex != nil {
		affixes = lex.Affixes
	}

	// Foreign word: double-quoted text.
	if len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' {
		return g.Foreign{Text: s[1 : len(s)-1]}, nil
	}

	// Register end: "<NAME>_END"
	if strings.HasSuffix(s, "_END") {
		if r, ok := parseRegisterName(strings.TrimSuffix(s, "_END")); ok {
			return g.RegisterMarker{Register: r, End: true}, nil
		}
	}

	// Carrier: "[TYPE]" or "[TYPE]-CASE"
	if strings.HasPrefix(s, "[") {
		if w, ok, err := parseCarrierOrReferential(s, affixes); ok || err != nil {
			return w, err
		}
	}

	// Bare uppercase abbreviation: bias OR register-start.
	if isBareUppercase(s) {
		if b, ok := parseBiasName(s); ok {
			return b, nil
		}
		if r, ok := parseRegisterName(s); ok {
			return g.RegisterMarker{Register: r}, nil
		}
	}

	// Referential or combination-referential with bare-referent head.
	if looksLikeReferential(s) {
		return parseReferentialToken(s, affixes)
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

	// Default: try as a formative via the existing Formative. affixes
	// is nil when lex is, which Formative accepts — it then takes an
	// affix written as a raw Cs cluster but not one written as an
	// abbreviation, and says so. Refusing the whole token for want of a
	// lexicon would fail every root as well.
	f, err := ParseFormative(s, affixes)
	if err != nil {
		return nil, fmt.Errorf("formative parse failed: %w", err)
	}
	return f, nil
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
//
// "[TYPE]" and "[TYPE]-CASE" are a plain carrier adjunct. Anything
// with more tail than that is a §4.6.3 suppletive-headed referential,
// which carries grammar a CarrierAdjunct has no room for. The two need
// no sigil to tell apart: a carrier adjunct holds one case and nothing
// else, so the extra slots are themselves the signal.
//
// A leading referent list ("[1m+2p]-ERG") is a multi-referent
// referential.
func parseCarrierOrReferential(s string, affixes map[string]lexicon.AffixEntry) (g.Word, bool, error) {
	end := strings.Index(s, "]")
	if end < 1 {
		return nil, false, nil
	}
	inner := s[1:end]
	tail := s[end+1:]
	if ct, ok := parseCarrierTypeAbbrev(inner); ok {
		caseChunk, restTail := splitFirstHyphenChunk(tail)
		if restTail == "" {
			cv, err := caseFromCanonicalTail(tail)
			if err != nil {
				return nil, true, fmt.Errorf("carrier %q: %w", s, err)
			}
			return g.CarrierAdjunct{Type: ct, Case: cv}, true, nil
		}
		w, err := buildReferential(s, g.SuppletiveHead{Type: ct},
			append([]string{caseChunk}, strings.Split(restTail, "-")...), affixes)
		return w, true, err
	}
	refs, err := parseRefList(inner)
	if err != nil {
		return nil, true, fmt.Errorf("referential %q: %w", s, err)
	}
	caseChunk, restTail := splitFirstHyphenChunk(tail)
	if caseChunk == "" {
		return nil, true, fmt.Errorf("referential %q: no case after referent list", s)
	}
	parts := []string{caseChunk}
	if restTail != "" {
		parts = append(parts, strings.Split(restTail, "-")...)
	}
	w, err := buildReferential(s, g.PersonalHead{Refs: refs}, parts, affixes)
	return w, true, err
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

// buildReferential decodes the slot tail that §4.6.1 and §4.6.2
// share, given an already-parsed head. parts starts at the case:
//
//	CASE [SPEC [affix...]] [CASE2] [[refs]/CASE] [RPV]
//
// A Specification in the second slot is what makes it a combination
// referential; §4.6.2 lists x/xt/xp/xx as that shape's tell-tale sign,
// and the four names are disjoint from every case name.
//
// The two shapes are parsed here together rather than in two places,
// because their tails overlap: both may end in a stacked case and an
// RPV marker, and a copy of that logic in each is what let the
// combination referential silently drop its affixes.
func buildReferential(
	text string,
	head g.RefHead,
	parts []string,
	affixes map[string]lexicon.AffixEntry,
) (g.Word, error) {
	if len(parts) == 0 || parts[0] == "" {
		return nil, fmt.Errorf("referential %q: missing case", text)
	}
	c, ok := parseCaseName(parts[0])
	if !ok {
		return nil, fmt.Errorf("referential %q: unknown case %q", text, parts[0])
	}
	rest := parts[1:]
	if len(rest) > 0 {
		if spec, ok := parseSpecName(rest[0]); ok {
			return buildCombinationRef(text, head, c, spec, rest[1:], affixes)
		}
	}
	ref := g.Referential{Head: head, Case: c}
	for _, p := range rest {
		switch {
		case p == "RPV":
			ref.RpvEssence = true
		case strings.HasPrefix(p, "["):
			// "[refs]/CASE": a second referent carrying its own case.
			end := strings.Index(p, "]")
			if end < 1 || !strings.HasPrefix(p[end+1:], "/") {
				return nil, fmt.Errorf("referential %q: %q is not [refs]/CASE", text, p)
			}
			refs, err := parseRefList(p[1:end])
			if err != nil {
				return nil, fmt.Errorf("referential %q second referent %q: %w", text, p, err)
			}
			cv, ok := parseCaseName(p[end+2:])
			if !ok {
				return nil, fmt.Errorf("referential %q: unknown case %q", text, p[end+2:])
			}
			if ref.Second != nil {
				return nil, fmt.Errorf("referential %q: two second referents", text)
			}
			ref.Second = &g.SecondReferent{Case: cv, Refs: refs}
		default:
			// A bare case with no referent of its own stacks onto the head.
			cv, ok := parseCaseName(p)
			if !ok {
				return nil, fmt.Errorf("referential %q: unexpected slot %q", text, p)
			}
			if ref.Second != nil {
				return nil, fmt.Errorf("referential %q: two second cases", text)
			}
			ref.Second = &g.SecondReferent{Case: cv}
		}
	}
	return ref, nil
}

// buildCombinationRef reads the §4.6.2 tail after the Specification:
// any number of V_X C_S affixes, then an optional stacked case, then
// an optional RPV marker.
func buildCombinationRef(
	text string,
	head g.RefHead,
	c g.Case,
	spec g.Specification,
	rest []string,
	affixes map[string]lexicon.AffixEntry,
) (g.Word, error) {
	comb := g.CombinationReferential{Head: head, Case: c, Spec: spec}
	for _, p := range rest {
		if p == "RPV" {
			comb.RpvEssence = true
			continue
		}
		if a, err := parseAffixField(p, affixes); err == nil {
			comb.Affixes = append(comb.Affixes, a)
			continue
		}
		cv, ok := parseCaseName(p)
		if !ok {
			return nil, fmt.Errorf("combination referential %q: unexpected slot %q", text, p)
		}
		if comb.Case2 != nil {
			return nil, fmt.Errorf("combination referential %q: two stacked cases", text)
		}
		comb.Case2 = &cv
	}
	return comb, nil
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
// symbol followed by "-" (e.g. "1m-ERG", "ma-AFF-DAT-2m"), optionally
// behind a "CAT:" category tag ("NOM:1m-ERG"). Multi-ref forms like
// "[1m+2p]-..." are detected by the "[" prefix instead.
func looksLikeReferential(s string) bool {
	dash := strings.Index(s, "-")
	if dash < 1 {
		return false
	}
	_, head := splitCategoryTag(s[:dash])
	// The head is a referent abbreviation, so ask the parser for one.
	// None of the eleven can be mistaken for a formative's root: a root
	// is written as a bare consonant cluster, and every abbreviation
	// either starts with a digit ("1m"), carries a vowel ("ma"), or
	// capitalises ("Mx", "Obv").
	_, _, err := parseRefSpec(head)
	return err == nil
}

// splitCategoryTag peels the "AGM:"/"NOM:"/"ABS:" tag §4.6 puts on a
// referent list, returning the category (nil when absent) and the rest.
func splitCategoryTag(s string) (*g.RefCategory, string) {
	name, rest, found := strings.Cut(s, ":")
	if !found {
		return nil, s
	}
	for _, c := range []g.RefCategory{
		g.Agglomerative, g.Nomic, g.Abstract,
	} {
		if c.String() == name {
			cat := c
			return &cat, rest
		}
	}
	return nil, s
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
	// §4.3 has three content slots and the glosser joins them with
	// "-", so every entry is tested rather than the body as a whole.
	// An entry is a V_N category name, optionally followed by ".Cn"
	// for a Mood or Case-Scope other than the default.
	//
	// A dot used to be required somewhere in the body, which claimed
	// "RTR.SUB" and "MNO.SUB-PRG" but not "MNO-PRG": a list whose
	// entries are all at the default Mood has no dot anywhere, and it
	// fell through to the formative parser as a rootless gloss.
	// Every entry naming a V_N category is what makes this shape a
	// modular adjunct, and no formative gloss can match it, a
	// formative always carrying a root slot.
	for _, entry := range strings.Split(body, "-") {
		name := entry
		if dot := strings.Index(entry, "."); dot >= 0 {
			name = entry[:dot]
			if !isBareUppercase(entry[dot+1:]) {
				return false
			}
		}
		if !isBareUppercase(name) || !isVnCategoryName(name) {
			return false
		}
	}
	return body != ""
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
	// An affixual adjunct is affixes and nothing else: every slot is
	// either an affix field or a brace. A formative can carry the same
	// affix slots, but it also carries a root, so one slot that is
	// neither settles it.
	//
	// A "{" used to be taken as proof on its own, which read the
	// formative "m-SYS/5_2-{Ca}-DCD/1_2" as an affixual adjunct: the
	// "{Ca}" there is the Slot V/VII boundary, not an affix scope.
	//
	// A parenthesised head settles it the other way: "()" holds a C_S
	// root, which only a formative has, so "(CTR)/1" is a root at
	// degree 1 rather than an affix at degree 1.
	if strings.HasPrefix(s, "(") {
		return false
	}
	// Every slot is an affix field or a scope in braces. A brace group
	// counts only when it names a scope, so "{Ca}" does not qualify
	// even where no bare root slot stands beside it.
	for _, p := range strings.Split(s, "-") {
		if strings.HasPrefix(p, "{") && strings.HasSuffix(p, "}") {
			if _, ok := parseScopeName(strings.Trim(p, "{}")); ok {
				continue
			}
			return false
		}
		if strings.Contains(p, "/") {
			continue
		}
		return false
	}
	return true
}

// ParseText splits a canonical-gloss sentence on whitespace and
// dispatches each token via ParseWord. Returns the slice of tokens
// in order; errors include the failing token's index and content.
func ParseText(s string, lex *lexicon.Lexicon) ([]g.Word, error) {
	fields := strings.Fields(s)
	out := make([]g.Word, 0, len(fields))
	for i, f := range fields {
		tok, err := ParseWord(f, lex)
		if err != nil {
			return nil, fmt.Errorf("token %d (%q): %w", i, f, err)
		}
		out = append(out, tok)
	}
	return joinChains(out)
}

// joinChains gathers concatenated formatives back into the chain they
// were glossed from.
//
// A chain is one word written with hyphens, but its canonical gloss is
// its members separated by a space, so ParseToken cannot see it: it is
// handed one member at a time. What survives the split is the Slot I
// marker each dependent carries, and §3.1.7 makes it enough. A
// dependent has a Cc and the parent has none, so a run of dependents
// closed by a plain formative is exactly one chain.
func joinChains(words []g.Word) ([]g.Word, error) {
	out := make([]g.Word, 0, len(words))
	var pending []g.Formative
	for _, w := range words {
		f, isFormative := w.(g.Formative)
		if !isFormative || f.Concat == g.ConcatNone {
			if len(pending) > 0 {
				if !isFormative {
					return nil, fmt.Errorf(
						"concatenated formative is followed by %T rather than the parent it needs", w)
				}
				chain := g.NewChain(f)
				for _, d := range pending {
					switch d.Concat {
					case g.Type1:
						chain.AddType1(d)
					case g.Type2:
						chain.AddType2(d)
					}
				}
				out = append(out, chain)
				pending = nil
				continue
			}
			out = append(out, w)
			continue
		}
		pending = append(pending, f)
	}
	if len(pending) > 0 {
		return nil, fmt.Errorf("%d concatenated formatives with no parent after them", len(pending))
	}
	return out, nil
}

// parseAffixualAdjunct decodes the canonical form of an affixual
// adjunct — single or multi — into a g.Word.
//
// Single: "Cs/N" optionally followed by "-{scope}".
// Multi:  "Cs1/N1[-{s1}]-Cs2/N2-Cs3/N3...[-{sN}]".
//
// Each affix segment is "Cs/N" with an optional "_2"/"_3" Type suffix.
// Scope segments are wrapped in "{}". Default-VDom scopes are absent.
func parseAffixualAdjunct(s string, affixes map[string]lexicon.AffixEntry) (g.Word, error) {
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
		return g.SingleAffixAdjunct{Affix: first, Scope: restScope}, nil
	}
	if len(rest) == 0 {
		// First-scope tagged but no further affixes — still single-affix
		// with that scope.
		return g.SingleAffixAdjunct{Affix: first, Scope: firstScope}, nil
	}
	// Multi-affix: when no explicit rest scope, it matches firstScope.
	if restScope == g.ScopeVDom && firstScope != g.ScopeVDom {
		restScope = firstScope
	}
	return g.MultipleAffixAdjunct{
		First:      first,
		Rest:       rest,
		FirstScope: firstScope,
		RestScope:  restScope,
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

// parseReferentialToken decodes a referential whose head is written
// bare, as a single referent with an optional §4.6 category tag
// ("NOM:1m-ERG"). Multi-referent and suppletive heads are bracketed
// and reach the tail parser through parseCarrierOrReferential.
func parseReferentialToken(s string, affixes map[string]lexicon.AffixEntry) (g.Word, error) {
	parts := strings.Split(s, "-")
	if len(parts) < 2 {
		return nil, fmt.Errorf("referential %q: need at least head and case", s)
	}
	category, head := splitCategoryTag(parts[0])
	ref, eff, err := parseRefSpec(head)
	if err != nil {
		return nil, fmt.Errorf("referential %q head: %w", s, err)
	}
	return buildReferential(s, g.PersonalHead{
		Refs:     []g.PersonalRef{{Referent: ref, Effect: eff}},
		Category: category,
	}, parts[1:], affixes)
}

func parseRefList(s string) ([]g.PersonalRef, error) {
	parts := strings.Split(s, "+")
	out := make([]g.PersonalRef, 0, len(parts))
	for _, p := range parts {
		ref, eff, err := parseRefSpec(p)
		if err != nil {
			return nil, err
		}
		out = append(out, g.PersonalRef{Referent: ref, Effect: eff})
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
// Returns a g.ModularAdjunct. Reach scope (V_H, §4.3 Slot 4)
// rides the trailing "-{formative}"-style marker and round-trips.
func parseModularToken(s string) (g.Word, error) {
	// The reach suffix comes after the application-scope suffix in
	// canonical output, so peel it first.
	body, reach := trimModularReach(s)
	body, scope := trimModularScope(body)
	ma := g.ModularAdjunct{Scope: scope, Reach: reach}
	if body == "MOD" {
		// "MOD" is what the glosser writes when the content is all at
		// its defaults, because slotVIII suppresses MNO and FAC and
		// leaves nothing to print. It names that content rather than
		// no content: §4.3 Slot 4 is mandatory, so a modular adjunct
		// with an empty Content is not a word, and reading "MOD" as
		// one dropped the Valence the word carried and produced a
		// value the writer then refused.
		ma.Content = []g.SlotVIII{g.VnCnValence{Valence: g.MNO, MoodScope: g.FAC}}
	} else {
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
	return ma, nil
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
