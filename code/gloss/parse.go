package gloss

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/christian-oudard/ithkuil/allomorph"
	"github.com/christian-oudard/ithkuil/fault"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
)

// Formative builds a grammar.Formative from a gloss-style authoring
// expression. The syntax is exactly the canonical gloss: slots are
// separated by "-", category values within a slot by ".", and "/"
// binds an argument — a degree or a case — to a head. The root cluster
// is written in plain Ithkuil orthography or ASCII digraphs (aa→ä,
// t,→ţ, sq→š, cq→č, dz→ẓ). Every other token is either a grammatical
// abbreviation (Stem, Version, Function, Specification, Context, Case,
// Aspect, Valence, Mood, Illocution, Stress) or an affix written
// "Cs/degree" or "ABBREV/degree".
//
// Affixes land in Slot VII unless they precede the Ca complex, which
// is written either as its components ("MSS.G") or, when every
// component is at its default, as the "{Ca}" marker. Slot V affixes
// apply to the stem alone; Slot VII affixes have scope over the Ca.
//
// Examples
//
//	ml                              minimal nominal formative on root "ml"
//	S2.CPT-ml-ERG                   stem 2, completive, ergative case
//	S2.CPT-ml-DYN.OBJ-DEV/3-ERG     plus dynamic+objective and a DEV/3 affix
//	m-ţř/5_2-{Ca}-t/1_2             SYS/5 in Slot V, DCD/1 in Slot VII
//	t,k-FNC                         ASCII digraph root "t,k" → "ţk"
//	ml-Ca:PRX-ERG                   a Ca stacked on the Slot VI Ca
//	ml-ACC/INS-ERG                  a §3.9.2 case-accessor
//	ml-(1m)/AFF-ERG                 a §4.6.5 Column-4 referential
//
// Lexicon-aware affix resolution requires passing an AffixMap; pass
// nil to accept only the bare Cs form.
func ParseFormative(s string, affixes map[string]lexicon.AffixEntry) (g.Formative, error) {
	s = strings.TrimSpace(s)
	if s == "" {
		return g.Formative{}, syntax("", "a gloss needs at least a root cluster")
	}
	// Collapse run of "-" into a single separator: gloss output writes
	// "S2.CPT--ml-…" with a double hyphen around the root marker.
	tokens := splitSlots(s)
	if len(tokens) == 0 {
		return g.Formative{}, syntax(s, "a gloss is slots joined by \"-\", and this has none")
	}

	// Identify the root. A bare Cr cluster (lowercase letters /
	// Ithkuil chars) beats a parenthesised form when both are
	// present: in "S3-tpl-(1m/BEN)/3" the `tpl` is the root and
	// `(1m/BEN)/3` is a Type-3 referential affix in Slot VII.
	// A bad root does not end the reading. It used to, which left
	// every other token unjudged while the report still had to say
	// something about them — and "ok" for a token nobody looked at is
	// a claim the reader never made. The cluster is folded to
	// lowercase and kept so the rest can be read against something;
	// the Formative is discarded when anything failed.
	var fs collected
	spent := map[int]bool{}
	rootIdx := -1
	var root g.Root
	for i, tok := range tokens {
		cluster, ok := isClusterToken(tok)
		if !ok {
			continue
		}
		if rootIdx >= 0 {
			// Consumed, not merely rejected. A token that read as a
			// root is a root; letting the slot loop have a second go
			// at it added "no grammatical value is named TPL" beside
			// the real complaint, which is the fallthrough this syntax
			// avoids everywhere else.
			fs.add(inToken(tok, syntax(tok,
				"a gloss has one root, and "+tokens[rootIdx]+" already read as one")))
			spent[i] = true
			continue
		}
		if err := validateRootCluster(cluster); err != nil {
			fs.add(inToken(tok, err))
			cluster = strings.ToLower(cluster)
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
				fs.add(inToken(tok, err))
				spent[i] = true
				continue
			}
			if !ok {
				continue
			}
			if rootIdx >= 0 {
				fs.add(inToken(tok, syntax(tok,
					"a gloss has one root, and "+tokens[rootIdx]+" already read as one")))
				spent[i] = true
				continue
			}
			rootIdx = i
			root = r
		}
	}
	if rootIdx < 0 {
		// No root, but the slots are still readable and a writer
		// wants to know which of them are also wrong. Reading stops
		// here only if nothing else could be judged; a placeholder
		// root lets the rest be, and the Formative is discarded.
		fs.add(syntax(s,
			"a gloss needs a root: a lowercase consonant cluster, (ABBREV)/degree, or (1m+2p)"))
		root = g.DefaultCrRoot("l")
	}

	f := g.Formative{
		Root:   root,
		SlotVI: g.DefaultSlotVI,
		Final:  g.UnframedNominal{Case: g.THM},
	}
	// Reading is permissive: a token that fails records its fault and
	// the rest are still read. Stopping at the first meant a gloss
	// with three bad tokens took three attempts to fix, each one
	// revealing the next, and the half-built Formative is discarded
	// anyway — a non-empty ledger is an error — so carrying on costs
	// nothing but tells the writer everything at once.
	caIdx := caTokenIndex(tokens, rootIdx)
	seen := newAssigned()
	for i, tok := range tokens {
		if i == rootIdx || spent[i] || tok == caMarker {
			continue
		}
		fs.add(inToken(tok, applyToken(&f, tok, affixes, i < caIdx, seen)))
	}
	if err := fs.err(s); err != nil {
		return g.Formative{}, err
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

// validateRootCluster rejects a Cr no root could be. isClusterToken
// accepts any token carrying a lowercase letter, so without this
// "qqq" composed to "aqqqal", which is not spelled in Ithkuil at all
// and does not round-trip, and "zzzz" to "azzzzal", which our own
// validator rejects for a triple consonant.
//
// Only these two rules are applied, because only these two are free
// of false positives across the lexicon: run over all 5946 roots and
// 528 affixes, neither fires once. The §2 pair rules fire on two
// roots, "ňkhw" and "řẓňy", which break §1.2.2's ban on ň before k
// and the rule against ň before y. Both are community coinages, so
// whether they are lexicon errors is a separate question, and a Cr is
// not held to §2 here until it is answered.
func validateRootCluster(cluster string) error {
	return validateCluster("root", cluster)
}

// validateCluster applies the two rules to a Cr or a Cs. kind names
// which, for the message. Neither fires on any of the 5946 lexicon
// roots or 528 affixes, so nothing attested is at risk.
func validateCluster(kind, cluster string) error {
	// A capital in a cluster is not an Ithkuil letter, it is a Latin
	// one. CheckChars lowercases before it looks, so "Ml" passed and
	// composed to "aMlal" — Latin capitals inside an Ithkuil word,
	// which nothing can read back. isClusterToken accepts a token
	// carrying any lowercase letter, which is the right test for
	// telling a root from an all-uppercase abbreviation and the wrong
	// one for deciding the root is well formed.
	if cluster != strings.ToLower(cluster) {
		return fault.One(cluster, fault.Fault{
			Stage: fault.Chars,
			Code:  kind,
			Found: cluster,
			Fix:   "a " + kind + " cluster is written in lowercase",
		})
	}
	if v := phonology.CheckChars(cluster); len(v) > 0 {
		return clusterFault(kind, cluster)
	}
	if phonology.HasTripleConsonant(cluster) {
		return clusterFault(kind, cluster)
	}
	return nil
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
			return nil, true, unlisted(m[1], "affix", m[1])
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
		return nil, true, syntax(tok, "a parenthesised head holds at least one referent")
	}
	parts := strings.Split(inner, "+")
	refs := make([]g.PersonalRef, 0, len(parts))
	for _, part := range parts {
		ref, eff, err := parseRefSpec(part)
		if err != nil {
			return nil, true, err
		}
		refs = append(refs, g.PersonalRef{Referent: ref, Effect: eff})
	}
	return g.RefRoot{Refs: refs, Version: g.PRC, SlotIV: g.DefaultSlotIV}, true, nil
}

// parseRefSpec decodes "1m" or "1m/BEN" into a Referent + Effect.
func parseRefSpec(s string) (g.Referent, g.RefEffect, error) {
	refName, effName, _ := strings.Cut(s, "/")
	var ref g.Referent
	matched := false
	for _, r := range g.AllReferents {
		if r.String() == refName {
			ref = r
			matched = true
			break
		}
	}
	if !matched {
		return 0, 0, unlisted(refName, "referent", refName)
	}
	eff := g.NEU
	if effName != "" {
		matched = false
		for _, e := range g.AllRefEffects {
			if e.String() == effName {
				eff = e
				matched = true
				break
			}
		}
		if !matched {
			return 0, 0, unlisted(effName, "effect", effName)
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
// "." and recurses for compound slot groups like "S2.CPT". slotV
// selects which affix slot the token lands in when it is an affix.
func applyToken(f *g.Formative, tok string, affixes map[string]lexicon.AffixEntry, slotV bool, seen assigned) error {
	// Type-3 referential affix: "(refs)/degree" where refs is a
	// referent list like "1m" or "1m/BEN+2p/DET". The cluster is
	// the concatenation of each ref's C1 form.
	if strings.HasPrefix(tok, "(") {
		return appendType3Affix(f, tok, slotV)
	}
	// Ca-stacking affix: "Ca:" plus the same component list the Slot VI
	// Ca uses. Checked before the generic slash-split path, which would
	// otherwise not know what to do with the tag.
	if strings.HasPrefix(tok, caStackPrefix) {
		return appendCaStack(f, strings.TrimPrefix(tok, caStackPrefix), slotV)
	}
	// §3.9.2 case-accessor family: "ACC/CASE", "IAC/CASE_3", "CST/CASE".
	if m := accessorToken.FindStringSubmatch(tok); m != nil {
		return appendAccessor(f, m[1], m[2], m[3], slotV)
	}
	// Affix form takes precedence over the generic slash-split path:
	// "DEV/3" must mean affix DEV degree 3, not flag DEV plus flag 3.
	if m := affixToken.FindStringSubmatch(tok); m != nil {
		return appendAffix(f, m[1], m[2], m[3], affixes, slotV)
	}
	// "." joins category values inside one slot — S2.CPT, DYN.OBJ,
	// MSS.G.RPV, ASR.INF — which flatten to a list of independent
	// flags. It is the only separator that does this: "/" binds an
	// argument to a head, and every shape that uses it was tried above,
	// so a "/" still here is an error rather than a grouping.
	// A "/" still here means the token claimed one of the shapes above
	// and failed inside it. Falling through to the plain-flag reading
	// described the token as something it never was: "DEV/99" came
	// back "unknown grammar flag", naming an affix the lexicon knows
	// as if it were unheard of, and sending the reader to look for a
	// typo in DEV rather than in the degree.
	//
	// So the shapes are committing. This is the same property the
	// romanization parser has and for the same reason: a reading that
	// gets far enough to identify what a token is meant to be should
	// report against that, not retry as something less specific.
	if strings.Contains(tok, "/") {
		return slashTokenFault(tok, affixes)
	}
	if strings.Contains(tok, ".") {
		for _, part := range strings.Split(tok, ".") {
			if part == "" {
				continue
			}
			if err := seen.apply(f, part); err != nil {
				return err
			}
		}
		return nil
	}
	return seen.apply(f, tok)
}

// slashTokenFault explains a token that carries a "/" and matched
// none of the shapes that use one. The head before the slash says
// which shape was meant, so the complaint can be about the argument
// rather than about the token as a whole.
func slashTokenFault(tok string, affixes map[string]lexicon.AffixEntry) error {
	head, arg, _ := strings.Cut(tok, "/")
	arg, _, _ = strings.Cut(arg, "_")
	switch {
	case head == "":
		return syntax(tok, "\"/\" binds an argument to a head, and there is nothing in front of it")
	case arg == "":
		return syntax(tok, "\"/\" binds an argument to a head, and there is nothing after it")
	}
	// A known affix or accessor family in front means the head was
	// right and the argument was not, which is the whole of the news.
	if resolveAffixCs(head, affixes) != "" {
		return badValue(tok, "degree", arg, degreeAdmits(arg))
	}
	if _, found := g.LookupAccessorKind(head, g.Type1Affix); found {
		return unlisted(tok, "case", arg)
	}
	return unlisted(tok, "affix", head)
}

// appendCaStack builds a Ca-stacking affix from the component list
// after "Ca:". The body is spelled exactly as a Slot VI Ca, so it is
// applied to a scratch Formative and its SlotVI read back — that way
// one set of component-name tables serves both, and a stacked Ca can
// never drift from the Slot VI spelling of the same complex.
func appendCaStack(f *g.Formative, body string, slotV bool) error {
	if body == "" {
		return syntax(caStackPrefix, "\"Ca:\" tags a stacked Ca and needs its components after it")
	}
	scratch := g.MinimalFormative("l")
	stacked := newAssigned()
	if body != caMarker {
		for _, part := range strings.Split(body, ".") {
			if part == "" {
				continue
			}
			// A stacked Ca is its own scope: its components are a
			// second Ca complex, not a second assignment to the Slot
			// VI one, so it gets a ledger of its own.
			if err := stacked.apply(&scratch, part); err != nil {
				return err
			}
		}
	}
	appendToAffixSlot(f, g.Affix{
		Type:      g.CaStackAffix,
		Consonant: allomorph.ConstructCa(scratch.SlotVI),
	}, slotV)
	return nil
}

// accessorToken matches "FAMILY/CASE" with the optional "_2"/"_3" Type
// suffix — a §3.9.2 case-accessor, inverse case-accessor or
// case-stacking affix. Only the three family names lead, so the shape
// cannot be confused with an ordinary "ABBREV/degree" affix, whose
// argument is a digit rather than a case.
var accessorToken = regexp.MustCompile(`^(ACC|IAC|CST)/([A-Z]{3})(?:_([23]))?$`)

// appendAccessor builds a §3.9.2 affix. The family and Type choose
// which of the fourteen Cs increments to write, and the case decides
// which half of the 68 it falls in and therefore which of that kind's
// two increments applies; the Vx then carries the case within the half.
func appendAccessor(f *g.Formative, family, caseName, typeStr string, slotV bool) error {
	kind, found := g.LookupAccessorKind(family, affixTypeFromSuffix(typeStr))
	if !found {
		return badValue(family, "affix type", typeStr,
			"§3.9.2 gives "+family+" no Type-"+typeStr+" form")
	}
	c, ok := parseCaseName(caseName)
	if !ok {
		return unlisted(caseName, "case", caseName)
	}
	series, degree, high, ok := g.AccessorVx(c)
	if !ok {
		return badValue(caseName, "case", caseName,
			"§3.9.2 gives no case-accessor increment for "+caseName)
	}
	atype, ok := g.SeriesAffixType(series)
	if !ok {
		return badValue(caseName, "case", caseName,
			fmt.Sprintf("a case-accessor Vx writes vowel series 1-3, and %s falls in series %d", caseName, series))
	}
	appendToAffixSlot(f, g.Affix{
		Type:      atype,
		Degree:    degree,
		Consonant: g.AccessorCs(kind, high),
	}, slotV)
	return nil
}

// column4AffixToken matches "(refs)/CASE" — §4.6.5's Column-4
// shortcut, a referential in one of the nine Transrelative cases. A
// case is three uppercase letters where a Type-3 degree is one digit,
// so the two token shapes cannot overlap.
var column4AffixToken = regexp.MustCompile(`^\(([^)]+)\)/([A-Z]{3})$`)

// type3AffixToken matches "(refs)/degree" — Type-3 referential affix.
var type3AffixToken = regexp.MustCompile(`^\(([^)]+)\)/([0-9])$`)

func appendType3Affix(f *g.Formative, tok string, slotV bool) error {
	atype, refSpec, value := g.Type3Affix, "", ""
	if m := type3AffixToken.FindStringSubmatch(tok); m != nil {
		refSpec, value = m[1], m[2]
	} else if m := column4AffixToken.FindStringSubmatch(tok); m != nil {
		atype, refSpec, value = g.Column4Affix, m[1], m[2]
	} else {
		return syntax(tok, "a Type-3 referential affix is written (refs)/degree (§4.6.5)")
	}

	degree, _ := strconv.Atoi(value)
	if atype == g.Column4Affix {
		c, ok := parseCaseName(value)
		if !ok {
			return unlisted(value, "case", value)
		}
		d, ok := g.TransrelativeDegree(c)
		if !ok {
			return badValue(value, "case", value,
				"§4.6.5's Column-4 shortcut reaches the nine Transrelative cases, and "+value+" is not one of them")
		}
		degree = d
	}

	var c1 strings.Builder
	for _, part := range strings.Split(refSpec, "+") {
		ref, eff, err := parseRefSpec(part)
		if err != nil {
			return err
		}
		c1.WriteString(parse.RefC1(g.PersonalRef{Referent: ref, Effect: eff}))
	}
	appendToAffixSlot(f, g.Affix{
		Type: atype, Degree: degree, Consonant: c1.String(),
	}, slotV)
	return nil
}

// affixTypeFromSuffix decodes the "_2"/"_3" Type suffix. Type 1 is
// unmarked, so an absent suffix and an explicit "1" both give it.
func affixTypeFromSuffix(s string) g.AffixType {
	switch s {
	case "2":
		return g.Type2Affix
	case "3":
		return g.Type3Affix
	}
	return g.Type1Affix
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
	atype := affixTypeFromSuffix(typeStr)
	cs := resolveAffixCs(csOrAbbrev, affixes)
	if cs == "" {
		return unlisted(csOrAbbrev, "affix", csOrAbbrev)
	}
	// resolveAffixCs hands back anything it cannot look up, which is
	// how "zzzz/3" composed to "malezzzza" (a triple consonant) and,
	// with no lexicon to resolve abbreviations against, "DEV/3" to
	// "maleDEV" — literal Latin capitals in an Ithkuil word, which
	// read back as an unrelated SCS/3-P08/3.
	if err := validateCluster("affix", cs); err != nil {
		return err
	}
	appendToAffixSlot(f, g.Affix{Type: atype, Degree: degree, Consonant: cs}, slotV)
	return nil
}

// resolveAffixCs returns the Cs cluster for a Slot VII affix
// identifier. Accepts the cluster itself, the all-caps abbreviation
// (looked up by .Abbrev), or any unknown lowercase cluster (the
// lexicon is a named subset, not the authoritative list of legal Cs
// clusters). An unknown cluster is folded through phonology.FromASCII,
// mirroring the root: the canonical gloss writes it in ASCII digraphs
// so that it stays typable, and this is what reads that back.
func resolveAffixCs(id string, affixes map[string]lexicon.AffixEntry) string {
	if affixes == nil {
		// Only the lexicon can turn an abbreviation into a Cs, so an
		// all-uppercase identifier has to fail here rather than be
		// folded to a literal cluster. "DEV/3" without a lexicon
		// composed to "maleDEV" — Latin capitals inside an Ithkuil
		// word — which reads back as an unrelated SCS/3-P08/3. The
		// lexicon path already rejects an unknown abbreviation; this
		// makes the no-lexicon path agree.
		if id == strings.ToUpper(id) && id != strings.ToLower(id) {
			return ""
		}
		return phonology.FromASCII(id)
	}
	if _, ok := affixes[id]; ok {
		return id
	}
	// Abbreviation lookup only fires for all-uppercase identifiers.
	//
	// SPT is the one abbreviation two C_S forms answer to. Quijada's
	// affix document gives the entry as "-rw/-ry SPT Specified Points
	// in Calendrical Time" with a single degree list, and §6.0 repeats
	// the pairing, so the affix really does have two consonant forms
	// for one meaning and nothing anywhere says which to use. That is
	// odd for its family — fourteen other C_S pairs differ only in a
	// final -w against -y, and in every one the two are distinct
	// affixes (CYC/CYL, ITE/ILT, VMA/VMB, and ten positional pairs on
	// the -Z/+Z contrast) — but it is what both sources print.
	//
	// The lowest cluster wins, so composing SPT is deterministic. A map
	// walk is not: Go randomizes it, so this returned rw or ry by
	// coin-flip and one Formative had two canonical spellings.
	if id == strings.ToUpper(id) {
		best := ""
		for cs, a := range affixes {
			if a.Abbrev == id && (best == "" || cs < best) {
				best = cs
			}
		}
		return best
	}
	// Lowercase/mixed cluster not in the lexicon — accept it, folding
	// any ASCII digraphs back to their Ithkuil glyphs.
	return phonology.FromASCII(id)
}

// isClusterToken returns (cluster, true) if tok looks like an Ithkuil
// root cluster — that is, a single token (no "/" or ":"), at least
// one character is a lowercase ASCII letter or an Ithkuil special
// orthographic glyph. ASCII digraphs are folded via phonology.FromASCII.
//
// All-caps tokens (with optional digits) are treated as abbreviations
// and rejected here so they flow through ApplyFlag instead.
func isClusterToken(tok string) (string, bool) {
	if tok == "" || tok == caMarker || strings.ContainsAny(tok, "/:") {
		return "", false
	}
	// Other toolkits write a CsRoot as a bare "(b)". Ours always
	// carries the degree, "(ABBREV)/N", so a parenthesised token is
	// never a plain cluster and the foreign variant is not accepted.
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
	return phonology.FromASCII(tok), true
}
