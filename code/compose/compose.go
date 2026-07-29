// Package compose provides reverse-lookup tooling on the grammar
// inventory. Given an abbreviation ("THM") or a written form ("a") or
// a meaning keyword ("agent"), find the grammar entries or roots that
// match.
package compose

import (
	"fmt"
	"sort"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/parse"
)

// Entry is one row of the grammar reverse-lookup table.
type Entry struct {
	Category    string // e.g. "Case", "Aspect", "Configuration"
	Abbrev      string // e.g. "THM", "RTR"
	Name        string // canonical English name ("Thematic", "Retrospective")
	Form        string // written vowel/consonant form, when available
	Description string // additional gloss / expression (used for Bias)
}

// Table is the full grammar inventory exposed as Entry rows. Built
// once at package init time from the grammar package's AllX slices
// and the canonical encoder functions.
var Table []Entry

func init() {
	// Cases.
	for _, c := range g.AllCases {
		Table = append(Table, Entry{
			Category: "Case/" + c.Group().String(),
			Abbrev:   c.String(),
			Form:     parse.CaseToVc(c),
		})
	}
	// Stem.
	for _, s := range []g.Stem{g.S1, g.S2, g.S3, g.S0} {
		Table = append(Table, Entry{Category: "Stem", Abbrev: s.String()})
	}
	// Version.
	for _, v := range []g.Version{g.PRC, g.CPT} {
		Table = append(Table, Entry{Category: "Version", Abbrev: v.String()})
	}
	// Function / Specification / Context.
	for _, f := range []g.Function{g.STA, g.DYN} {
		Table = append(Table, Entry{Category: "Function", Abbrev: f.String()})
	}
	for _, s := range []g.Specification{g.BSC, g.CTE, g.CSV, g.OBJ} {
		Table = append(Table, Entry{Category: "Specification", Abbrev: s.String()})
	}
	for _, c := range []g.Context{g.EXS, g.FNC, g.RPS, g.AMG} {
		Table = append(Table, Entry{Category: "Context", Abbrev: c.String()})
	}
	// Ca components.
	for _, c := range g.AllConfigurations {
		Table = append(Table, Entry{Category: "Configuration", Abbrev: c.String()})
	}
	for _, a := range g.AllAffiliations {
		Table = append(Table, Entry{Category: "Affiliation", Abbrev: a.String()})
	}
	for _, p := range g.AllPerspectives {
		Table = append(Table, Entry{Category: "Perspective", Abbrev: p.String()})
	}
	for _, e := range g.AllExtensions {
		Table = append(Table, Entry{Category: "Extension", Abbrev: e.String()})
	}
	for _, e := range g.AllEssences {
		Table = append(Table, Entry{Category: "Essence", Abbrev: e.String()})
	}
	// Verbal categories.
	for _, v := range g.AllValences {
		Table = append(Table, Entry{Category: "Valence", Abbrev: v.String()})
	}
	for _, p := range g.AllPhases {
		Table = append(Table, Entry{Category: "Phase", Abbrev: p.String()})
	}
	for _, e := range g.AllEffects {
		Table = append(Table, Entry{Category: "Effect", Abbrev: e.String()})
	}
	for _, l := range g.AllLevels {
		Table = append(Table, Entry{Category: "Level", Abbrev: l.String()})
	}
	for _, a := range g.AllAspects {
		Table = append(Table, Entry{Category: "Aspect", Abbrev: a.String()})
	}
	for _, m := range g.AllMoods {
		Table = append(Table, Entry{Category: "Mood", Abbrev: m.String()})
	}
	for _, cs := range g.AllCaseScopes {
		Table = append(Table, Entry{Category: "CaseScope", Abbrev: cs.String()})
	}
	for _, v := range g.AllVk {
		Table = append(Table, Entry{Category: "Illocution", Abbrev: v.Tag()})
	}
	for _, v := range g.AllValidations {
		Table = append(Table, Entry{Category: "Validation", Abbrev: v.String()})
	}
	// Bias and Register.
	for _, b := range g.AllBiases {
		Table = append(Table, Entry{
			Category:    "Bias",
			Abbrev:      b.String(),
			Form:        parse.BiasForm(b),
			Description: g.BiasExpression(b),
		})
	}
	for _, r := range g.AllRegisters {
		f := parse.RegisterInitialForm(r)
		Table = append(Table, Entry{Category: "Register", Abbrev: r.String(), Form: f})
	}
	for _, ct := range g.AllCarrierTypes {
		Table = append(Table, Entry{
			Category: "CarrierType",
			Abbrev:   ct.String(),
			Form:     parse.CarrierTypeForm(ct),
		})
	}
	// Fill canonical Name on every entry from grammar.Name. Bias rows
	// already carry the human expression in Description (e.g. "Please"
	// for SOL); other rows leave Description empty.
	for i := range Table {
		Table[i].Name = g.Name(Table[i].Abbrev)
	}
}

// LookupGrammar returns every entry whose Abbrev is an exact case-
// insensitive match for query.
func LookupGrammar(query string) []Entry {
	q := strings.ToUpper(query)
	var out []Entry
	for _, e := range Table {
		if strings.ToUpper(e.Abbrev) == q {
			out = append(out, e)
		}
	}
	return out
}

// SearchGrammar returns entries whose Abbrev, Category, or Form
// contains the query substring (case-insensitive). Exact Abbrev
// matches sort first.
func SearchGrammar(query string) []Entry {
	q := strings.ToLower(query)
	var exact, fuzzy []Entry
	for _, e := range Table {
		if strings.EqualFold(e.Abbrev, q) {
			exact = append(exact, e)
			continue
		}
		if strings.Contains(strings.ToLower(e.Abbrev), q) ||
			strings.Contains(strings.ToLower(e.Category), q) ||
			(e.Form != "" && strings.Contains(strings.ToLower(e.Form), q)) ||
			(e.Name != "" && strings.Contains(strings.ToLower(e.Name), q)) ||
			(e.Description != "" && strings.Contains(strings.ToLower(e.Description), q)) {
			fuzzy = append(fuzzy, e)
		}
	}
	return append(exact, fuzzy...)
}

// LookupForm returns every entry whose written Form equals form.
// Useful for "what grammar values does this vowel encode?" queries.
func LookupForm(form string) []Entry {
	var out []Entry
	for _, e := range Table {
		if e.Form == form {
			out = append(out, e)
		}
	}
	return out
}

// Filter returns inventory entries matching the optional category
// and query. Category match is case-insensitive: "Case" matches
// "Case/THM" and "Case/ABS" too. Empty cat means "all categories".
// When exact, query must equal Abbrev (case-insensitive). When not
// exact, query is a substring against Abbrev, Category, Form, and
// Description. Empty query means "no query filter".
func Filter(cat, query string, exact bool) []Entry {
	catL := strings.ToLower(cat)
	qL := strings.ToLower(query)
	var out []Entry
	for _, e := range Table {
		if catL != "" {
			ec := strings.ToLower(e.Category)
			if ec != catL && !strings.HasPrefix(ec, catL+"/") {
				continue
			}
		}
		if qL != "" {
			if exact {
				if !strings.EqualFold(e.Abbrev, qL) {
					continue
				}
			} else if !strings.Contains(strings.ToLower(e.Abbrev), qL) &&
				!strings.Contains(strings.ToLower(e.Category), qL) &&
				(e.Form == "" || !strings.Contains(strings.ToLower(e.Form), qL)) &&
				(e.Name == "" || !strings.Contains(strings.ToLower(e.Name), qL)) &&
				(e.Description == "" || !strings.Contains(strings.ToLower(e.Description), qL)) {
				continue
			}
		}
		out = append(out, e)
	}
	return out
}

// Categories returns the distinct top-level category names (the part
// before any "/") present in Table, in alphabetical order.
func Categories() []string {
	seen := map[string]struct{}{}
	for _, e := range Table {
		top := e.Category
		if i := strings.Index(top, "/"); i >= 0 {
			top = top[:i]
		}
		seen[top] = struct{}{}
	}
	out := make([]string, 0, len(seen))
	for k := range seen {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

// RootHit pairs a root cluster with its lexicon entry and a relevance
// score (lower = better). Score 0 is a direct Cr match.
type RootHit struct {
	Score int
	Cr    string
	Entry lexicon.RootEntry
}

// SearchRoots returns root entries whose Cr or any stem meaning
// contains the query substring. Direct Cr hits get score 0; stem
// matches get progressively higher scores by stem index. Results are
// sorted ascending by (score, Cr).
func SearchRoots(query string, roots map[string]lexicon.RootEntry) []RootHit {
	q := strings.ToLower(strings.Trim(query, "-"))
	if q == "" {
		return nil
	}
	var hits []RootHit
	for cr, entry := range roots {
		if strings.EqualFold(cr, q) {
			hits = append(hits, RootHit{Score: 0, Cr: cr, Entry: entry})
			continue
		}
		// Stem priority: S1 > S2 > S3 > S0.
		stems := []string{entry.Stem1, entry.Stem2, entry.Stem3, entry.Stem0}
		for i, s := range stems {
			if strings.Contains(strings.ToLower(s), q) {
				hits = append(hits, RootHit{Score: i + 1, Cr: cr, Entry: entry})
				break
			}
		}
	}
	sort.SliceStable(hits, func(i, j int) bool {
		if hits[i].Score != hits[j].Score {
			return hits[i].Score < hits[j].Score
		}
		if len(hits[i].Cr) != len(hits[j].Cr) {
			return len(hits[i].Cr) < len(hits[j].Cr)
		}
		return hits[i].Cr < hits[j].Cr
	})
	return hits
}

// SearchAffixes returns affix entries whose Cs or any degree gloss
// contains query (case-insensitive substring).
func SearchAffixes(query string, affixes map[string]lexicon.AffixEntry) []lexicon.AffixEntry {
	q := strings.ToLower(strings.Trim(query, "-"))
	if q == "" {
		return nil
	}
	var hits []lexicon.AffixEntry
	for cs, a := range affixes {
		if strings.EqualFold(cs, q) ||
			strings.EqualFold(a.Abbrev, q) ||
			strings.Contains(strings.ToLower(a.Description), q) {
			hits = append(hits, a)
			continue
		}
		for _, d := range a.Degrees {
			if strings.Contains(strings.ToLower(d), q) {
				hits = append(hits, a)
				break
			}
		}
	}
	sort.SliceStable(hits, func(i, j int) bool { return hits[i].Cs < hits[j].Cs })
	return hits
}

// concatPrefix matches the gloss-format concatenation marker: "T1"
// or "T2". Slot I in the gloss writes a Type-1 chain dependent as
// "T1-" and a Type-2 as "T2-".
var concatPrefix = map[string]g.ConcatenationStatus{
	"T1": g.Type1,
	"T2": g.Type2,
}

// ApplyFlag mutates f according to one grammar-abbreviation flag like
// "S2", "DYN", "OBJ", "ERG", "RTR", "PEN". Case-insensitive. Returns
// an error for unrecognized flags.
//
// Recognized flag families:
//
//	S0..S3                Stem
//	PRC | CPT             Version
//	STA | DYN             Function
//	BSC | CTE | CSV | OBJ Specification
//	EXS | FNC | RPS | AMG Context
//	MON | PEN | ULT | ANT Stress
//	<Case>                Slot IX case (any of 68)
//	<Aspect>              Slot VIII aspect (with CCN case-scope)
//	<Valence>             Slot VIII valence (with FAC mood)
//	<Mood>                Slot VIII mood (wraps existing Slot VIII)
//	<Illocution>          Slot IX illocution (forces ultimate stress)
func ApplyFlag(f *g.Formative, flag string) error {
	flag = strings.ToUpper(flag)

	// Concatenation status.
	if c, ok := concatPrefix[flag]; ok {
		f.Concat = c
		return nil
	}

	// Stem (CrRoot only).
	switch flag {
	case "S0", "S1", "S2", "S3":
		stem := map[string]g.Stem{"S0": g.S0, "S1": g.S1, "S2": g.S2, "S3": g.S3}[flag]
		cr, ok := f.Root.(g.CrRoot)
		if !ok {
			return fmt.Errorf("stem %s only applies to CrRoot formatives", flag)
		}
		cr.Stem = stem
		f.Root = cr
		return nil
	}

	// Version (CrRoot / CsRoot / RefRoot).
	switch flag {
	case "PRC", "CPT":
		v := g.PRC
		if flag == "CPT" {
			v = g.CPT
		}
		switch r := f.Root.(type) {
		case g.CrRoot:
			r.Version = v
			f.Root = r
		case g.CsRoot:
			r.Version = v
			f.Root = r
		case g.RefRoot:
			r.Version = v
			f.Root = r
		}
		return nil
	}

	// Function.
	switch flag {
	case "STA", "DYN":
		fn := g.STA
		if flag == "DYN" {
			fn = g.DYN
		}
		switch r := f.Root.(type) {
		case g.CrRoot:
			r.SlotIV.Function = fn
			f.Root = r
		case g.CsRoot:
			r.Function = fn
			f.Root = r
		case g.RefRoot:
			r.SlotIV.Function = fn
			f.Root = r
		}
		return nil
	}

	// Specification (CrRoot / RefRoot — CsRoot is implicitly BSC).
	switch flag {
	case "BSC", "CTE", "CSV", "OBJ":
		s := map[string]g.Specification{"BSC": g.BSC, "CTE": g.CTE, "CSV": g.CSV, "OBJ": g.OBJ}[flag]
		switch r := f.Root.(type) {
		case g.CrRoot:
			r.SlotIV.Specification = s
			f.Root = r
		case g.RefRoot:
			r.SlotIV.Specification = s
			f.Root = r
		default:
			return fmt.Errorf("specification %s only applies to CrRoot/RefRoot", flag)
		}
		return nil
	}

	// Context.
	switch flag {
	case "EXS", "FNC", "RPS", "AMG":
		c := map[string]g.Context{"EXS": g.EXS, "FNC": g.FNC, "RPS": g.RPS, "AMG": g.AMG}[flag]
		switch r := f.Root.(type) {
		case g.CrRoot:
			r.SlotIV.Context = c
			f.Root = r
		case g.CsRoot:
			r.Context = c
			f.Root = r
		case g.RefRoot:
			r.SlotIV.Context = c
			f.Root = r
		}
		return nil
	}

	// Stress is encoded in the Final variant. PEN/MON → nominal,
	// ANT → framed verbal, ULT → verbal (default Assertive/OBS).
	switch flag {
	case "PEN", "MON":
		f.Final = g.UnframedNominal{Case: currentCase(f.Final)}
		return nil
	case "ANT":
		f.Final = g.FramedVerbal{Case: currentCase(f.Final)}
		return nil
	case "ULT":
		if _, ok := f.Final.(g.UnframedVerbal); !ok {
			f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
		}
		return nil
	}

	// Case (any of 68): nominal or framed-nominal Final.
	for _, c := range g.AllCases {
		if c.String() == flag {
			if _, framed := f.Final.(g.FramedVerbal); framed {
				f.Final = g.FramedVerbal{Case: c}
			} else {
				f.Final = g.UnframedNominal{Case: c}
			}
			return nil
		}
	}

	// Aspect.
	for _, a := range g.AllAspects {
		if a.String() == flag {
			f.SlotVIII = g.VnCnAspect{Aspect: a, MoodScope: g.FAC}
			return nil
		}
	}

	// Valence.
	for _, v := range g.AllValences {
		if v.String() == flag {
			f.SlotVIII = g.VnCnValence{Valence: v, MoodScope: g.FAC}
			return nil
		}
	}

	// Phase.
	for _, p := range g.AllPhases {
		if p.String() == flag {
			f.SlotVIII = g.VnCnPhase{Phase: p, MoodScope: g.FAC}
			return nil
		}
	}

	// Effect.
	for _, e := range g.AllEffects {
		if e.String() == flag {
			f.SlotVIII = g.VnCnEffect{Effect: e, MoodScope: g.FAC}
			return nil
		}
	}

	// Level.
	for _, lv := range g.AllLevels {
		if lv.String() == flag {
			f.SlotVIII = g.VnCnLevel{Level: lv, MoodScope: g.FAC}
			return nil
		}
	}

	// CaseScope: same underlying field as Mood (Cn position encodes
	// both). Map to the Mood counterpart via CaseScopeToMood.
	for _, c := range g.AllCaseScopes {
		if c.String() == flag {
			return applyMoodScope(f, g.CaseScopeToMood(c))
		}
	}

	// Mood: replaces MoodScope on whatever SlotVIII variant is there.
	for _, m := range g.AllMoods {
		if m.String() == flag {
			return applyMoodScope(f, m)
		}
	}

	// Illocution: forces UnframedVerbal Final.
	if vk, ok := illocutionByName(flag); ok {
		f.Final = g.UnframedVerbal{Vk: vk}
		return nil
	}

	// Validation: only meaningful on Assertive illocution. Replace
	// the Vk if the current Final is already Assertive; otherwise
	// promote to UnframedVerbal{Assertive{Validation: v}}.
	for _, v := range g.AllValidations {
		if v.String() == flag {
			f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: v}}
			return nil
		}
	}

	// Ca components (Affiliation / Configuration / Extension /
	// Perspective / Essence). Each enum is disjoint so dispatch by
	// matching the abbreviation against every enum's String().
	if applyCaFlag(f, flag) {
		return nil
	}

	return fmt.Errorf("unknown grammar flag %q", flag)
}

// applyMoodScope sets the MoodScope on the existing SlotVIII variant,
// or creates a Valence-MNO SlotVIII to carry it when absent.
func applyMoodScope(f *g.Formative, m g.Mood) error {
	switch s := f.SlotVIII.(type) {
	case g.VnCnValence:
		s.MoodScope = m
		f.SlotVIII = s
	case g.VnCnAspect:
		s.MoodScope = m
		f.SlotVIII = s
	case g.VnCnPhase:
		s.MoodScope = m
		f.SlotVIII = s
	case g.VnCnEffect:
		s.MoodScope = m
		f.SlotVIII = s
	case g.VnCnLevel:
		s.MoodScope = m
		f.SlotVIII = s
	default:
		f.SlotVIII = g.VnCnValence{Valence: g.MNO, MoodScope: m}
	}
	return nil
}

// applyCaFlag tries to interpret flag as one of the five Ca-complex
// abbreviations and mutate f.SlotVI accordingly. Returns true if the
// flag matched a Ca component.
func applyCaFlag(f *g.Formative, flag string) bool {
	for _, c := range g.AllConfigurations {
		if c.String() == flag {
			f.SlotVI.Configuration = c
			return true
		}
	}
	for _, a := range g.AllAffiliations {
		if a.String() == flag {
			f.SlotVI.Affiliation = a
			return true
		}
	}
	for _, e := range g.AllExtensions {
		if e.String() == flag {
			f.SlotVI.Extension = e
			return true
		}
	}
	for _, p := range g.AllPerspectives {
		if p.String() == flag {
			f.SlotVI.Perspective = p
			return true
		}
	}
	for _, e := range g.AllEssences {
		if e.String() == flag {
			f.SlotVI.Essence = e
			return true
		}
	}
	return false
}

// currentCase pulls the Case out of a nominal/framed-verbal Final, or
// returns THM when the Final is verbal.
func currentCase(fin g.Final) g.Case {
	switch v := fin.(type) {
	case g.UnframedNominal:
		return v.Case
	case g.FramedVerbal:
		return v.Case
	}
	return g.THM
}

// illocutionByName returns the Vk variant for a 3-letter illocution
// abbreviation.
func illocutionByName(name string) (g.Vk, bool) {
	switch name {
	case "ASR":
		return g.Assertive{Validation: g.OBS}, true
	case "DIR":
		return g.Directive{}, true
	case "DEC":
		return g.Declarative{}, true
	case "IRG":
		return g.Interrogative{}, true
	case "VER":
		return g.Verificative{}, true
	case "ADM":
		return g.Admonitive{}, true
	case "POT":
		return g.Potentiative{}, true
	case "HOR":
		return g.Hortative{}, true
	case "CNJ":
		return g.Conjectural{}, true
	}
	return nil, false
}
