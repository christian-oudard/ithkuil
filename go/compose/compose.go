// Package compose provides reverse-lookup tooling on the grammar
// inventory. Given an abbreviation ("THM") or a surface form ("a") or
// a meaning keyword ("agent"), find the grammar entries or roots that
// match.
//
// The Go port omits the hand-curated descriptions and glosses that
// live in the Haskell Compose module (200+ entries of free-text data
// entry); the lookup APIs work fine without them. If you need rich
// descriptions, layer them on top of the entries returned here.
package compose

import (
	"sort"
	"strings"

	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/lexicon"
)

// Entry is one row of the grammar reverse-lookup table.
type Entry struct {
	Category string // e.g. "Case", "Aspect", "Configuration"
	Abbrev   string // e.g. "THM", "RTR"
	Form     string // surface vowel/consonant form, when available
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
			Form:     g.CaseToVc(c),
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
			Category: "Bias",
			Abbrev:   b.String(),
			Form:     g.BiasForm(b),
		})
	}
	for _, r := range g.AllRegisters {
		f := g.RegisterInitialForm(r)
		Table = append(Table, Entry{Category: "Register", Abbrev: r.String(), Form: f})
	}
	for _, ct := range g.AllCarrierTypes {
		Table = append(Table, Entry{
			Category: "CarrierType",
			Abbrev:   ct.String(),
			Form:     g.CarrierTypeForm(ct),
		})
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
			(e.Form != "" && strings.Contains(strings.ToLower(e.Form), q)) {
			fuzzy = append(fuzzy, e)
		}
	}
	return append(exact, fuzzy...)
}

// LookupForm returns every entry whose surface Form equals form.
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
