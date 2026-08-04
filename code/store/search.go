package store

import "github.com/christian-oudard/ithkuil/lexicon"

// Searcher adapts a Store to the lexicon-search interface the api
// package declares. It exists so that api can be given the full-text
// index without importing this package: api must stay linkable for
// js/wasm, where the SQLite driver has no build at all.
type Searcher struct{ s *Store }

// NewSearcher wraps a store. A nil store gives a searcher that answers
// nothing rather than panicking, which is the state the MCP server runs
// in when the data file is missing.
func NewSearcher(s *Store) Searcher { return Searcher{s} }

func (x Searcher) SearchRoots(query string, limit int) ([]lexicon.RootEntry, error) {
	if x.s == nil {
		return nil, nil
	}
	hits, err := x.s.SearchRoots(query, limit)
	if err != nil {
		return nil, err
	}
	out := make([]lexicon.RootEntry, len(hits))
	for i, e := range hits {
		out[i] = lexicon.RootEntry{
			Cr: e.Cr, Stem0: e.Stem0, Stem1: e.Stem1, Stem2: e.Stem2,
			Stem3: e.Stem3, Contential: e.Contential,
			Constitutive: e.Constitutive, Objective: e.Objective,
			Completive: e.Completive, Dynamic: e.Dynamic,
			Wikidata: e.Wikidata,
		}
	}
	return out, nil
}

func (x Searcher) SearchAffixes(query string, limit int) ([]lexicon.AffixEntry, error) {
	if x.s == nil {
		return nil, nil
	}
	hits, err := x.s.SearchAffixes(query, limit)
	if err != nil {
		return nil, err
	}
	out := make([]lexicon.AffixEntry, len(hits))
	for i, e := range hits {
		out[i] = lexicon.AffixEntry{
			Cs: e.Cs, Abbrev: e.Abbrev, Description: e.Description,
			Type: e.Type, Degrees: e.Degrees,
		}
	}
	return out, nil
}
