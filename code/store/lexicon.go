package store

import (
	"fmt"

	"github.com/christian-oudard/ithkuil/lexicon"
)

// LoadLexicon reads all roots and affixes from an open store into
// an in-memory Lexicon. Use this when you need map-based access across
// many lookups (e.g. analysis of a full document). Version is left
// zero since the store does not record a version.
func LoadLexicon(s *Store) (*lexicon.Lexicon, error) {
	sr, err := s.AllRoots()
	if err != nil {
		return nil, fmt.Errorf("load roots from store: %w", err)
	}
	sa, err := s.AllAffixes()
	if err != nil {
		return nil, fmt.Errorf("load affixes from store: %w", err)
	}
	roots := make(map[string]lexicon.RootEntry, len(sr))
	for _, r := range sr {
		roots[r.Cr] = lexicon.RootEntry{
			Cr: r.Cr, Stem0: r.Stem0, Stem1: r.Stem1,
			Stem2: r.Stem2, Stem3: r.Stem3,
			Contential: r.Contential, Constitutive: r.Constitutive,
			Objective: r.Objective, Completive: r.Completive,
			Dynamic: r.Dynamic, Wikidata: r.Wikidata,
		}
	}
	affixes := make(map[string]lexicon.AffixEntry, len(sa))
	for _, a := range sa {
		affixes[a.Cs] = lexicon.AffixEntry{
			Cs: a.Cs, Abbrev: a.Abbrev, Description: a.Description,
			Type: a.Type, Degrees: a.Degrees,
		}
	}
	return &lexicon.Lexicon{Roots: roots, Affixes: affixes}, nil
}
