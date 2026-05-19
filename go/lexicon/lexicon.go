// Package lexicon loads Ithkuil V4 roots and affixes from JSON and
// exposes lookup by consonant form. Both data files live under data/
// at the repo root (data/roots.json, data/affixes.json).
package lexicon

import (
	"encoding/json"
	"fmt"
	"os"
)

// RootEntry pairs a root consonant cluster with its four-stem meaning.
// Stem 0 is the basic/generic meaning; stems 1-3 are specializations
// selected by the formative's Vv slot.
type RootEntry struct {
	Cr    string `json:"cr"`
	Stem0 string `json:"stem0"`
	Stem1 string `json:"stem1"`
	Stem2 string `json:"stem2"`
	Stem3 string `json:"stem3"`
}

// AffixEntry holds an affix identifier, its three-letter abbreviation,
// its short description, type code, and the 9 degree-specific meanings.
type AffixEntry struct {
	Cs          string   `json:"cs"`
	Abbrev      string   `json:"abbrev"`
	Description string   `json:"description"`
	Type        string   `json:"type"`
	Degrees     []string `json:"degrees"`
}

// Lexicon bundles the root and affix maps so callers can pass a single
// value through their pipelines.
type Lexicon struct {
	Roots   map[string]RootEntry
	Affixes map[string]AffixEntry
}

// LoadRoots reads and parses a roots.json file.
func LoadRoots(path string) (map[string]RootEntry, error) {
	var entries []RootEntry
	if err := readJSON(path, &entries); err != nil {
		return nil, fmt.Errorf("load roots: %w", err)
	}
	m := make(map[string]RootEntry, len(entries))
	for _, e := range entries {
		m[e.Cr] = e
	}
	return m, nil
}

// LoadAffixes reads and parses an affixes.json file.
func LoadAffixes(path string) (map[string]AffixEntry, error) {
	var entries []AffixEntry
	if err := readJSON(path, &entries); err != nil {
		return nil, fmt.Errorf("load affixes: %w", err)
	}
	m := make(map[string]AffixEntry, len(entries))
	for _, e := range entries {
		m[e.Cs] = e
	}
	return m, nil
}

// Load reads both lexicon files from the given paths.
func Load(rootsPath, affixesPath string) (*Lexicon, error) {
	roots, err := LoadRoots(rootsPath)
	if err != nil {
		return nil, err
	}
	affixes, err := LoadAffixes(affixesPath)
	if err != nil {
		return nil, err
	}
	return &Lexicon{Roots: roots, Affixes: affixes}, nil
}

// Stem selects the meaning string for a given stem index (0-3).
// Out-of-range indices fall back to Stem0.
func (r RootEntry) Stem(i int) string {
	switch i {
	case 1:
		return r.Stem1
	case 2:
		return r.Stem2
	case 3:
		return r.Stem3
	default:
		return r.Stem0
	}
}

func readJSON(path string, v any) error {
	data, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	return json.Unmarshal(data, v)
}
