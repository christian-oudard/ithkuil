// Package lexicon loads Ithkuil V4 roots and affixes from JSON and
// exposes lookup by consonant form. The canonical data files live
// under data/ at the repo root and are also embedded into binaries
// via the github.com/christian-oudard/ithkuil/data package — use
// LoadDefault for the embedded copy or Load to read from disk.
package lexicon

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/christian-oudard/ithkuil/data"
)

// RootEntry pairs a root consonant cluster with its four-stem meaning
// plus optional specialization variants. Stem 0 is the basic/generic
// meaning; stems 1-3 are specializations selected by the formative's
// Vv slot.
//
// The optional fields capture cross-slot alternates from the upstream
// spreadsheet:
//
//   - Contential, Constitutive: alternate stem-0 meanings for spec=CTE
//     and spec=CSV. Stem0 itself is the BSC (basic) reading.
//   - Objective: per-stem (S1..S3) alternates for spec=OBJ. Three-slot
//     slice when present.
//   - Completive: per-stem alternates for completion=CPT. PRC reads
//     Stem(n) as usual.
//   - Dynamic: alternate reading when function=DYN.
//   - Wikidata: external Q-IDs per stem (S1..S3).
//
// Most roots only fill Stem0..Stem3. The variants are populated for
// well-defined word families (~1-4% of entries) and are omitted when
// blank to keep the JSON compact.
type RootEntry struct {
	Cr           string   `json:"cr"`
	Stem0        string   `json:"stem0"`
	Stem1        string   `json:"stem1"`
	Stem2        string   `json:"stem2"`
	Stem3        string   `json:"stem3"`
	Contential   string   `json:"contential,omitempty"`
	Constitutive string   `json:"constitutive,omitempty"`
	Objective    []string `json:"objective,omitempty"`
	Completive   []string `json:"completive,omitempty"`
	Dynamic      string   `json:"dynamic,omitempty"`
	Wikidata     []string `json:"wikidata,omitempty"`
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

// LoadDefault returns the lexicon bundled with the binary via
// //go:embed. Use Load to override with a different on-disk copy.
func LoadDefault() (*Lexicon, error) {
	roots, err := parseRoots(data.Roots)
	if err != nil {
		return nil, fmt.Errorf("embedded roots: %w", err)
	}
	affixes, err := parseAffixes(data.Affixes)
	if err != nil {
		return nil, fmt.Errorf("embedded affixes: %w", err)
	}
	return &Lexicon{Roots: roots, Affixes: affixes}, nil
}

// LoadRoots reads and parses a roots.json file.
func LoadRoots(path string) (map[string]RootEntry, error) {
	bytes, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("load roots: %w", err)
	}
	return parseRoots(bytes)
}

// LoadAffixes reads and parses an affixes.json file.
func LoadAffixes(path string) (map[string]AffixEntry, error) {
	bytes, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("load affixes: %w", err)
	}
	return parseAffixes(bytes)
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

func parseRoots(bytes []byte) (map[string]RootEntry, error) {
	var entries []RootEntry
	if err := json.Unmarshal(bytes, &entries); err != nil {
		return nil, err
	}
	m := make(map[string]RootEntry, len(entries))
	for _, e := range entries {
		m[e.Cr] = e
	}
	return m, nil
}

func parseAffixes(bytes []byte) (map[string]AffixEntry, error) {
	var entries []AffixEntry
	if err := json.Unmarshal(bytes, &entries); err != nil {
		return nil, err
	}
	m := make(map[string]AffixEntry, len(entries))
	for _, e := range entries {
		m[e.Cs] = e
	}
	return m, nil
}
