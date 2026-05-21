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
	"regexp"

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
// value through their pipelines. Version is a content-derived short
// hash that lets downstream consumers (notably the binary serialize
// format) detect when the lexicon has changed.
type Lexicon struct {
	Version string
	Roots   map[string]RootEntry
	Affixes map[string]AffixEntry
}

// LoadDefault returns the lexicon bundled with the binary via
// //go:embed. Use Load to override with a different on-disk copy.
func LoadDefault() (*Lexicon, error) {
	return parseLexicon(data.Lexicon)
}

// Load reads a combined lexicon.json file (with version, roots, and
// affixes fields) from disk.
func Load(path string) (*Lexicon, error) {
	bytes, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("load lexicon: %w", err)
	}
	return parseLexicon(bytes)
}

// Category-valued affixes (MCS, PHS, AP1-4, IVL, LVL, VAL) write their
// degree descriptions as "(CODE) Full Name", where CODE is the 2-4
// char canonical abbreviation the gloss should surface instead of the
// degree number. IVL is the one affix where type 1 and type 2 carry
// different category values per degree, written as
// "(ASR) Assertive [(OBS) Observational₂]" — the bracketed alternate
// is the type-2 reading.
var (
	categoryPrefix = regexp.MustCompile(`^\(([A-Z0-9]{2,4})\)`)
	categoryType2  = regexp.MustCompile(`\[\(([A-Z0-9]{2,4})\)[^]]*\]`)
)

// CategoryValue returns the canonical category code for the given
// degree (1-9) and affix type (1, 2, or 3). Returns "" when the affix
// is not category-valued, the degree is out of range, or the requested
// type has no alternate reading.
func (a AffixEntry) CategoryValue(degree int, affixType int) string {
	if degree < 1 || degree > len(a.Degrees) {
		return ""
	}
	text := a.Degrees[degree-1]
	switch affixType {
	case 1:
		if m := categoryPrefix.FindStringSubmatch(text); m != nil {
			return m[1]
		}
	case 2:
		if m := categoryType2.FindStringSubmatch(text); m != nil {
			return m[1]
		}
	}
	return ""
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

// parseLexicon decodes the combined lexicon.json shape.
func parseLexicon(buf []byte) (*Lexicon, error) {
	var raw struct {
		Version string       `json:"version"`
		Roots   []RootEntry  `json:"roots"`
		Affixes []AffixEntry `json:"affixes"`
	}
	if err := json.Unmarshal(buf, &raw); err != nil {
		return nil, fmt.Errorf("lexicon: %w", err)
	}
	if raw.Version == "" {
		return nil, fmt.Errorf("lexicon: missing version field")
	}
	roots := make(map[string]RootEntry, len(raw.Roots))
	for _, e := range raw.Roots {
		roots[e.Cr] = e
	}
	affixes := make(map[string]AffixEntry, len(raw.Affixes))
	for _, e := range raw.Affixes {
		affixes[e.Cs] = e
	}
	return &Lexicon{Version: raw.Version, Roots: roots, Affixes: affixes}, nil
}
