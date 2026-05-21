package lexicon

import (
	"path/filepath"
	"testing"
)

// dataPath returns the path to a file under the repo's data/ directory,
// resolved relative to this test file (lexicon/ → ../data/).
func dataPath(name string) string {
	return filepath.Join("..", "data", name)
}

func TestLoadRoots(t *testing.T) {
	roots, err := LoadRoots(dataPath("roots.json"))
	if err != nil {
		t.Fatalf("LoadRoots error: %v", err)
	}
	// CLAUDE.md notes 4717 entries.
	if len(roots) < 4000 {
		t.Errorf("expected ~4717 root entries, got %d", len(roots))
	}
	// Spot check: the root "b" exists and has stems populated.
	b, ok := roots["b"]
	if !ok {
		t.Fatalf("root \"b\" not found")
	}
	if b.Stem0 == "" || b.Stem1 == "" || b.Stem2 == "" || b.Stem3 == "" {
		t.Errorf("root \"b\" has empty stems: %+v", b)
	}
}

func TestLoadAffixes(t *testing.T) {
	affixes, err := LoadAffixes(dataPath("affixes.json"))
	if err != nil {
		t.Fatalf("LoadAffixes error: %v", err)
	}
	if len(affixes) < 400 {
		t.Errorf("expected several hundred affix entries, got %d", len(affixes))
	}
	// Spot check: "b" affix exists with 9 degrees.
	b, ok := affixes["b"]
	if !ok {
		t.Fatalf("affix \"b\" not found")
	}
	if b.Abbrev == "" {
		t.Errorf("affix \"b\" has no abbrev")
	}
	if len(b.Degrees) != 9 {
		t.Errorf("affix \"b\" degrees = %d, want 9", len(b.Degrees))
	}
}

func TestLoad(t *testing.T) {
	lex, err := Load(dataPath("roots.json"), dataPath("affixes.json"))
	if err != nil {
		t.Fatalf("Load error: %v", err)
	}
	if len(lex.Roots) == 0 || len(lex.Affixes) == 0 {
		t.Errorf("Load returned empty maps: roots=%d affixes=%d",
			len(lex.Roots), len(lex.Affixes))
	}
}

func TestRootStem(t *testing.T) {
	r := RootEntry{
		Cr:    "x",
		Stem0: "zero",
		Stem1: "one",
		Stem2: "two",
		Stem3: "three",
	}
	cases := []struct {
		i    int
		want string
	}{
		{0, "zero"}, {1, "one"}, {2, "two"}, {3, "three"},
		{-1, "zero"}, {99, "zero"}, // out of range falls to Stem0
	}
	for _, c := range cases {
		if got := r.Stem(c.i); got != c.want {
			t.Errorf("RootEntry.Stem(%d) = %q, want %q", c.i, got, c.want)
		}
	}
}

func TestLoadRoots_BadPath(t *testing.T) {
	if _, err := LoadRoots("/nonexistent/path.json"); err == nil {
		t.Error("LoadRoots(/nonexistent) succeeded, want error")
	}
}

func TestLoadAffixes_BadPath(t *testing.T) {
	if _, err := LoadAffixes("/nonexistent/path.json"); err == nil {
		t.Error("LoadAffixes(/nonexistent) succeeded, want error")
	}
}

func TestLoad_BadPaths(t *testing.T) {
	if _, err := Load("/no/roots.json", "/no/affixes.json"); err == nil {
		t.Error("Load with bad paths succeeded, want error")
	}
}

func TestParseRoots_BadJSON(t *testing.T) {
	if _, err := parseRoots([]byte("not json")); err == nil {
		t.Error("parseRoots(bad json) succeeded, want error")
	}
}

func TestParseAffixes_BadJSON(t *testing.T) {
	if _, err := parseAffixes([]byte("not json")); err == nil {
		t.Error("parseAffixes(bad json) succeeded, want error")
	}
}

func TestLoad_BadAffixesPath(t *testing.T) {
	// Cover Load's second-error branch: roots load OK, affixes fail.
	if _, err := Load(dataPath("roots.json"), "/nonexistent/path.json"); err == nil {
		t.Error("Load with bad affixes path succeeded, want error")
	}
}

func TestRootEntry_RichFields(t *testing.T) {
	roots, err := LoadRoots(dataPath("roots.json"))
	if err != nil {
		t.Fatalf("LoadRoots: %v", err)
	}
	// "t" (demonstrative) has Objective alternates per stem in upstream.
	tr, ok := roots["t"]
	if !ok {
		t.Fatalf("root \"t\" missing")
	}
	if len(tr.Objective) != 3 {
		t.Fatalf("root \"t\".Objective = %v, want 3 entries", tr.Objective)
	}
	for i, s := range tr.Objective {
		if s == "" {
			t.Errorf("root \"t\".Objective[%d] empty", i)
		}
	}
	// Plain entry with no rich fields should not allocate any.
	mb, ok := roots["mb"]
	if !ok {
		t.Fatalf("root \"mb\" missing")
	}
	if mb.Objective != nil || mb.Completive != nil || mb.Wikidata != nil ||
		mb.Contential != "" || mb.Constitutive != "" || mb.Dynamic != "" {
		t.Errorf("root \"mb\" should have no rich fields, got %+v", mb)
	}
}

func TestLoadDefault(t *testing.T) {
	// LoadDefault reads from the embedded JSON, which is always
	// available. Sanity-check a few well-known entries are present.
	lex, err := LoadDefault()
	if err != nil {
		t.Fatalf("LoadDefault: %v", err)
	}
	if len(lex.Roots) == 0 {
		t.Error("LoadDefault: Roots empty")
	}
	if len(lex.Affixes) == 0 {
		t.Error("LoadDefault: Affixes empty")
	}
	// Canonical root "ml" exists; canonical affix "rf" (SIZ) exists.
	if _, ok := lex.Roots["ml"]; !ok {
		t.Error("LoadDefault: root \"ml\" missing")
	}
	if _, ok := lex.Affixes["rf"]; !ok {
		t.Error("LoadDefault: affix \"rf\" missing")
	}
}
