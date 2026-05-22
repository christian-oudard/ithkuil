package lexicon

import (
	"path/filepath"
	"testing"

	"github.com/christian-oudard/ithkuil/store"
)

// dataPath returns the path to a file under the repo's data/ directory,
// resolved relative to this test file (lexicon/ → ../data/).
func dataPath(name string) string {
	return filepath.Join("..", "data", name)
}

func TestLoad(t *testing.T) {
	lex, err := Load(dataPath("data.json"))
	if err != nil {
		t.Fatalf("Load error: %v", err)
	}
	if lex.Version == 0 {
		t.Error("Load: Version zero")
	}
	if len(lex.Roots) < 4000 {
		t.Errorf("expected several thousand root entries, got %d", len(lex.Roots))
	}
	if len(lex.Affixes) < 400 {
		t.Errorf("expected several hundred affix entries, got %d", len(lex.Affixes))
	}
	b, ok := lex.Roots["b"]
	if !ok {
		t.Fatalf("root \"b\" not found")
	}
	if b.Stem0 == "" || b.Stem1 == "" || b.Stem2 == "" || b.Stem3 == "" {
		t.Errorf("root \"b\" has empty stems: %+v", b)
	}
	ba, ok := lex.Affixes["b"]
	if !ok {
		t.Fatalf("affix \"b\" not found")
	}
	if ba.Abbrev == "" {
		t.Errorf("affix \"b\" has no abbrev")
	}
	if len(ba.Degrees) != 9 {
		t.Errorf("affix \"b\" degrees = %d, want 9", len(ba.Degrees))
	}
}

func TestLoadFromStore(t *testing.T) {
	s, err := store.Open(dataPath("data.db"))
	if err != nil {
		t.Skipf("data.db not available: %v", err)
	}
	defer s.Close()
	lex, err := LoadFromStore(s)
	if err != nil {
		t.Fatalf("LoadFromStore: %v", err)
	}
	if len(lex.Roots) < 4000 {
		t.Errorf("expected several thousand root entries, got %d", len(lex.Roots))
	}
	if len(lex.Affixes) < 400 {
		t.Errorf("expected several hundred affix entries, got %d", len(lex.Affixes))
	}
	if _, ok := lex.Roots["ml"]; !ok {
		t.Error("root \"ml\" missing")
	}
	if _, ok := lex.Affixes["rf"]; !ok {
		t.Error("affix \"rf\" missing")
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
		{-1, "zero"}, {99, "zero"},
	}
	for _, c := range cases {
		if got := r.Stem(c.i); got != c.want {
			t.Errorf("RootEntry.Stem(%d) = %q, want %q", c.i, got, c.want)
		}
	}
}

func TestLoad_BadPath(t *testing.T) {
	if _, err := Load("/nonexistent/path.json"); err == nil {
		t.Error("Load(/nonexistent) succeeded, want error")
	}
}

func TestParseLexicon_BadJSON(t *testing.T) {
	if _, err := parseLexicon([]byte("not json")); err == nil {
		t.Error("parseLexicon(bad json) succeeded, want error")
	}
}

func TestRootEntry_RichFields(t *testing.T) {
	lex, err := Load(dataPath("data.json"))
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	tr, ok := lex.Roots["t"]
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
	mb, ok := lex.Roots["mb"]
	if !ok {
		t.Fatalf("root \"mb\" missing")
	}
	if mb.Objective != nil || mb.Completive != nil || mb.Wikidata != nil ||
		mb.Contential != "" || mb.Constitutive != "" || mb.Dynamic != "" {
		t.Errorf("root \"mb\" should have no rich fields, got %+v", mb)
	}
}

func TestAffixEntry_CategoryValue(t *testing.T) {
	lex, err := Load(dataPath("data.json"))
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	cases := []struct {
		cs       string
		degree   int
		typeCode int
		want     string
	}{
		{"bẓ", 1, 1, "SUB"},
		{"bẓ", 5, 1, "HYP"},
		{"bž", 1, 1, "PCT"},
		{"nļ", 1, 1, "ASR"},
		{"nļ", 1, 2, "OBS"},
		{"nļ", 9, 2, "INF"},
		{"b", 1, 1, ""},
		{"bẓ", 0, 1, ""},
		{"bẓ", 10, 1, ""},
		{"bẓ", 3, 3, ""},
	}
	for _, c := range cases {
		entry, ok := lex.Affixes[c.cs]
		if !ok {
			t.Errorf("affix %q missing from lexicon", c.cs)
			continue
		}
		got := entry.CategoryValue(c.degree, c.typeCode)
		if got != c.want {
			t.Errorf("%s.CategoryValue(%d, type=%d) = %q, want %q",
				c.cs, c.degree, c.typeCode, got, c.want)
		}
	}
}
