package lexicon

import (
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"testing"
)

// dataPath returns the path to a file under the repo's data/ directory,
// resolved relative to this test file (code/lexicon/ → ../../data/).
func dataPath(name string) string {
	return filepath.Join("..", "..", "data", name)
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

// XCL is documented in the grammar but absent from the upstream
// spreadsheet the lexicon syncs from, so it only survives because
// tools/sync_lexicon.py keeps local-only affixes. Guard that.
func TestLoad_LocalOnlyAffix(t *testing.T) {
	lex, err := Load(dataPath("data.json"))
	if err != nil {
		t.Fatalf("Load error: %v", err)
	}
	xcl, ok := lex.Affixes["çx"]
	if !ok {
		t.Fatal("affix \"çx\" (XCL) not found; did a lexicon sync drop it?")
	}
	if xcl.Abbrev != "XCL" {
		t.Errorf("affix \"çx\" abbrev = %q, want XCL", xcl.Abbrev)
	}
	if len(xcl.Degrees) != 9 {
		t.Errorf("affix \"çx\" degrees = %d, want 9", len(xcl.Degrees))
	}
}

// A C_R identifies a root, so two roots cannot share one. Five do in
// the upstream spreadsheet: cfw, ksmy, lzbḑ, nļt and rţnw, each a pair
// of unrelated senses.
//
// Every collision this once carried has been resolved upstream, so the
// list is empty. The guard is kept rather than deleted: both readers
// keep the first of a pair and drop the second, so a collision
// reintroduced upstream would lose a root silently. It fails in either
// direction, which is the point.
func TestParse_DuplicateClusters(t *testing.T) {
	b, err := os.ReadFile(dataPath("data.json"))
	if err != nil {
		t.Skipf("data.json not readable: %v", err)
	}
	var doc struct {
		Roots []struct {
			Cr    string `json:"cr"`
			Stem1 string `json:"stem1"`
		} `json:"roots"`
	}
	if err := json.Unmarshal(b, &doc); err != nil {
		t.Fatal(err)
	}
	seen := map[string]string{}
	collisions := map[string][]string{}
	for _, r := range doc.Roots {
		if first, ok := seen[r.Cr]; ok {
			collisions[r.Cr] = []string{first, r.Stem1}
			continue
		}
		seen[r.Cr] = r.Stem1
	}

	// Upstream has resolved every collision this once pinned: cfw,
	// lzbḑ, nļt and rţnw. The guard stays and the list is empty, so a
	// new one cannot appear without a root vanishing noticeably.
	var want []string
	var got []string
	for cr := range collisions {
		got = append(got, cr)
	}
	sort.Strings(got)
	sort.Strings(want)
	if !slices.Equal(got, want) {
		t.Errorf("duplicated clusters = %v, want %v\n"+
			"a change here is upstream's doing: update this list and say which sense won",
			got, want)
	}

	// Whichever reader loads them, the first sense of a pair is the one
	// that survives. store keeps the first and this used to keep the
	// last, so one file produced two different lexicons.
	lex, err := Parse(b)
	if err != nil {
		t.Fatal(err)
	}
	for cr, senses := range collisions {
		if lex.Roots[cr].Stem1 != senses[0] {
			t.Errorf("%s resolved to %q, want the first sense %q",
				cr, lex.Roots[cr].Stem1, senses[0])
		}
	}
}
