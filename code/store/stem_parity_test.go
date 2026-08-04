package store_test

import (
	"database/sql"
	"os"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/api"
	"github.com/christian-oudard/ithkuil/search"
	"github.com/christian-oudard/ithkuil/store"

	_ "modernc.org/sqlite"
)

// The store's index stems with FTS5's `porter` tokenizer and the
// browser stems with search.Stem, because a page has no SQLite. Two
// implementations of one algorithm is exactly the arrangement that
// drifts, and a drift here is invisible: a query just quietly stops
// finding something in one of the two.
//
// So this asks SQLite what it thinks, over every word in the lexicon,
// and fails on any disagreement. It builds its own throwaway index
// rather than reading data.db, so it does not depend on the store
// having been rebuilt.
func TestStem_MatchesSQLitePorter(t *testing.T) {
	db, err := sql.Open("sqlite", ":memory:")
	if err != nil {
		t.Skipf("no sqlite: %v", err)
	}
	defer db.Close()
	if _, err := db.Exec(`CREATE VIRTUAL TABLE t USING fts5(w, tokenize="porter unicode61")`); err != nil {
		t.Skipf("porter tokenizer unavailable: %v", err)
	}

	for _, w := range stemWords(t) {
		if _, err := db.Exec(`INSERT INTO t(w) VALUES (?)`, w); err != nil {
			t.Fatalf("index %q: %v", w, err)
		}
	}

	// fts5vocab exposes the index's own terms, which are the stems
	// SQLite produced. Comparing sets rather than word-by-word mappings
	// is enough: if the two stemmers agree on every word, they produce
	// the same set of terms, and any word they disagree on leaves a
	// term in one set and not the other.
	if _, err := db.Exec(`CREATE VIRTUAL TABLE v USING fts5vocab(t, 'row')`); err != nil {
		t.Skipf("fts5vocab unavailable: %v", err)
	}
	rows, err := db.Query(`SELECT term FROM v`)
	if err != nil {
		t.Fatal(err)
	}
	defer rows.Close()
	theirs := map[string]bool{}
	for rows.Next() {
		var term string
		if err := rows.Scan(&term); err != nil {
			t.Fatal(err)
		}
		theirs[term] = true
	}

	ours := map[string]bool{}
	for _, w := range stemWords(t) {
		for _, tok := range tokenize(w) {
			ours[search.Stem(tok)] = true
		}
	}

	var missing, extra []string
	for term := range theirs {
		if !ours[term] {
			missing = append(missing, term)
		}
	}
	for term := range ours {
		if !theirs[term] {
			extra = append(extra, term)
		}
	}
	if len(missing) > 0 || len(extra) > 0 {
		t.Errorf("stemmers disagree over %d terms\n  SQLite has, we do not: %v\n  we have, SQLite does not: %v",
			len(theirs), sample(missing), sample(extra))
	}
}

// stemWords is every distinct word in the lexicon's English glosses,
// which is the text a search actually runs against.
func stemWords(t *testing.T) []string {
	t.Helper()
	path := store.DefaultPath()
	if _, err := os.Stat(path); err != nil {
		t.Skipf("data.db not found (%v); run tools/build_db.py", err)
	}
	s, err := store.Open(path)
	if err != nil {
		t.Skip(err)
	}
	defer s.Close()
	roots, err := s.AllRoots()
	if err != nil {
		t.Fatal(err)
	}
	seen := map[string]bool{}
	var out []string
	for _, r := range roots {
		for _, field := range []string{r.Stem0, r.Stem1, r.Stem2, r.Stem3} {
			for _, w := range tokenize(field) {
				if !seen[w] {
					seen[w] = true
					out = append(out, w)
				}
			}
		}
	}
	return out
}

// tokenize splits on everything outside a-z and 0-9, and keeps only
// the pure-ASCII alphabetic pieces.
//
// Restricting to those is what makes this a test of the stemmer rather
// than of a tokenizer. unicode61 folds diacritics and splits on
// typographic punctuation, so feeding it "frémont's" or "snyder–robinson"
// measures how well this test reimplements unicode61, which is not the
// question. search.Stem passes anything non-ASCII through untouched, so
// those words are outside its remit either way.
func tokenize(s string) []string {
	var out []string
	for _, f := range strings.FieldsFunc(strings.ToLower(s), func(r rune) bool {
		return !(r >= 'a' && r <= 'z' || r >= '0' && r <= '9')
	}) {
		if isASCIIWord(f) {
			out = append(out, f)
		}
	}
	return out
}

func isASCIIWord(s string) bool {
	if s == "" {
		return false
	}
	for i := 0; i < len(s); i++ {
		if s[i] < 'a' || s[i] > 'z' {
			return false
		}
	}
	return true
}

func sample(ss []string) []string {
	if len(ss) > 12 {
		return ss[:12]
	}
	return ss
}

// TestSearchEnginesAgree is the property the stemming was for. There
// are two lexicon searches, the SQLite index here and the in-memory
// scan in api, and a browser can only have the second. Two engines
// answering one question is exactly the arrangement that drifts, and a
// drift is invisible from either side: a query quietly stops finding
// something in one of them.
//
// Before stemming they disagreed badly, 13 of 20 hits for "water" and
// 16 of 20 for "cat". The queries below are the ones that showed it.
func TestSearchEnginesAgree(t *testing.T) {
	path := store.DefaultPath()
	if _, err := os.Stat(path); err != nil {
		t.Skipf("data.db not found (%v); run tools/build_db.py", err)
	}
	s, err := store.Open(path)
	if err != nil {
		t.Skip(err)
	}
	defer s.Close()
	lex, err := store.LoadLexicon(s)
	if err != nil {
		t.Fatal(err)
	}
	a := api.New()
	a.SetLexicon(lex)

	for _, q := range []string{
		"speak", "speaks", "speaking", "cat", "cats", "tree", "trees",
		"water", "watering", "run", "running", "erg", "degree", "study",
	} {
		indexed, err := s.SearchRoots(q, 1000)
		if err != nil {
			t.Fatalf("%s: %v", q, err)
		}
		inIndex := map[string]bool{}
		for _, h := range indexed {
			inIndex[h.Cr] = true
		}
		inMemory := map[string]bool{}
		// A negative limit uncaps, so this compares the whole answer
		// rather than two different top-twenties.
		for _, h := range a.Search(q, api.SearchOptions{Limit: -1}).Roots {
			inMemory[h.Root.Cr] = true
		}
		for cr := range inIndex {
			if !inMemory[cr] {
				t.Errorf("%q: the index finds root %q and the scan does not", q, cr)
			}
		}
		for cr := range inMemory {
			if !inIndex[cr] {
				t.Errorf("%q: the scan finds root %q and the index does not", q, cr)
			}
		}
	}
}

// TestSearch_InflectedFindsBase is the user-visible half: an inflected
// query used to find almost nothing, because the index matched whole
// tokens and their prefixes and nothing else. "trees" answered with 2
// roots against "tree"'s 306.
func TestSearch_InflectedFindsBase(t *testing.T) {
	path := store.DefaultPath()
	if _, err := os.Stat(path); err != nil {
		t.Skipf("data.db not found (%v); run tools/build_db.py", err)
	}
	s, err := store.Open(path)
	if err != nil {
		t.Skip(err)
	}
	defer s.Close()
	for _, pair := range [][2]string{
		{"cat", "cats"}, {"tree", "trees"}, {"speak", "speaks"},
		{"water", "watering"},
	} {
		base, err := s.SearchRoots(pair[0], 1000)
		if err != nil {
			t.Fatal(err)
		}
		inflected, err := s.SearchRoots(pair[1], 1000)
		if err != nil {
			t.Fatal(err)
		}
		if len(base) == 0 {
			t.Fatalf("%q finds nothing at all", pair[0])
		}
		if len(base) != len(inflected) {
			t.Errorf("%q finds %d roots and %q finds %d; a stemmer makes them one query",
				pair[0], len(base), pair[1], len(inflected))
		}
	}
}

// TestSearch_ClusterIsStillAPrefix pins what the prose columns gave up
// prefix matching for and the cluster column kept: a root is looked up
// by its letters, and "ţ" should find ţr.
func TestSearch_ClusterIsStillAPrefix(t *testing.T) {
	path := store.DefaultPath()
	if _, err := os.Stat(path); err != nil {
		t.Skipf("data.db not found (%v); run tools/build_db.py", err)
	}
	s, err := store.Open(path)
	if err != nil {
		t.Skip(err)
	}
	defer s.Close()
	hits, err := s.SearchRoots("ţr", 100)
	if err != nil {
		t.Fatal(err)
	}
	var found bool
	for _, h := range hits {
		if h.Cr == "ţr" {
			found = true
		}
	}
	if !found {
		t.Errorf("ţr is a root and its own cluster search misses it; got %d hits", len(hits))
	}
	// The digraph notation's punctuation is a term, not query syntax.
	if _, err := s.SearchRoots("l,x", 10); err != nil {
		t.Errorf("a cluster with a comma is an error: %v", err)
	}
}
