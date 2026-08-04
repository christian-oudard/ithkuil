package store_test

import (
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"testing"

	"github.com/christian-oudard/ithkuil/store"
)

// TestOpen_Missing pins two things about a path that is not there.
// The error must name the missing file, not surface later as "no such
// table: roots" from the first query. And Open must not create it: the
// DSN asks for mode=ro, but sqlite made the file anyway, so a mistyped
// --data left a stray empty database behind.
func TestOpen_Missing(t *testing.T) {
	path := filepath.Join(t.TempDir(), "absent.db")
	s, err := store.Open(path)
	if err == nil {
		s.Close()
		t.Fatal("Open on a missing file succeeded, want error")
	}
	if !errors.Is(err, os.ErrNotExist) {
		t.Errorf("Open error = %v, want it to wrap os.ErrNotExist", err)
	}
	if _, err := os.Stat(path); err == nil {
		t.Error("Open created the database file it was asked to read")
	}
}

func TestOpen(t *testing.T) {
	testDB := store.DefaultPath()
	if _, err := os.Stat(testDB); err != nil {
		t.Skipf("data.db not found (%v); run data/build_db.py first", err)
	}
	s, err := store.Open(testDB)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()

	t.Run("Grammar", func(t *testing.T) {
		e, err := s.Grammar("THM")
		if err != nil {
			t.Fatal(err)
		}
		if e == nil {
			t.Fatal("THM not found")
		}
		if e.Name != "Thematic" {
			t.Errorf("Name = %q, want Thematic", e.Name)
		}
		if e.Form != "a" {
			t.Errorf("Form = %q, want a", e.Form)
		}
	})

	// A miss is a nil entry and no error, so a caller can tell "not in
	// the table" from "the query failed" without inspecting the error.
	t.Run("GrammarMissing", func(t *testing.T) {
		e, err := s.Grammar("NOPE")
		if err != nil {
			t.Fatalf("a miss is not an error: %v", err)
		}
		if e != nil {
			t.Errorf("want nil for an unknown abbreviation, got %+v", e)
		}
	})

	t.Run("RootMissing", func(t *testing.T) {
		e, err := s.Root("zzzz")
		if err != nil {
			t.Fatalf("a miss is not an error: %v", err)
		}
		if e != nil {
			t.Errorf("want nil for an unknown cluster, got %+v", e)
		}
	})

	t.Run("GrammarCategory", func(t *testing.T) {
		entries, err := s.GrammarCategory("Case")
		if err != nil {
			t.Fatal(err)
		}
		if len(entries) < 68 {
			t.Errorf("got %d case entries, want ≥68", len(entries))
		}
	})

	t.Run("Root", func(t *testing.T) {
		e, err := s.Root("ml")
		if err != nil {
			t.Fatal(err)
		}
		if e == nil {
			t.Fatal("root ml not found")
		}
	})

	t.Run("SearchRoots", func(t *testing.T) {
		hits, err := s.SearchRoots("speak", 5)
		if err != nil {
			t.Fatal(err)
		}
		if len(hits) == 0 {
			t.Error("SearchRoots(speak) returned no results")
		}
	})

	t.Run("SearchAffixes", func(t *testing.T) {
		hits, err := s.SearchAffixes("degree", 5)
		if err != nil {
			t.Fatal(err)
		}
		if len(hits) == 0 {
			t.Error("SearchAffixes(degree) returned no results")
		}
	})
}

// TestGrammarAll_MatchesSource closes the loop nothing else does:
// data.json is the source, tools/build_db.py builds the store from it,
// and until now no test compared the two. A build that silently dropped
// or renamed rows looked exactly like a working one, and a stale store
// reports itself as an unrelated failure somewhere downstream.
func TestGrammarAll_MatchesSource(t *testing.T) {
	testDB := store.DefaultPath()
	if _, err := os.Stat(testDB); err != nil {
		t.Skipf("data.db not found (%v); run tools/build_db.py first", err)
	}
	s, err := store.Open(testDB)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()

	b, err := os.ReadFile(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatalf("read data.json: %v", err)
	}
	var doc struct {
		Grammar []struct{ Abbrev, Category, Form string } `json:"grammar"`
	}
	if err := json.Unmarshal(b, &doc); err != nil {
		t.Fatalf("parse data.json: %v", err)
	}

	built, err := s.GrammarAll()
	if err != nil {
		t.Fatal(err)
	}
	if len(built) != len(doc.Grammar) {
		t.Fatalf("store has %d grammar rows, data.json has %d; rebuild with tools/build_db.py",
			len(built), len(doc.Grammar))
	}
	inStore := make(map[string]string, len(built))
	for _, e := range built {
		inStore[e.Category+"/"+e.Abbrev] = e.Form
	}
	for _, e := range doc.Grammar {
		key := e.Category + "/" + e.Abbrev
		form, ok := inStore[key]
		if !ok {
			t.Errorf("%s is in data.json but not in the store", key)
			continue
		}
		if form != e.Form {
			t.Errorf("%s: store has form %q, data.json has %q", key, form, e.Form)
		}
	}
}

// TestDefaultPath covers both arms. The XDG_DATA_HOME branch is the one
// a packaged install takes and the one every test that overrides the
// data directory relies on, so a change here moves every test's store
// out from under it.
func TestDefaultPath(t *testing.T) {
	t.Setenv("XDG_DATA_HOME", "/xdg")
	if got, want := store.DefaultPath(), filepath.Join("/xdg", "ithkuil", "data.db"); got != want {
		t.Errorf("DefaultPath() = %q, want %q", got, want)
	}
	t.Setenv("XDG_DATA_HOME", "")
	home, err := os.UserHomeDir()
	if err != nil {
		t.Skipf("no home directory: %v", err)
	}
	want := filepath.Join(home, ".local", "share", "ithkuil", "data.db")
	if got := store.DefaultPath(); got != want {
		t.Errorf("DefaultPath() with no XDG_DATA_HOME = %q, want %q", got, want)
	}
}

// TestAll covers the two whole-table reads. They are how lexicon loads,
// so they are exercised indirectly by most of the suite, but the counts
// are worth stating here where the store is the thing under test.
func TestAll(t *testing.T) {
	testDB := store.DefaultPath()
	if _, err := os.Stat(testDB); err != nil {
		t.Skipf("data.db not found (%v); run tools/build_db.py first", err)
	}
	s, err := store.Open(testDB)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()

	roots, err := s.AllRoots()
	if err != nil {
		t.Fatal(err)
	}
	if len(roots) < 5000 {
		t.Errorf("AllRoots() = %d, want the whole lexicon", len(roots))
	}
	for _, r := range roots[:10] {
		if r.Cr == "" {
			t.Errorf("root with no cluster: %+v", r)
		}
	}

	affixes, err := s.AllAffixes()
	if err != nil {
		t.Fatal(err)
	}
	if len(affixes) < 500 {
		t.Errorf("AllAffixes() = %d, want the whole table", len(affixes))
	}
	for _, a := range affixes[:10] {
		if a.Cs == "" {
			t.Errorf("affix with no cluster: %+v", a)
		}
		if len(a.Degrees) == 0 {
			t.Errorf("affix %s has no degrees; the JSON column did not decode", a.Cs)
		}
	}
}

// A search term goes into an FTS5 MATCH expression, which is a query
// language rather than a plain string. The term was concatenated in
// raw, so the punctuation the ASCII digraph notation uses was read as
// syntax: "l,x" came back as `fts5: syntax error near ","` rather than
// as a search that found nothing. Searching for a root by its cluster
// is the ordinary case, so this failed on ordinary input.
func TestSearchRoots_PunctuationIsNotSyntax(t *testing.T) {
	testDB := store.DefaultPath()
	if _, err := os.Stat(testDB); err != nil {
		t.Skipf("data.db not found (%v); run tools/build_db.py first", err)
	}
	s, err := store.Open(testDB)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()

	for _, q := range []string{`l,x`, `t,`, `-tl-`, `a"b`, `sq`, `ml`, ``} {
		if _, err := s.SearchRoots(q, 5); err != nil {
			t.Errorf("SearchRoots(%q): %v", q, err)
		}
		if _, err := s.SearchAffixes(q, 5); err != nil {
			t.Errorf("SearchAffixes(%q): %v", q, err)
		}
	}
}
