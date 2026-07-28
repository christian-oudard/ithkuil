package store_test

import (
	"os"
	"testing"

	"github.com/christian-oudard/ithkuil/store"
)

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
