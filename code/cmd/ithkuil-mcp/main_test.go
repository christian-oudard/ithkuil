package main

import (
	"bytes"
	"context"
	"database/sql"
	"log"
	"path/filepath"
	"strings"
	"testing"

	_ "modernc.org/sqlite"

	"github.com/christian-oudard/ithkuil/store"
)

// writeEmptyDB makes a real SQLite file with none of the tables the
// store expects, which is what a half-finished or wrong-format data.db
// looks like from outside.
func writeEmptyDB(path string) error {
	db, err := sql.Open("sqlite", path)
	if err != nil {
		return err
	}
	defer db.Close()
	_, err = db.Exec("create table placeholder (x integer)")
	return err
}

// captureLog redirects the standard logger for the duration of a test,
// since the warnings newServer emits are the behaviour under test.
func captureLog(t *testing.T) *bytes.Buffer {
	t.Helper()
	var buf bytes.Buffer
	flags := log.Flags()
	log.SetOutput(&buf)
	log.SetFlags(0)
	t.Cleanup(func() {
		log.SetOutput(nil)
		log.SetFlags(flags)
	})
	return &buf
}

func TestNewServer_WithStore(t *testing.T) {
	if _, err := store.Open(store.DefaultPath()); err != nil {
		t.Skipf("no data store at %s; run tools/build_db.py", store.DefaultPath())
	}
	buf := captureLog(t)
	s := newServer(store.DefaultPath(), grammarDir())
	t.Cleanup(func() { s.st.Close() })
	if s.st == nil {
		t.Fatal("the store opened, so the server should hold it")
	}
	if len(s.lex.Roots) == 0 {
		t.Error("the lexicon should be loaded from the store")
	}
	if buf.Len() != 0 {
		t.Errorf("a healthy start warns about nothing; got %q", buf)
	}
}

// TestNewServer_NoStore is the documented fallback: a client launches
// this as a subprocess and cannot show a startup failure, so a missing
// store warns and the server serves what does not need it.
func TestNewServer_NoStore(t *testing.T) {
	buf := captureLog(t)
	s := newServer(filepath.Join(t.TempDir(), "absent.db"), grammarDir())
	if s.st != nil {
		t.Error("no store should have opened")
	}
	if s.lex == nil {
		t.Fatal("the lexicon must be non-nil even when empty; define checks it")
	}
	if len(s.lex.Roots) != 0 {
		t.Errorf("want an empty lexicon, got %d roots", len(s.lex.Roots))
	}
	if !strings.Contains(buf.String(), "cannot open data store") {
		t.Errorf("want a warning naming the store; got %q", buf)
	}

	// And the half that does not need a store still answers.
	_, out, err := s.parse(context.Background(), nil, parseIn{Text: "mala"})
	if err != nil {
		t.Fatalf("parse without a store: %v", err)
	}
	if out.Words[0].Gloss == "" {
		t.Error("parsing does not need the lexicon to produce a gloss")
	}
}

// TestNewServer_EmptyStore covers the second warning: the file opens as
// a database but holds none of the tables, so the lexicon load fails
// and the server falls back to an empty one rather than a nil one.
func TestNewServer_EmptyStore(t *testing.T) {
	path := filepath.Join(t.TempDir(), "empty.db")
	// sql.Open on a missing file creates it, which is what store.Open
	// stats to avoid; here that side effect is what we want, so make the
	// file the plainest way and let Open find it.
	if err := writeEmptyDB(path); err != nil {
		t.Skipf("cannot make an empty database: %v", err)
	}
	buf := captureLog(t)
	s := newServer(path, grammarDir())
	if s.lex == nil {
		t.Fatal("the lexicon must never be nil")
	}
	if len(s.lex.Roots) != 0 {
		t.Errorf("an empty store yields an empty lexicon; got %d roots", len(s.lex.Roots))
	}
	if !strings.Contains(buf.String(), "warning") {
		t.Errorf("want a warning; got %q", buf)
	}
	if s.st != nil {
		s.st.Close()
	}
}
