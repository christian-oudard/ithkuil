// Package store provides read access to the Ithkuil data database, built
// from data/data.json by data/build_db.py and installed at DefaultPath.
//
// Open the database once at process startup and pass the *Store to
// whatever needs it. All methods are read-only and safe for concurrent
// use after Open returns.
package store

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	_ "modernc.org/sqlite"
)

// DefaultPath is where the database lives: $XDG_DATA_HOME/ithkuil/data.db,
// or ~/.local/share/ithkuil/data.db when XDG_DATA_HOME is unset (the
// location the XDG Base Directory spec mandates in that case).
//
// The database is a build artifact, so it does not belong in the source
// tree. data/build_db.py writes here; --data overrides.
func DefaultPath() string {
	dir := os.Getenv("XDG_DATA_HOME")
	if dir == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			panic(fmt.Sprintf("store.DefaultPath: %v", err))
		}
		dir = filepath.Join(home, ".local", "share")
	}
	return filepath.Join(dir, "ithkuil", "data.db")
}

// Store is an open connection to the Ithkuil data database.
type Store struct {
	db *sql.DB
}

// Open opens the SQLite database at path. Call Close when done.
func Open(path string) (*Store, error) {
	// The "mode=ro" below is not honoured: a missing file survives
	// both sql.Open and Ping, and is created empty on the way through.
	// A mistyped --data therefore left a stray database behind and
	// reported "no such table: roots" from whichever query ran first,
	// rather than saying the file was not there. Stat first, so the
	// error names the real problem and a read-only store stops writing
	// to disk.
	if _, err := os.Stat(path); err != nil {
		return nil, fmt.Errorf("store.Open %s: %w", path, err)
	}
	db, err := sql.Open("sqlite", path+"?mode=ro")
	if err != nil {
		return nil, fmt.Errorf("store.Open %s: %w", path, err)
	}
	if err := db.Ping(); err != nil {
		db.Close()
		return nil, fmt.Errorf("store.Open %s: %w", path, err)
	}
	return &Store{db: db}, nil
}

// Close releases the database connection.
func (s *Store) Close() error { return s.db.Close() }

// ── Grammar ──────────────────────────────────────────────────────────────────

// GrammarEntry is one row from the grammar table.
type GrammarEntry struct {
	Abbrev      string
	Name        string
	Category    string
	Form        string
	Description string
	Explanation string
}

// Grammar returns the entry for the given abbreviation, or nil if not found.
func (s *Store) Grammar(abbrev string) (*GrammarEntry, error) {
	row := s.db.QueryRow(
		`SELECT abbrev, name, category, form, description, explanation
		   FROM grammar WHERE abbrev = ?`, abbrev)
	var e GrammarEntry
	if err := row.Scan(&e.Abbrev, &e.Name, &e.Category, &e.Form, &e.Description, &e.Explanation); err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, err
	}
	return &e, nil
}

// GrammarCategory returns all entries whose category starts with prefix
// (e.g. "Case" matches "Case/Transrelative", "Case/Appositive", ...).
func (s *Store) GrammarCategory(prefix string) ([]GrammarEntry, error) {
	rows, err := s.db.Query(
		`SELECT abbrev, name, category, form, description, explanation
		   FROM grammar WHERE category = ? OR category LIKE ?
		   ORDER BY rowid`,
		prefix, prefix+"/%")
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	return scanGrammar(rows)
}

// GrammarAll returns every grammar entry in table order.
func (s *Store) GrammarAll() ([]GrammarEntry, error) {
	rows, err := s.db.Query(
		`SELECT abbrev, name, category, form, description, explanation
		   FROM grammar ORDER BY rowid`)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	return scanGrammar(rows)
}

func scanGrammar(rows *sql.Rows) ([]GrammarEntry, error) {
	var out []GrammarEntry
	for rows.Next() {
		var e GrammarEntry
		if err := rows.Scan(&e.Abbrev, &e.Name, &e.Category, &e.Form, &e.Description, &e.Explanation); err != nil {
			return nil, err
		}
		out = append(out, e)
	}
	return out, rows.Err()
}

// ── Roots ─────────────────────────────────────────────────────────────────────

// RootEntry matches lexicon.RootEntry for drop-in use.
type RootEntry struct {
	Cr           string
	Stem0        string
	Stem1        string
	Stem2        string
	Stem3        string
	Contential   string
	Constitutive string
	Dynamic      string
	Objective    []string
	Completive   []string
	Wikidata     []string
}

// Root returns the entry for Cr, or nil if not found.
func (s *Store) Root(cr string) (*RootEntry, error) {
	row := s.db.QueryRow(
		`SELECT cr, stem0, stem1, stem2, stem3,
		        contential, constitutive, dynamic,
		        objective, completive, wikidata
		   FROM roots WHERE cr = ?`, cr)
	return scanRoot(row)
}

// AllRoots returns every root entry in insertion order.
func (s *Store) AllRoots() ([]RootEntry, error) {
	rows, err := s.db.Query(
		`SELECT cr, stem0, stem1, stem2, stem3,
		        contential, constitutive, dynamic,
		        objective, completive, wikidata
		   FROM roots ORDER BY rowid`)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	return scanRoots(rows)
}

// ftsPrefix turns a user's search term into an FTS5 prefix query.
//
// FTS5 MATCH takes an expression language of its own, not a plain
// string, so a term went in as syntax: "l,x" failed with `fts5: syntax
// error near ","` and any term holding a hyphen or a quote failed the
// same way. Every Ithkuil search that named a root by its cluster hit
// this, because those are exactly the characters the ASCII digraphs
// use. Wrapping the term in double quotes makes it a literal, and
// doubling any quote inside it escapes that.
func ftsPrefix(query string) string {
	return `"` + strings.ReplaceAll(query, `"`, `""`) + `"*`
}

// SearchRoots runs an FTS5 prefix search and returns ranked hits.
func (s *Store) SearchRoots(query string, limit int) ([]RootEntry, error) {
	rows, err := s.db.Query(
		`SELECT r.cr, r.stem0, r.stem1, r.stem2, r.stem3,
		        r.contential, r.constitutive, r.dynamic,
		        r.objective, r.completive, r.wikidata
		   FROM roots_fts f
		   JOIN roots r ON r.rowid = f.rowid
		  WHERE roots_fts MATCH ?
		  ORDER BY rank
		  LIMIT ?`, ftsPrefix(query), limit)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	return scanRoots(rows)
}

func scanRoot(row *sql.Row) (*RootEntry, error) {
	var e RootEntry
	var obj, cpt, wik string
	if err := row.Scan(&e.Cr, &e.Stem0, &e.Stem1, &e.Stem2, &e.Stem3,
		&e.Contential, &e.Constitutive, &e.Dynamic,
		&obj, &cpt, &wik); err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, err
	}
	json.Unmarshal([]byte(obj), &e.Objective)
	json.Unmarshal([]byte(cpt), &e.Completive)
	json.Unmarshal([]byte(wik), &e.Wikidata)
	return &e, nil
}

func scanRoots(rows *sql.Rows) ([]RootEntry, error) {
	var out []RootEntry
	for rows.Next() {
		var e RootEntry
		var obj, cpt, wik string
		if err := rows.Scan(&e.Cr, &e.Stem0, &e.Stem1, &e.Stem2, &e.Stem3,
			&e.Contential, &e.Constitutive, &e.Dynamic,
			&obj, &cpt, &wik); err != nil {
			return nil, err
		}
		json.Unmarshal([]byte(obj), &e.Objective)
		json.Unmarshal([]byte(cpt), &e.Completive)
		json.Unmarshal([]byte(wik), &e.Wikidata)
		out = append(out, e)
	}
	return out, rows.Err()
}

// ── Affixes ───────────────────────────────────────────────────────────────────

// AffixEntry matches lexicon.AffixEntry for drop-in use.
type AffixEntry struct {
	Cs          string
	Abbrev      string
	Description string
	Type        string
	Degrees     []string
}

// AllAffixes returns every affix entry in insertion order.
func (s *Store) AllAffixes() ([]AffixEntry, error) {
	rows, err := s.db.Query(
		`SELECT cs, abbrev, description, type, degrees FROM affixes ORDER BY rowid`)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []AffixEntry
	for rows.Next() {
		var e AffixEntry
		var deg string
		if err := rows.Scan(&e.Cs, &e.Abbrev, &e.Description, &e.Type, &deg); err != nil {
			return nil, err
		}
		json.Unmarshal([]byte(deg), &e.Degrees)
		out = append(out, e)
	}
	return out, rows.Err()
}

// SearchAffixes runs an FTS5 prefix search and returns ranked hits.
func (s *Store) SearchAffixes(query string, limit int) ([]AffixEntry, error) {
	rows, err := s.db.Query(
		`SELECT a.cs, a.abbrev, a.description, a.type, a.degrees
		   FROM affixes_fts f
		   JOIN affixes a ON a.rowid = f.rowid
		  WHERE affixes_fts MATCH ?
		  ORDER BY rank
		  LIMIT ?`, ftsPrefix(query), limit)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []AffixEntry
	for rows.Next() {
		var e AffixEntry
		var deg string
		if err := rows.Scan(&e.Cs, &e.Abbrev, &e.Description, &e.Type, &deg); err != nil {
			return nil, err
		}
		json.Unmarshal([]byte(deg), &e.Degrees)
		out = append(out, e)
	}
	return out, rows.Err()
}
