package search

import (
	"encoding/json"
	"os"
	"path/filepath"
	"sort"
	"testing"
)

// storeGrammar reads the grammar section of data/data.json, which is
// what tools/build_db.py turns into the store the CLI reads. The
// section is a flat list of {abbrev, category, form} rows transcribed
// from Quijada's tables, arrived at independently of the Go lookup
// tables, which is the whole reason it is worth comparing against.
func storeGrammar(t *testing.T) []struct{ Abbrev, Category, Form string } {
	t.Helper()
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
	if len(doc.Grammar) == 0 {
		t.Fatal("data.json has no grammar section")
	}
	return doc.Grammar
}

// TestTable_MatchesStore holds the two independent statements of the
// grammar inventory against each other. Table is built at init time
// from the grammar package's AllX slices and the parse encoders; the
// store is transcribed from Quijada's tables. Neither derives from the
// other, so a disagreement is a real defect in one of them, and this is
// the only place either can be checked for completeness at all: a
// category the Go code has never heard of is invisible to every other
// test in the suite, because every other test enumerates from the Go
// code.
func TestTable_MatchesStore(t *testing.T) {
	code := map[string]bool{}
	for _, e := range Table {
		key := e.Category + "/" + e.Abbrev
		if code[key] {
			t.Errorf("Table lists %s twice", key)
		}
		code[key] = true
	}
	store := map[string]bool{}
	for _, e := range storeGrammar(t) {
		store[e.Category+"/"+e.Abbrev] = true
	}
	var missing, extra []string
	for k := range store {
		if !code[k] {
			missing = append(missing, k)
		}
	}
	for k := range code {
		if !store[k] {
			extra = append(extra, k)
		}
	}
	sort.Strings(missing)
	sort.Strings(extra)
	if len(missing) > 0 {
		t.Errorf("in the store but not in Table (%d): %v", len(missing), missing)
	}
	if len(extra) > 0 {
		t.Errorf("in Table but not in the store (%d): %v", len(extra), extra)
	}
}

// TestTable_FormsMatchStore compares the written form for every entry
// that carries one. This is the check that found OPT: commit 4698e4d
// corrected the bias from ččk to ççk in data.json, on the grammar plus
// 28 standalone corpus instances against none, and parse's own table
// was left on the old spelling. The two halves of one correction can
// drift apart silently because nothing else reads both.
//
// Table fills Form for Case, Bias, Register and CarrierType, where a
// value maps to one conjunct. Categories whose form depends on
// neighbouring slots — the Ca components, whose letters change under
// allomorphy, and the Vn categories, which share four vowel series —
// carry no Form and are skipped here rather than compared wrongly.
func TestTable_FormsMatchStore(t *testing.T) {
	form := map[string]string{}
	for _, e := range storeGrammar(t) {
		form[e.Category+"/"+e.Abbrev] = e.Form
	}
	var compared int
	for _, e := range Table {
		if e.Form == "" {
			continue
		}
		key := e.Category + "/" + e.Abbrev
		want, ok := form[key]
		if !ok {
			continue // TestTable_MatchesStore reports this.
		}
		compared++
		if e.Form != want {
			t.Errorf("%s: Table has %q, the store has %q", key, e.Form, want)
		}
	}
	if compared == 0 {
		t.Fatal("no entry carried a form; the test is not exercising anything")
	}
	t.Logf("%d written forms agree with the store", compared)
}
