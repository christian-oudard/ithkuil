package search

import "testing"

// TestMatchesWord_NotMidWord is the bug this exists for. Searching the
// grammar for "ERG" answered with the Absolutive, because its
// description reads "PATIENT undergoing the act" and a substring test
// finds "erg" inside "undergoing". SQLite's full-text index never had
// the fault, which is why the lexicon half of a store-backed search was
// right all along and only the grammar table, which has no index and
// cannot have one in a browser, was wrong.
func TestMatchesWord_NotMidWord(t *testing.T) {
	for _, tc := range []struct {
		text, query string
		want        bool
	}{
		{"PATIENT undergoing the act", "erg", false},
		{"AGENT or FORCE that causes the act", "erg", false},
		{"Ergative", "erg", true},
		{"indicate", "cat", false},
		{"communicate", "cat", false},
		{"🐈 cat (Felis catus)", "cat", true},
		{"waterfall/cascade", "water", true},
		{"water in motion", "water", true},
		{"Case/Transrelative", "trans", true},
		{"(ASR) Assertive", "assertive", true},
		{"a feedback-driven system", "driven", true},
		{"undriven", "driven", false},
		{"BEN1", "ben", true},
		{"anything", "", false},
	} {
		if got := matchesWord(tc.text, tc.query); got != tc.want {
			t.Errorf("matchesWord(%q, %q) = %v, want %v", tc.text, tc.query, got, tc.want)
		}
	}
}

// TestSearchGrammar_NoMidWordHits pins the whole-search behaviour, not
// just the matcher: ERG must find the Ergative and nothing that merely
// contains those three letters.
func TestSearchGrammar_NoMidWordHits(t *testing.T) {
	for _, e := range SearchGrammar("ERG") {
		if e.Abbrev != "ERG" && !matchesWord(e.Name, "erg") {
			t.Errorf("ERG matched %s/%s (%q)", e.Category, e.Abbrev, e.Description)
		}
	}
}
