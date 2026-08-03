package store_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/store"
)

// TestLoadLexicon checks the whole-lexicon read. It lives here rather
// than beside the Lexicon type because store depends on lexicon and not
// the other way round, which is what keeps the database driver out of
// every package that only wants a root's meaning.
func TestLoadLexicon(t *testing.T) {
	s, err := store.Open(store.DefaultPath())
	if err != nil {
		t.Skipf("data.db not available: %v", err)
	}
	defer s.Close()
	lex, err := store.LoadLexicon(s)
	if err != nil {
		t.Fatalf("LoadLexicon: %v", err)
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
