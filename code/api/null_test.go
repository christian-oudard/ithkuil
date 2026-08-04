package api

import (
	"bytes"
	"encoding/json"
	"fmt"
	"testing"
)

// A declared array must never arrive as null.
//
// ithkuil.d.ts declares `roots: RootHit[]`, not `RootHit[] | null`, and
// TypeScript believes it, so a page doing .roots.length crashed on a
// query that found nothing. Go makes this easy to get wrong: a nil
// slice marshals as null, and every slice starts nil, so the fault
// appears whenever a result is empty, which is the case nobody tries by
// hand.
//
// Initialising each slice at its construction is the fix, and this is
// what keeps it fixed. It is a stronger invariant than the arrays: no
// value anywhere in a reply is null, because every optional field is
// either a pointer with omitempty, which is omitted, or a slice, which
// must be empty rather than absent-but-present.
func TestReply_NeverContainsNull(t *testing.T) {
	a := loaded(t)
	empty := New() // nothing loaded: the emptiest answers there are

	for _, tc := range []struct {
		name string
		call func() any
	}{
		{"search miss", func() any { return a.Search("zzzznothing", SearchOptions{}) }},
		{"search miss, no lexicon", func() any { return empty.Search("zzzznothing", SearchOptions{}) }},
		{"search category", func() any { return a.Search("", SearchOptions{Category: "Aspect"}) }},
		{"search form", func() any { return a.Search("ëu", SearchOptions{Form: true}) }},
		{"parse", func() any { return a.Parse("Maţřëullait") }},
		{"parse unreadable", func() any { return a.Parse("xxxx") }},
		{"parse empty", func() any { return a.Parse("") }},
		{"parse adjunct", func() any { return a.Parse("ai") }},
		{"parse referential", func() any { return a.Parse("ex") }},
		{"affixes", func() any { return a.Affixes(0, 3) }},
		{"affixes past end", func() any { return a.Affixes(99999, 3) }},
		{"affixes, no lexicon", func() any { return empty.Affixes(0, 3) }},
		{"roots", func() any { return a.Roots(0, 3) }},
		{"roots past end", func() any { return a.Roots(99999, 3) }},
		{"positions", func() any { return a.Positions() }},
		{"topics", func() any { return a.Topics() }},
		{"categories", func() any { return a.Categories() }},
		{"table", func() any { return a.Table("Aspect") }},
		{"info", func() any { return a.Info() }},
		{"input", func() any { return a.Input("mat") }},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assertNoNull(t, Reply(tc.call(), nil))
		})
	}

	// The two that return an error alongside a value.
	t.Run("compare identical", func(t *testing.T) {
		got, err := a.Compare("mlala", "mlala")
		if err != nil {
			t.Fatal(err)
		}
		assertNoNull(t, Reply(got, nil))
	})
	t.Run("compare differing", func(t *testing.T) {
		got, err := a.Compare("Maţřëullait", "Malëuţřait")
		if err != nil {
			t.Fatal(err)
		}
		assertNoNull(t, Reply(got, nil))
	})
	t.Run("define miss", func(t *testing.T) {
		got, err := a.Define("zzzznothing", 0)
		if err != nil {
			t.Fatal(err)
		}
		assertNoNull(t, Reply(got, nil))
	})
	t.Run("compose", func(t *testing.T) {
		got, err := a.Compose("S2.CPT-ml-ERG", false)
		if err != nil {
			t.Fatal(err)
		}
		assertNoNull(t, Reply(got, nil))
	})
	t.Run("error arm", func(t *testing.T) {
		assertNoNull(t, Reply(nil, ErrNoLexicon))
	})
}

// assertNoNull walks the decoded reply and fails on any null, naming
// the path so the field that produced it is obvious.
func assertNoNull(t *testing.T, reply string) {
	t.Helper()
	dec := json.NewDecoder(bytes.NewReader([]byte(reply)))
	dec.UseNumber()
	var v any
	if err := dec.Decode(&v); err != nil {
		t.Fatalf("reply is not JSON: %v", err)
	}
	var walk func(path string, v any)
	walk = func(path string, v any) {
		switch x := v.(type) {
		case nil:
			t.Errorf("%s is null; declare it optional or send an empty value", path)
		case map[string]any:
			for k, sub := range x {
				walk(path+"."+k, sub)
			}
		case []any:
			for i, sub := range x {
				walk(fmt.Sprintf("%s[%d]", path, i), sub)
			}
		}
	}
	walk("reply", v)
}
