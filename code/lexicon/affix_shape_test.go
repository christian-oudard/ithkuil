package lexicon_test

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/store"
)

// The affix half of the lexicon, held against the shape every row is
// supposed to have. All three checks here were failing when they were
// written, which is why they exist: the community spreadsheet is a
// transcription of Quijada's affix document and loses things in ways
// that are invisible from inside the sheet, because a broken row still
// looks like a row.
//
// The repairs live in data/lexicon_overrides.json rather than in
// data.json, because a hand-edit to a synced row does not survive the
// next sync. Two had already been lost that way before the overrides
// existed. See ERRATA.md -ẓd-, -sļ-, -dg- and -çx-.

func loadAffixes(t *testing.T) map[string]lexicon.AffixEntry {
	t.Helper()
	s, err := store.Open(store.DefaultPath())
	if err != nil {
		t.Skipf("no store: %v", err)
	}
	defer s.Close()
	lex, err := store.LoadLexicon(s)
	if err != nil {
		t.Fatal(err)
	}
	return lex.Affixes
}

// An affix is nine degrees. A blank one is not a degree with no
// meaning, it is a meaning that did not arrive: six Functional Group
// affixes reached us with all nine empty, and MET with all nine empty
// because the block above it had been pasted a row low.
func TestAffix_EveryDegreeHasAMeaning(t *testing.T) {
	for _, a := range loadAffixes(t) {
		if len(a.Degrees) != 9 {
			t.Errorf("%s (-%s-): %d degrees, want 9", a.Abbrev, a.Cs, len(a.Degrees))
			continue
		}
		for i, d := range a.Degrees {
			if strings.TrimSpace(d) == "" {
				t.Errorf("%s (-%s-): degree %d is blank", a.Abbrev, a.Cs, i+1)
			}
		}
	}
}

// The gradient type is one of seven values. It arrives as free text
// from a spreadsheet cell, and ANG's held a whole nine-item degree
// list that someone had drafted in the wrong column, which is not a
// thing any consumer can do anything with. The trailing asterisk is
// §3.5.0.1's mark for an affix that also has a C_R root.
func TestAffix_GradientTypeIsOneOfTheSeven(t *testing.T) {
	valid := map[string]bool{
		"0": true, "A1": true, "A2": true, "B": true,
		"C": true, "D1": true, "D2": true,
	}
	for _, a := range loadAffixes(t) {
		bare := strings.TrimSuffix(a.Type, "*")
		if !valid[bare] {
			t.Errorf("%s (-%s-): gradient type %q is not one of the seven",
				a.Abbrev, a.Cs, a.Type)
		}
	}
}

// Two affixes must not share a degree list. Identical nine-degree
// lists on different C_S values is the signature of the paste shift
// that put MET's meanings on GPJ and GPJ's on ENS, and it survived a
// sync unnoticed because each row on its own reads perfectly well.
//
// SPT is the one pair left, and it is not a paste error. Quijada
// prints the affix once under two C_S forms, "-rw/-ry", the only
// heading in 141 pages to carry two; the sheet's rows are keyed by C_S
// and so had to split it in half. It is named here rather than
// repaired because what to do about one affix with two forms is open,
// as ISSUES.md A9.
func TestAffix_DegreeListsAreNotShared(t *testing.T) {
	known := map[string]bool{"SPT|SPT": true}
	seen := map[string]lexicon.AffixEntry{}
	for _, a := range loadAffixes(t) {
		key := strings.Join(a.Degrees, "\x00")
		if prev, dup := seen[key]; dup {
			pair := prev.Abbrev + "|" + a.Abbrev
			if !known[pair] && !known[a.Abbrev+"|"+prev.Abbrev] {
				t.Errorf("%s (-%s-) and %s (-%s-) have identical degree lists",
					prev.Abbrev, prev.Cs, a.Abbrev, a.Cs)
			}
			continue
		}
		seen[key] = a
	}
}
