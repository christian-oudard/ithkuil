package phonology

import (
	"testing"
)

// TestRootConjunctRows_MatchTheDocument holds the embedded copy against
// the reference document, which stays the source. The rows were pulled
// out of it mechanically, so the risk is not that the transcription was
// wrong on the day but that the two drift afterwards: a correction to
// phonotactics.md that never reaches the Go, or the other way round.
func TestRootConjunctRows_MatchTheDocument(t *testing.T) {
	var want [][]string
	for _, sec := range []struct {
		heading, next string
		positions     int
	}{
		{"## 9.", "## 10.", 3},
		{"## 10.", "## 11.", 4},
		{"## 11.", "## 12.", 5},
	} {
		rows := readConjunctRows(t, sec.heading, sec.next, sec.positions)
		if len(rows) < 10 {
			t.Fatalf("%s read %d rows, want the whole table", sec.heading, len(rows))
		}
		want = append(want, rows...)
	}
	if len(want) != len(rootConjunctRows) {
		t.Fatalf("document has %d rows, rootConjunctRows has %d", len(want), len(rootConjunctRows))
	}
	for i, w := range want {
		got := rootConjunctRows[i]
		if len(got) != len(w) {
			t.Errorf("row %d: %d positions, want %d", i, len(got), len(w))
			continue
		}
		for j := range w {
			if got[j] != w[j] {
				t.Errorf("row %d position %d: have %q, document says %q", i, j, got[j], w[j])
			}
		}
	}
}

// TestRootConjunctLegal_Excludes is the direction the §9 test could not
// check. TestSection9ConjunctsAreLegal expands the table and asserts
// every entry is sayable, which tests what the table admits and never
// what it excludes, so it would pass just as well against a predicate
// that said yes to everything.
//
// fbm is the case that prompted this. Both its pairs are permissible
// per §8, so ClusterLegal says yes; §9's row for medial b whose
// initials include f permits only vlrwyř third, which excludes m, and
// a speaker asked to say it reported it impossible.
func TestRootConjunctLegal_Excludes(t *testing.T) {
	for _, c := range []string{"fbm", "fbn", "fbň"} {
		if !ClusterLegal(c) {
			t.Errorf("%q: this test is not saying anything unless ClusterLegal admits it", c)
		}
		if RootConjunctLegal(c) {
			t.Errorf("%q is in no §9 row and must not be a root or affix form", c)
		}
	}
}

// TestRootConjunctLegal_AdmitsTheTables requires every conjunct §§9-11
// enumerate to pass, which is the other direction and the one that
// catches a row transcribed into the wrong position.
func TestRootConjunctLegal_AdmitsTheTables(t *testing.T) {
	var checked int
	for _, row := range rootConjunctRows {
		var walk func(prefix string, rest []string)
		walk = func(prefix string, rest []string) {
			if len(rest) == 0 {
				checked++
				if !RootConjunctLegal(prefix) {
					t.Errorf("§§9-11 list %q, which RootConjunctLegal rejects", prefix)
				}
				return
			}
			for _, c := range splitRunes(rest[0]) {
				walk(prefix+c, rest[1:])
			}
		}
		walk("", row)
	}
	if checked < 1000 {
		t.Fatalf("only %d conjuncts checked; the tables did not load", checked)
	}
	t.Logf("checked %d conjuncts from §§9-11", checked)
}

// TestRootConjunctLegal_ShortFormsDeferToTheRules pins that one and two
// consonants are judged by §§2-7 rather than by the tables, §8 being
// derived from those rules rather than independent of them.
func TestRootConjunctLegal_ShortFormsDeferToTheRules(t *testing.T) {
	for _, c := range []string{"m", "ml", "ţř"} {
		if !RootConjunctLegal(c) {
			t.Errorf("%q is a permissible root form", c)
		}
	}
	if RootConjunctLegal("nň") {
		t.Error("§2.23 prohibits nň, so it is not a root form either")
	}
}
