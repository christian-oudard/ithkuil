package phonology

import (
	"bufio"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// §9 lists the tri-consonantal conjuncts that may be a root or an
// affix, as rows of (initial set, medial consonant, third set): every
// combination of the three is a permissible conjunct. §8's grid is
// checked against our rules cell for cell; nothing checked this one,
// and it is the larger claim, some five thousand conjuncts against
// §8's 684.
//
// Only one direction is asserted. A conjunct the source lists and we
// reject is a defect in our rules, with no reading of the source that
// makes it right. The converse is not: §9 says which conjuncts may be
// a root or an affix, and §§2-7 say which may be said at all, so a
// conjunct we permit and §9 omits may simply be one no root uses.
func TestSection9ConjunctsAreLegal(t *testing.T) {
	rows := readSection9(t)
	if len(rows) < 100 {
		t.Fatalf("read %d rows from phonotactics.md §9, want the whole table", len(rows))
	}
	checked, rejected := 0, 0
	seen := map[string]bool{}
	for _, r := range rows {
		for _, a := range splitRunes(r.initials) {
			for _, c := range splitRunes(r.thirds) {
				conj := a + r.medial + c
				if seen[conj] {
					continue
				}
				seen[conj] = true
				checked++
				if !ClusterLegal(conj) {
					rejected++
					if rejected <= 20 {
						rule, why := firstFault(conj)
						t.Errorf("§9 lists %q, which our rules reject: %s %s", conj, rule, why)
					}
				}
			}
		}
	}
	t.Logf("checked %d distinct tri-consonantal conjuncts, rejected %d", checked, rejected)
}

// firstFault names the rule that rejects a cluster, for the report.
func firstFault(cluster string) (string, string) {
	rs := []rune(cluster)
	for i := 0; i+1 < len(rs); i++ {
		if rule, why := CheckProhibitedPair(rs[i], rs[i+1]); rule != "" {
			return rule, why
		}
	}
	return "", "(rejected by a whole-cluster rule)"
}

func splitRunes(s string) []string {
	out := make([]string, 0, len(s))
	for _, r := range s {
		out = append(out, string(r))
	}
	return out
}

type section9Row struct{ initials, medial, thirds string }

func readSection9(t *testing.T) []section9Row {
	t.Helper()
	path := filepath.Join("..", "..", "docs", "reference", "phonotactics.md")
	f, err := os.Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	var rows []section9Row
	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 1<<20), 1<<20)
	in := false
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if strings.HasPrefix(line, "## 9.") {
			in = true
			continue
		}
		if strings.HasPrefix(line, "## 10.") {
			break
		}
		if !in || !strings.HasPrefix(line, "|") {
			continue
		}
		cells := splitRow(line)
		if len(cells) < 4 || cells[0] == "Initial" || strings.HasPrefix(cells[0], "-") {
			continue
		}
		rows = append(rows, section9Row{initials: cells[0], medial: cells[1], thirds: cells[2]})
	}
	if err := sc.Err(); err != nil {
		t.Fatal(err)
	}
	return rows
}
