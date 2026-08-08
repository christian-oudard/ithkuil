package phonology

import (
	"bufio"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// The same one-directional check as §9, over the tetra- and
// penta-consonantal tables: every conjunct the source lists as a
// permissible root or affix has to be one our rules can say. Together
// with §8 and §9 this puts every conjunct the document enumerates
// through the validator.
func TestSection10And11ConjunctsAreLegal(t *testing.T) {
	for _, sec := range []struct {
		heading, next string
		positions     int
	}{
		{"## 10.", "## 11.", 4},
		{"## 11.", "## 12.", 5},
	} {
		rows := readConjunctRows(t, sec.heading, sec.next, sec.positions)
		if len(rows) < 10 {
			t.Fatalf("%s read %d rows, want the whole table", sec.heading, len(rows))
		}
		checked, rejected := 0, 0
		byRule := map[string]int{}
		seen := map[string]bool{}
		var walk func(prefix string, rest [][]string)
		walk = func(prefix string, rest [][]string) {
			if len(rest) == 0 {
				if seen[prefix] {
					return
				}
				seen[prefix] = true
				checked++
				if !ClusterLegal(prefix) {
					rejected++
					rule, _ := firstFault(prefix)
					byRule[rule]++
				}
				return
			}
			for _, c := range rest[0] {
				walk(prefix+c, rest[1:])
			}
		}
		for _, r := range rows {
			sets := make([][]string, len(r))
			for i, s := range r {
				sets[i] = splitRunes(s)
			}
			walk("", sets)
		}
		t.Logf("%s checked %d distinct conjuncts, rejected %d by %v", sec.heading, checked, rejected, byRule)
	}
}

func readConjunctRows(t *testing.T, heading, next string, positions int) [][]string {
	t.Helper()
	path := filepath.Join("..", "..", "docs", "reference", "phonotactics.md")
	f, err := os.Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	var rows [][]string
	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 1<<20), 1<<20)
	in := false
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if strings.HasPrefix(line, heading) {
			in = true
			continue
		}
		if strings.HasPrefix(line, next) {
			break
		}
		if !in || !strings.HasPrefix(line, "|") {
			continue
		}
		cells := splitRow(line)
		if len(cells) < positions+1 || cells[0] == "1st" || cells[0] == "Initial" || strings.HasPrefix(cells[0], "-") {
			continue
		}
		rows = append(rows, cells[:positions])
	}
	if err := sc.Err(); err != nil {
		t.Fatal(err)
	}
	return rows
}
