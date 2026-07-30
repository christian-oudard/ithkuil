package phonology

import (
	"bufio"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// §8 of the phonotactics document tabulates every bi-consonantal
// conjunct that can be a C_R root or a C_S affix. It is a derived
// table: §§1-7 give the rules, and §8 is what they produce. So it is
// the one place the document checks itself, and the one place we can
// check our reading of the rules against Quijada's own arithmetic
// rather than against our own transcription.
//
// The grid in the PDF is drawn as coloured squares rather than text,
// so docs/reference/phonotactics.md carries it as a + / . matrix
// recovered from the cell fills. This test reads that matrix back and
// regenerates it from CheckProhibitedPair.
//
// Two things it pins:
//
//  1. Our rules agree with Quijada's grid in 26 of the 27 rows, cell
//     for cell. That is 810 independent checks on the transcription of
//     §§2-7, and it is why a change to any pair rule shows up here.
//
//  2. The ç row is the exception, and it is the source disagreeing with
//     itself rather than with us. §2.10 says ç "cannot be ... followed
//     by a voiced sibilant affricate (ż, j)"; the grid marks çẓ and çj
//     permissible. We follow the prose, since it states a reason and
//     the grid states none, so we bar two cells the grid allows.
var section8ExpectedDiff = map[string]bool{"çẓ": true, "çj": true}

func TestSection8GridMatchesRules(t *testing.T) {
	grid, printed := readSection8(t)
	if len(grid) != 27 {
		t.Fatalf("read %d rows from phonotactics.md §8, want 27", len(grid))
	}

	seconds := append(append([]string{}, section8Initials...), "w", "y", "h")
	total, disagree := 0, 0
	for _, a := range section8Initials {
		for _, b := range seconds {
			pair := a + b
			doc := grid[a][b]
			ours := ClusterLegal(pair)
			if doc {
				total++
			}
			if doc == ours {
				continue
			}
			if doc && !ours && section8ExpectedDiff[pair] {
				disagree++
				continue
			}
			t.Errorf("%s: §8 says permissible=%v, our rules say %v", pair, doc, ours)
		}
	}
	if disagree != len(section8ExpectedDiff) {
		t.Errorf("expected %d cells to differ from §8, got %d",
			len(section8ExpectedDiff), disagree)
	}
	if total != 684 {
		t.Errorf("§8 grid holds %d permissible forms, want 684", total)
	}

	// The source prints a total beside each row. Five of them are one
	// below what the same row's cells show. Pinned so that a correction
	// upstream, or a mis-transcription here, is visible.
	shortRows := map[string]bool{"ç": true, "c": true, "č": true, "ẓ": true, "j": true}
	for _, a := range section8Initials {
		n := 0
		for _, b := range seconds {
			if grid[a][b] {
				n++
			}
		}
		want := n
		if shortRows[a] {
			want = n - 1
		}
		if printed[a] != want {
			t.Errorf("row %s: %d cells, printed total %d, want %d", a, n, printed[a], want)
		}
	}
}

var section8Initials = strings.Fields("p t k b d g f ţ ç x v ḑ ļ s š z ž c č ẓ j l r ř m n ň")

// readSection8 parses the § 8 matrix out of the reference document.
func readSection8(t *testing.T) (map[string]map[string]bool, map[string]int) {
	t.Helper()
	path := filepath.Join("..", "..", "docs", "reference", "phonotactics.md")
	f, err := os.Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()

	grid := map[string]map[string]bool{}
	printed := map[string]int{}
	var header []string
	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 1<<20), 1<<20)
	in := false
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if strings.HasPrefix(line, "## 8.") {
			in = true
			continue
		}
		if strings.HasPrefix(line, "## 9.") {
			break
		}
		if !in || !strings.HasPrefix(line, "|") {
			continue
		}
		cells := splitRow(line)
		if header == nil {
			if len(cells) > 30 && cells[1] == "p" {
				header = cells[1 : len(cells)-2]
			}
			continue
		}
		name := strings.Trim(cells[0], "*")
		if name == "" || strings.HasPrefix(cells[0], "-") {
			continue
		}
		row := map[string]bool{}
		for i, h := range header {
			row[h] = cells[1+i] == "+"
		}
		grid[name] = row
		printed[name] = atoi(t, strings.Trim(cells[len(cells)-1], "*"))
	}
	if err := sc.Err(); err != nil {
		t.Fatal(err)
	}
	return grid, printed
}

func splitRow(line string) []string {
	parts := strings.Split(strings.Trim(line, "|"), "|")
	for i := range parts {
		parts[i] = strings.TrimSpace(parts[i])
	}
	return parts
}

func atoi(t *testing.T, s string) int {
	t.Helper()
	n := 0
	for _, r := range s {
		if r < '0' || r > '9' {
			t.Fatalf("not a number: %q", s)
		}
		n = n*10 + int(r-'0')
	}
	return n
}
