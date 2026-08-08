package phonology

import (
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"testing"
)

// docs/reference/ERRATA.md records every place our output departs from
// the published sources, or resolves a question they leave open. It
// outlives ISSUES.md, which is a worklist, so it is the thing a
// downstream implementation reads to reproduce what we do.
//
// A hand-maintained index of code decisions drifts from the code. That
// is the failure this repository has spent a lot of effort repairing in
// the reference documents themselves, so the index is checked rather
// than trusted: every Where: pointer must name a file that exists and a
// line that is inside it, and every entry must carry the four fields
// the file's own preamble promises.
//
// What this cannot check is whether the pointer still points at the
// right code. Moving a function without updating the entry leaves a
// valid line number aimed at nothing in particular. The Decision text
// is the record; the pointer is a convenience.

// Entries are named for what they rule on rather than numbered, so a
// citation carries its own meaning and two people adding an entry at
// once cannot collide on an id. That is a section of Quijada's for the
// grammar, and the C_R at issue for the lexicon, which has no sections.
//
// The id pattern admits a span (§§9-11) as well as a single section,
// because a defect can belong to a run of them. It did not at first,
// and the §§9-11 entry sat in the file unchecked: no heading matched,
// so every check below simply skipped it. Widening the pattern is what
// found that, which is the argument for a guard naming what it covers
// rather than counting what it happens to see.
var (
	errataEntry  = regexp.MustCompile("(?m)^### (§§?[0-9][0-9.-]*[0-9]|-[^ ]+-) — (.+)$")
	errataTarget = regexp.MustCompile("`([a-z][a-z0-9_/]*\\.go):(\\d+)`")
	errataStatus = regexp.MustCompile(`(?m)^\*\*Status\.\*\* ` + "`" + `(adopted|proposed|implemented)` + "`")
)

// wantEntries is the number of entries the file holds. A pattern that
// stops matching a heading makes every other check pass vacuously, so
// the count is pinned rather than inferred: this is the failure the
// §§9-11 entry actually had.
const wantEntries = 31

func TestErrataEntriesAreWellFormed(t *testing.T) {
	repo := filepath.Join("..", "..")
	text := readErrata(t)

	// Split on entry headings so each entry is checked on its own.
	locs := errataEntry.FindAllStringSubmatchIndex(text, -1)
	if len(locs) != wantEntries {
		t.Errorf("ERRATA.md holds %d entries the heading pattern matches, "+
			"want %d; a heading it stops seeing is checked by nothing",
			len(locs), wantEntries)
	}
	seen := map[string]bool{}
	for i, loc := range locs {
		id := text[loc[2]:loc[3]]
		title := text[loc[4]:loc[5]]
		end := len(text)
		if i+1 < len(locs) {
			end = locs[i+1][0]
		}
		body := text[loc[1]:end]

		// The key is the id, so two entries sharing one would make a
		// citation ambiguous. Split it or widen the reference instead.
		if seen[id] {
			t.Errorf("%s: two entries rule on this", id)
		}
		seen[id] = true

		for _, want := range []string{"Source", "Decision", "Status", "Where"} {
			if !strings.Contains(body, "**"+want+".**") {
				t.Errorf("%s (%s): no %s: field", id, title, want)
			}
		}
		if !errataStatus.MatchString(body) {
			t.Errorf("%s: Status is not one of adopted/proposed/implemented", id)
		}
		for _, m := range errataTarget.FindAllStringSubmatch(body, -1) {
			checkTarget(t, repo, id, m[1], m[2])
		}
	}
}

// TestErrataCoversTheCodeThatCitesIt is the other direction: a comment
// naming an errata entry must find it in the file.
func TestErrataCoversTheCodeThatCitesIt(t *testing.T) {
	text := readErrata(t)
	ids := map[string]bool{}
	for _, m := range errataEntry.FindAllStringSubmatch(text, -1) {
		ids[m[1]] = true
	}

	cited := map[string][]string{}
	cite := regexp.MustCompile("ERRATA\\.md (§§?[0-9][0-9.-]*[0-9]|-[^ ]+-)")
	err := filepath.Walk(filepath.Join("..", ""), func(path string, info os.FileInfo, err error) error {
		if err != nil || info.IsDir() || !strings.HasSuffix(path, ".go") {
			return err
		}
		b, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, m := range cite.FindAllStringSubmatch(string(b), -1) {
			id := m[1]
			cited[id] = append(cited[id], path)
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	for _, id := range sortedIDs(cited) {
		if !ids[id] {
			t.Errorf("%s cited by %v but not in ERRATA.md", id, cited[id])
		}
	}
}

func readErrata(t *testing.T) string {
	t.Helper()
	b, err := os.ReadFile(filepath.Join("..", "..", "docs", "reference", "ERRATA.md"))
	if err != nil {
		t.Fatal(err)
	}
	return string(b)
}

// checkTarget verifies a Where: pointer names a file that exists and a
// line inside it.
func checkTarget(t *testing.T, repo, id, file, line string) {
	t.Helper()
	path := filepath.Join(repo, "code", file)
	b, err := os.ReadFile(path)
	if err != nil {
		t.Errorf("%s: Where: names %s, which does not exist", id, file)
		return
	}
	n := 0
	for _, r := range line {
		n = n*10 + int(r-'0')
	}
	if got := strings.Count(string(b), "\n") + 1; n > got {
		t.Errorf("%s: Where: names %s:%s, but the file has %d lines", id, file, line, got)
	}
}

func sortedIDs(m map[string][]string) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}
