package lexicon_test

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/lexicon"
)

// docs/reference/affixes_reference.md is generated content: unlike the
// two transcriptions beside it, it has no source of its own and is
// supposed to say exactly what data/data.json says. Nothing checked
// that, and no generator survives to regenerate it from, so the two
// drifted apart in silence.
//
// The drift is what found five defects in the synced data, because the
// reference file predates the spreadsheet sync and so kept the older,
// correct reading in five places: ILT degree 7 had become the word
// "Eight", PHS degree 4 named the INTERMITTENT Phase IMT where every
// other document writes ITM, and ENS and GPJ were carrying a
// neighbouring affix's meanings outright. All are repaired in
// data/lexicon_overrides.json; see ERRATA.md -ẓd-.
//
// So this test is pointed in the direction the evidence ran. It says
// the two files agree, and either one can be the one that is wrong.

var (
	refHead   = regexp.MustCompile(`(?m)^### ([A-Z0-9]{3}) - `)
	refCs     = regexp.MustCompile(`(?m)^- \*\*Cs\*\*: -(\S+?)-$`)
	refType   = regexp.MustCompile(`(?m)^- \*\*Type\*\*: (\S+)$`)
	refDegree = regexp.MustCompile(`(?m)^\| ([1-9]) \| (.*?) \|$`)
)

func TestAffixReferenceMatchesTheData(t *testing.T) {
	path := filepath.Join("..", "..", "docs", "reference", "affixes_reference.md")
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	text := string(b)

	byKey := map[string]lexicon.AffixEntry{}
	for _, a := range loadAffixes(t) {
		byKey[a.Abbrev+"|"+a.Cs] = a
	}

	locs := refHead.FindAllStringSubmatchIndex(text, -1)
	if len(locs) != len(byKey) {
		t.Errorf("the reference has %d affix sections, the data has %d entries",
			len(locs), len(byKey))
	}
	for i, loc := range locs {
		abbrev := text[loc[2]:loc[3]]
		end := len(text)
		if i+1 < len(locs) {
			end = locs[i+1][0]
		}
		body := text[loc[1]:end]

		m := refCs.FindStringSubmatch(body)
		if m == nil {
			t.Errorf("%s: no Cs line", abbrev)
			continue
		}
		cs := m[1]
		a, ok := byKey[abbrev+"|"+cs]
		if !ok {
			t.Errorf("%s (-%s-) is in the reference but not in the data", abbrev, cs)
			continue
		}

		want := ""
		if m := refType.FindStringSubmatch(body); m != nil {
			want = m[1]
		}
		if want != a.Type {
			t.Errorf("%s (-%s-): reference says type %q, data says %q",
				abbrev, cs, want, a.Type)
		}

		got := make([]string, 9)
		for _, m := range refDegree.FindAllStringSubmatch(body, -1) {
			got[m[1][0]-'1'] = m[2]
		}
		for k := range got {
			if got[k] != a.Degrees[k] {
				t.Errorf("%s (-%s-) degree %d:\n  reference %q\n  data      %q",
					abbrev, cs, k+1, got[k], a.Degrees[k])
			}
		}
	}
}

// The reverse direction: an affix in the data with no section in the
// reference. Kept separate because the two failures want opposite
// fixes, one a deletion from the reference and one an addition to it.
func TestAffixReferenceCoversEveryAffix(t *testing.T) {
	path := filepath.Join("..", "..", "docs", "reference", "affixes_reference.md")
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	text := string(b)
	for _, a := range loadAffixes(t) {
		if !strings.Contains(text, fmt.Sprintf("### %s - ", a.Abbrev)) {
			t.Errorf("%s (-%s-) has no section in affixes_reference.md", a.Abbrev, a.Cs)
		}
	}
}
