package phonology

import (
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"testing"
)

// docs/reference/morphology.md is a transcription of Quijada's grammar
// document, and the part of it that a transcription damages silently is
// the Ithkuil itself: an English clause that loses a word still reads,
// but welacärzu written welacarzu is a different word, and nothing in
// the prose around it says so. Two thirds of the forms this test now
// pins were wrong at one time or another, in four ways — diacritics
// dropped (rö -> ro), letters transliterated to ASCII (wanžekcoë ->
// wanzekcooe, -inļ written -in| with a literal pipe), glottal stops
// dropped (hle'i -> hlei), and §7's name tables reflowed out of the
// source's grid and misaligned in the process.
//
// So: every token in the source document that carries a letter unique
// to Ithkuil romanization must appear verbatim in ours, and the reverse.
// Diacritics are what make a token identifiable as Ithkuil without a
// parser, and they are also exactly what the damage removed, which is
// why the check keys on them.
//
// The source is a PDF and lives outside the repo (see CLAUDE.md), so
// this needs its text alongside the other downloaded reference
// material. Produce it with:
//
//	pdftotext -layout net_v1_3.pdf net_v1_3.txt
//
// in $XDG_DATA_HOME/ithkuil/reference/. Without it the test skips,
// the same way the Discord word list does.

// ithkuilOnly are letters that appear in Ithkuil romanization and never
// in the document's English prose. A token holding one of these is
// Ithkuil; a token holding none of them may be either, so it is left to
// the prose and out of this test.
const ithkuilOnly = "äëöüáéíóúâêôûţḑļçžšẓřň"

// referenceExtra are the forms in our document that the grammar
// document does not contain. §3.5's gradient types and affix list come
// from the affixes document, merged in under our own section numbers
// (see the provenance table in ISSUES.md), so their C_S forms are
// legitimately absent upstream.
// Our chapter 8 has the same provenance problem on a larger scale and
// is cut wholesale below rather than listed here.
var referenceExtra = map[string]bool{
	"řn": true, // §3.5 gradient type A1, example form
	"řh": true, // §3.5 gradient type A2, named as an A1 exception
	"çţ": true, // §3.5 RPN
	"nř": true, // §3.5 AGN
}

func TestMorphologyFormsMatchSource(t *testing.T) {
	src := referenceText(t)
	ours, err := os.ReadFile(filepath.Join("..", "..", "docs", "reference", "morphology.md"))
	if err != nil {
		t.Fatal(err)
	}
	// Our chapter 8 transcribes §6.4 of the lexicon document, not this
	// one, so its number roots have nothing to match against.
	text := string(ours)
	if i := strings.Index(text, "\n## 8."); i > 0 {
		text = text[:i]
	}
	// Emphasis markers sit inside words in the markdown, where §5.8
	// once bolded the CHC affix mid-word. They are not letters.
	theirs := ithkuilForms(string(src))
	mine := ithkuilForms(strings.ReplaceAll(text, "*", ""))

	for _, w := range sortedKeys(theirs) {
		if !mine[w] {
			t.Errorf("%q is in the grammar document but not in morphology.md", w)
		}
	}
	// A form the reference prints and the source does not is a
	// deliberate correction, and the only licence for one is an
	// ERRATA.md entry. Drawing the allowlist from that file rather
	// than from a literal here means a correction cannot reach the
	// reference without its reasoning being written down.
	corrected := ithkuilForms(errataText(t))
	for _, w := range sortedKeys(mine) {
		if !theirs[w] && !referenceExtra[w] && !corrected[w] {
			t.Errorf("%q is in morphology.md but neither in the grammar "+
				"document nor named in ERRATA.md", w)
		}
	}
	if len(theirs) < 400 {
		t.Fatalf("only %d forms read from the source text; is it the right file?", len(theirs))
	}
}

// referenceText reads the extracted grammar document, or skips.
func referenceText(t *testing.T) string {
	t.Helper()
	base := os.Getenv("XDG_DATA_HOME")
	if base == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			t.Skip("no XDG_DATA_HOME and no home directory")
		}
		base = filepath.Join(home, ".local", "share")
	}
	path := filepath.Join(base, "ithkuil", "reference", "net_v1_3.txt")
	b, err := os.ReadFile(path)
	if err != nil {
		t.Skipf("%s absent; see the comment at the head of this file", path)
	}
	return string(b)
}

var referenceToken = regexp.MustCompile(`[\p{L}'’-]+`)

// ithkuilForms picks out every token carrying a letter unique to
// Ithkuil romanization and holding nothing outside the alphabet.
func ithkuilForms(text string) map[string]bool {
	text = strings.NewReplacer("’", "'", "ż", "ẓ").Replace(text)
	out := map[string]bool{}
	for _, tok := range referenceToken.FindAllString(text, -1) {
		w := strings.Trim(strings.ToLower(tok), "-'")
		if !strings.ContainsAny(w, ithkuilOnly) {
			continue
		}
		if strings.TrimFunc(w, func(r rune) bool {
			return ithkuilRunes[r]
		}) != "" {
			continue // holds a letter outside the alphabet
		}
		out[w] = true
	}
	return out
}

func sortedKeys(m map[string]bool) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

// errataText reads ERRATA.md, whose entries license the reference
// documents to depart from their sources.
func errataText(t *testing.T) string {
	t.Helper()
	b, err := os.ReadFile(filepath.Join("..", "..", "docs", "reference", "ERRATA.md"))
	if err != nil {
		t.Fatal(err)
	}
	return string(b)
}
