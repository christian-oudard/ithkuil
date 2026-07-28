package main

import (
	"bytes"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/store"
)

func dataFile() string {
	return store.DefaultPath()
}

// runCLI invokes the CLI in-process with the given args.
func runCLI(args ...string) (string, string, int) {
	var stdout, stderr bytes.Buffer
	code := run(args, strings.NewReader(""), &stdout, &stderr)
	return stdout.String(), stderr.String(), code
}

func TestRun_NoArgs_Usage(t *testing.T) {
	_, errOut, code := runCLI()
	if code != 2 {
		t.Errorf("expected exit 2 with no args, got %d", code)
	}
	if !strings.Contains(errOut, "usage") {
		t.Errorf("expected usage in stderr; got %q", errOut)
	}
}

func TestRun_UnknownSub(t *testing.T) {
	_, errOut, code := runCLI("frobnicate")
	if code != 2 {
		t.Errorf("expected exit 2 for unknown subcommand, got %d", code)
	}
	if !strings.Contains(errOut, "unknown subcommand") {
		t.Errorf("expected 'unknown subcommand' in stderr; got %q", errOut)
	}
}

func TestRun_Help(t *testing.T) {
	for _, flag := range []string{"help", "--help", "-h"} {
		out, _, code := runCLI(flag)
		if code != 0 {
			t.Fatalf("%q exit %d", flag, code)
		}
		for _, want := range []string{"parse", "compose", "search", "define"} {
			if !strings.Contains(out, want) {
				t.Errorf("%q missing %q", flag, want)
			}
		}
	}
}

// ---- parse ----

func TestParse_Detailed(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "parse", "malëuţřait")
	if code != 0 {
		t.Fatalf("parse exit %d", code)
	}
	for _, want := range []string{
		"PHONETIC", "SLOT", "ENCODES",
		"CATEGORY", "CODE", "NAME", "MEANING",
		"Root \"m\"", "Cr", "Vr", "Ca", "Vx₁", "Cs₁",
		"STA", "BSC", "EXS", "THM",
		"linguistic utterance",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("parse output missing %q; got %q", want, out)
		}
	}
}

func TestParse_ConcatenatedChainLabels(t *testing.T) {
	// A chain lists its dependents first and the parent last
	// (§3.1.7), so the leading formative must be labelled a
	// dependent, not a head. Keying the label off position instead
	// of the Cc marker labelled every member "[head]".
	out, _, code := runCLI("-data", dataFile(), "parse", "hakšal-uḑfarf")
	if code != 0 {
		t.Fatalf("parse exit %d; got %q", code, out)
	}
	if !strings.Contains(out, "[Type1 dependent]") {
		t.Errorf("chain missing dependent label; got %q", out)
	}
	if strings.Count(out, "[head]") != 1 {
		t.Errorf("want exactly one [head] in chain; got %q", out)
	}
}

func TestParse_UnclassifiedShowsDiagnostic(t *testing.T) {
	// An unreadable word used to print only "?mavẓorf". The shape
	// split survives even when the grammatical decode fails, so both
	// the decoder's complaint and the slot split are available and
	// worth showing. "mavẓorf" is "mavẓorff" with the §3.6.1 Ca
	// gemination removed, which forces "vẓ" to be read as the Ca.
	out, _, code := runCLI("-data", dataFile(), "parse", "mavẓorf")
	if code != 0 {
		t.Fatalf("parse exit %d; got %q", code, out)
	}
	for _, want := range []string{
		"(unclassified)",
		`unrecognized Ca "vẓ"`,
		"PHONETIC", "SLOT",
		"Cr", "Vr", "Ca",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("diagnostic missing %q; got %q", want, out)
		}
	}
	// No grammatical values are known, so the column must be gone
	// rather than printed empty over the rows.
	if strings.Contains(out, "ENCODES") {
		t.Errorf("empty ENCODES column should be dropped; got %q", out)
	}
}

func TestParse_ASCIIInput(t *testing.T) {
	// ASCII typing convention: "maleeut,rqait" must normalize to
	// "malëuţřait" before parsing, so the slot breakdown shows the
	// right affixes (ţř → SYS at degree 5, not literal "t,rq").
	out, _, code := runCLI("-data", dataFile(), "parse", "maleeut,rqait")
	if code != 0 {
		t.Fatalf("parse exit %d", code)
	}
	for _, want := range []string{"malëuţřait", "SYS", "DEG5"} {
		if !strings.Contains(out, want) {
			t.Errorf("ASCII parse missing %q; got %q", want, out)
		}
	}
	for _, bad := range []string{"t,rq", "DEG0"} {
		if strings.Contains(out, bad) {
			t.Errorf("ASCII parse should not contain %q; got %q", bad, out)
		}
	}
}

func TestParse_InvalidWord(t *testing.T) {
	// Phonotactically invalid input must fail loudly instead of
	// rendering a garbage slot breakdown.
	out, errOut, code := runCLI("parse", "akxq")
	if code != 1 {
		t.Fatalf("parse invalid exit = %d, want 1; out=%q err=%q", code, out, errOut)
	}
	if !strings.Contains(errOut, "non-Ithkuil characters") {
		t.Errorf("expected non-Ithkuil error; got stderr=%q", errOut)
	}
}

func TestParse_ReportsTheRuleBroken(t *testing.T) {
	// Parsing is also the validation command, so it has to name the
	// rule the word breaks, not merely refuse the word.
	_, errOut, code := runCLI("parse", "akx")
	if code != 1 {
		t.Fatalf("parse akx exit = %d, want 1", code)
	}
	if !strings.Contains(errOut, "2.3") {
		t.Errorf("expected rule 2.3 in stderr; got %q", errOut)
	}
}

func TestParse_ShortValidates(t *testing.T) {
	// The one-line view runs the same phonotactic check; an invalid
	// word must not slip through as a gloss line.
	out, errOut, code := runCLI("parse", "--short", "akx")
	if code != 1 {
		t.Fatalf("parse --short akx exit = %d, want 1; out=%q", code, out)
	}
	if !strings.Contains(errOut, "2.3") {
		t.Errorf("expected rule 2.3 in stderr; got %q", errOut)
	}
}

func TestParse_Short(t *testing.T) {
	out, _, code := runCLI("parse", "--short", "malëuţřait")
	if code != 0 {
		t.Fatalf("parse --short exit %d", code)
	}
	if !strings.Contains(out, "Form") {
		t.Errorf("--short should include type tag; got %q", out)
	}
}

func TestParse_ShortFlag(t *testing.T) {
	// -s should behave the same as --short.
	out, _, code := runCLI("parse", "-s", "malëuţřait")
	if code != 0 {
		t.Fatalf("parse -s exit %d", code)
	}
	if !strings.Contains(out, "Form") {
		t.Errorf("-s should include type tag; got %q", out)
	}
}

func TestParse_Stdin(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run([]string{"parse", "--short"}, strings.NewReader("amlala\n"), &stdout, &stderr)
	if code != 0 {
		t.Fatalf("parse (stdin) exit %d; stderr=%s", code, stderr.String())
	}
	if !strings.Contains(stdout.String(), "amlala") {
		t.Errorf("stdin path missing word; got %q", stdout.String())
	}
}

// ---- compose ----

func TestCompose_Expression(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "compose", "S2/CPT-ml-ERG")
	if code != 0 {
		t.Fatalf("compose exit %d", code)
	}
	lines := strings.Split(strings.TrimSpace(out), "\n")
	if len(lines) != 2 {
		t.Fatalf("compose output = %q, want 2 lines (surface + gloss)", out)
	}
	if lines[0] != "wimlo" {
		t.Errorf("compose surface = %q, want canonical \"wimlo\" (Cc shortcut form)", lines[0])
	}
	if !strings.Contains(lines[1], "-ml-") || !strings.Contains(lines[1], "ERG") {
		t.Errorf("compose gloss = %q, want it to mention -ml- and ERG", lines[1])
	}
}

func TestCompose_BareRoot(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "compose", "ml")
	if code != 0 {
		t.Fatalf("compose exit %d", code)
	}
	lines := strings.Split(strings.TrimSpace(out), "\n")
	if len(lines) != 2 {
		t.Fatalf("compose output = %q, want 2 lines", out)
	}
}

func TestCompose_BadValue(t *testing.T) {
	_, errOut, code := runCLI("compose", "ml-XYZZY")
	if code == 0 {
		t.Error("expected non-zero exit on unknown abbreviation")
	}
	if !strings.Contains(errOut, "unknown") {
		t.Errorf("expected 'unknown' in stderr; got %q", errOut)
	}
}

func TestCompose_NoArg(t *testing.T) {
	_, errOut, code := runCLI("compose")
	if code != 2 {
		t.Errorf("expected exit 2 with no expression, got %d", code)
	}
	if !strings.Contains(errOut, "usage") {
		t.Errorf("expected usage in stderr; got %q", errOut)
	}
}

// ---- search ----

func TestSearch_ListCategories(t *testing.T) {
	out, _, code := runCLI("search")
	if code != 0 {
		t.Fatalf("search exit %d", code)
	}
	for _, want := range []string{"categories:", "Case", "Bias", "Aspect"} {
		if !strings.Contains(out, want) {
			t.Errorf("category list missing %q; got %q", want, out)
		}
	}
}

func TestSearch_QueryExact(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "search", "THM", "--exact")
	if code != 0 {
		t.Fatalf("search --exact exit %d", code)
	}
	if !strings.Contains(out, "THM") {
		t.Errorf("--exact missing THM; got %q", out)
	}
}

func TestSearch_FormMode(t *testing.T) {
	out, _, code := runCLI("search", "--form", "a")
	if code != 0 {
		t.Fatalf("search --form exit %d", code)
	}
	if !strings.Contains(out, "THM") {
		t.Errorf("--form a should mention THM; got %q", out)
	}
	// A surface form is a grammar question; the lexicon has no
	// answer to "what does this vowel encode".
	if strings.Contains(out, "Roots:") {
		t.Errorf("--form should not search the lexicon; got %q", out)
	}
}

func TestSearch_FlagAfterQuery(t *testing.T) {
	// Flags are accepted in any position. Go's flag package stops at
	// the first positional, which used to drop the flag in silence.
	out, _, code := runCLI("-data", dataFile(), "search", "a", "--form")
	if code != 0 {
		t.Fatalf("search a --form exit %d", code)
	}
	if strings.Contains(out, "Roots:") {
		t.Errorf("--form after the query was ignored; got %q", out)
	}
}

func TestSearch_Category(t *testing.T) {
	out, _, code := runCLI("search", "--category", "Bias")
	if code != 0 {
		t.Fatalf("search --category Bias exit %d", code)
	}
	// At least 60 biases.
	count := strings.Count(out, "\nBias")
	if count < 50 {
		t.Errorf("expected many Bias rows, got %d; out=%q", count, out)
	}
}

func TestSearch_CategoryQuery(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "search", "please", "--category", "Bias")
	if code != 0 {
		t.Fatalf("search bias please exit %d", code)
	}
	if !strings.Contains(out, "SOL") {
		t.Errorf("expected SOL for 'please' bias; got %q", out)
	}
}

func TestSearch_GrammarBeforeLexicon(t *testing.T) {
	// A short query is more often an abbreviation than a root, so the
	// grammar half is answered first.
	out, _, code := runCLI("-data", dataFile(), "search", "ERG")
	if code != 0 {
		t.Fatalf("search ERG exit %d", code)
	}
	gram := strings.Index(out, "Ergative")
	lex := strings.Index(out, "Roots:")
	if gram < 0 {
		t.Fatalf("search ERG missing the Ergative case; got %q", out)
	}
	if lex >= 0 && lex < gram {
		t.Errorf("lexicon hits printed before grammar hits; got %q", out)
	}
}

func TestSearch_LexiconRoot(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "search", "yellow")
	if code != 0 {
		t.Fatalf("search exit %d", code)
	}
	if !strings.Contains(out, "Roots:") || !strings.Contains(out, "-ml-") {
		t.Errorf("search yellow missing root -ml-; got %q", out)
	}
}

func TestSearch_LexiconAffix(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "search", "negation")
	if code != 0 {
		t.Fatalf("search negation exit %d", code)
	}
	if !strings.Contains(out, "Affixes:") || !strings.Contains(out, "NEG") {
		t.Errorf("search negation should find affix NEG; got %q", out)
	}
}

func TestSearch_NoMatches(t *testing.T) {
	// An empty section is noise; nothing found says so once.
	out, _, code := runCLI("-data", dataFile(), "search", "zzzznotaword")
	if code != 0 {
		t.Fatalf("search exit %d", code)
	}
	if !strings.Contains(out, "no matches") {
		t.Errorf("expected 'no matches'; got %q", out)
	}
	for _, bad := range []string{"Roots:", "Affixes:", "CATEGORY"} {
		if strings.Contains(out, bad) {
			t.Errorf("empty section %q should not print; got %q", bad, out)
		}
	}
}

// ---- define ----

func TestDefine_Found(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "define", "crisis")
	if code != 0 {
		t.Fatalf("define exit %d", code)
	}
	if !strings.Contains(out, "jd,") {
		t.Errorf("define crisis should reach root -jḑ-; got %q", out)
	}
}

func TestDefine_Missing(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "define", "zzzznotaword")
	if code != 1 {
		t.Errorf("expected exit 1 for an unnamed word, got %d", code)
	}
	if !strings.Contains(out, "no root names this") {
		t.Errorf("got %q", out)
	}
}

func TestDefine_NoArg(t *testing.T) {
	_, errOut, code := runCLI("define")
	if code != 2 {
		t.Errorf("expected exit 2 with no word, got %d", code)
	}
	if !strings.Contains(errOut, "usage") {
		t.Errorf("got %q", errOut)
	}
}
