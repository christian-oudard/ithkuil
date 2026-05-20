package main

import (
	"bytes"
	"path/filepath"
	"strings"
	"testing"
)

func dataDir() string {
	return filepath.Join("..", "..", "data")
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
		for _, want := range []string{"analyze", "compose", "diff", "grammar", "lexicon", "validate"} {
			if !strings.Contains(out, want) {
				t.Errorf("%q missing %q", flag, want)
			}
		}
	}
}

// ---- analyze ----

func TestAnalyze_Detailed(t *testing.T) {
	out, _, code := runCLI("-lex", dataDir(), "analyze", "malëuţřait")
	if code != 0 {
		t.Fatalf("analyze exit %d", code)
	}
	for _, want := range []string{
		"PHONETIC", "SLOT", "ENCODES",
		"CATEGORY", "CODE", "NAME", "MEANING",
		"Root \"m\"", "Cr", "Vr", "Ca", "Vx₁", "Cs₁",
		"STA", "BSC", "EXS", "THM",
		"linguistic utterance",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("analyze output missing %q; got %q", want, out)
		}
	}
}

func TestAnalyze_Short(t *testing.T) {
	out, _, code := runCLI("analyze", "--short", "malëuţřait")
	if code != 0 {
		t.Fatalf("analyze --short exit %d", code)
	}
	if !strings.Contains(out, "Form") {
		t.Errorf("--short should include type tag; got %q", out)
	}
}

func TestAnalyze_ShortFlag(t *testing.T) {
	// -s should behave the same as --short.
	out, _, code := runCLI("analyze", "-s", "malëuţřait")
	if code != 0 {
		t.Fatalf("analyze -s exit %d", code)
	}
	if !strings.Contains(out, "Form") {
		t.Errorf("-s should include type tag; got %q", out)
	}
}

func TestAnalyze_Polygraph(t *testing.T) {
	out, _, code := runCLI("analyze", "--polygraph", "amlala", "la", "řřx")
	if code != 0 {
		t.Fatalf("analyze --polygraph exit %d", code)
	}
	for _, want := range []string{"Slot II", "type", "Form"} {
		if !strings.Contains(out, want) {
			t.Errorf("--polygraph missing %q; got %q", want, out)
		}
	}
}

func TestAnalyze_Stdin(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run([]string{"analyze", "--short"}, strings.NewReader("amlala\n"), &stdout, &stderr)
	if code != 0 {
		t.Fatalf("analyze (stdin) exit %d; stderr=%s", code, stderr.String())
	}
	if !strings.Contains(stdout.String(), "amlala") {
		t.Errorf("stdin path missing word; got %q", stdout.String())
	}
}

// ---- compose ----

func TestCompose_NamedFlags(t *testing.T) {
	out, _, code := runCLI("compose", "--stem", "S2", "--version", "CPT", "--case", "ERG", "ml")
	if code != 0 {
		t.Fatalf("compose exit %d", code)
	}
	if got := strings.TrimSpace(out); got != "imlalo" {
		t.Errorf("compose = %q, want \"imlalo\"", got)
	}
}

func TestCompose_BadValue(t *testing.T) {
	_, errOut, code := runCLI("compose", "--case", "XYZZY", "ml")
	if code == 0 {
		t.Error("expected non-zero exit on bad case value")
	}
	if !strings.Contains(errOut, "unknown") {
		t.Errorf("expected 'unknown' in stderr; got %q", errOut)
	}
}

func TestCompose_NoRoot(t *testing.T) {
	_, errOut, code := runCLI("compose")
	if code != 2 {
		t.Errorf("expected exit 2 with no root, got %d", code)
	}
	if !strings.Contains(errOut, "usage") {
		t.Errorf("expected usage in stderr; got %q", errOut)
	}
}

// ---- diff ----

func TestDiff_TwoWords(t *testing.T) {
	out, _, code := runCLI("diff", "malëuţřait", "mal")
	if code != 0 {
		t.Fatalf("diff exit %d; out=%q", code, out)
	}
	for _, want := range []string{"Slot III(Cr)", "→", "rows changed"} {
		if !strings.Contains(out, want) {
			t.Errorf("diff output missing %q; got %q", want, out)
		}
	}
}

func TestDiff_Sentence(t *testing.T) {
	out, _, code := runCLI("diff", "amlala", "la", "--", "imlalo", "la")
	if code != 0 {
		t.Fatalf("diff sentence exit %d; out=%q", code, out)
	}
	for _, want := range []string{"[1]", "[2]", "amlala", "imlalo"} {
		if !strings.Contains(out, want) {
			t.Errorf("diff sentence output missing %q; got %q", want, out)
		}
	}
}

func TestDiff_NoArgs(t *testing.T) {
	_, errOut, code := runCLI("diff")
	if code != 2 {
		t.Errorf("expected exit 2 for empty diff, got %d", code)
	}
	if !strings.Contains(errOut, "usage") {
		t.Errorf("expected usage; got %q", errOut)
	}
}

// ---- grammar ----

func TestGrammar_ListCategories(t *testing.T) {
	out, _, code := runCLI("grammar")
	if code != 0 {
		t.Fatalf("grammar exit %d", code)
	}
	for _, want := range []string{"categories:", "Case", "Bias", "Aspect"} {
		if !strings.Contains(out, want) {
			t.Errorf("category list missing %q; got %q", want, out)
		}
	}
}

func TestGrammar_QueryExact(t *testing.T) {
	out, _, code := runCLI("grammar", "THM", "--exact")
	if code != 0 {
		t.Fatalf("grammar --exact exit %d", code)
	}
	if !strings.Contains(out, "THM") {
		t.Errorf("--exact missing THM; got %q", out)
	}
}

func TestGrammar_FormMode(t *testing.T) {
	out, _, code := runCLI("grammar", "a", "--form")
	if code != 0 {
		t.Fatalf("grammar --form exit %d", code)
	}
	if !strings.Contains(out, "THM") {
		t.Errorf("--form a should mention THM; got %q", out)
	}
}

func TestGrammar_Category(t *testing.T) {
	out, _, code := runCLI("grammar", "--category", "Bias")
	if code != 0 {
		t.Fatalf("grammar --category Bias exit %d", code)
	}
	// At least 60 biases.
	count := strings.Count(out, "\nBias")
	if count < 50 {
		t.Errorf("expected many Bias rows, got %d; out=%q", count, out)
	}
}

func TestGrammar_CategoryQuery(t *testing.T) {
	out, _, code := runCLI("grammar", "please", "--category", "Bias")
	if code != 0 {
		t.Fatalf("grammar bias please exit %d", code)
	}
	if !strings.Contains(out, "SOL") {
		t.Errorf("expected SOL for 'please' bias; got %q", out)
	}
}

// ---- lexicon ----

func TestLexicon_Root(t *testing.T) {
	out, _, code := runCLI("-lex", dataDir(), "lexicon", "yellow", "--kind", "root")
	if code != 0 {
		t.Fatalf("lexicon exit %d", code)
	}
	if !strings.Contains(out, "Roots:") || !strings.Contains(out, "-ml-") {
		t.Errorf("lexicon yellow root missing -ml-; got %q", out)
	}
}

func TestLexicon_Affix(t *testing.T) {
	out, _, code := runCLI("-lex", dataDir(), "lexicon", "negation", "--kind", "affix")
	if code != 0 {
		t.Fatalf("lexicon --kind=affix exit %d", code)
	}
	if !strings.Contains(out, "NEG") {
		t.Errorf("lexicon negation should find NEG; got %q", out)
	}
}

func TestLexicon_Both(t *testing.T) {
	out, _, code := runCLI("-lex", dataDir(), "lexicon", "yellow")
	if code != 0 {
		t.Fatalf("lexicon both exit %d", code)
	}
	if !strings.Contains(out, "Roots:") || !strings.Contains(out, "Affixes:") {
		t.Errorf("lexicon both sections missing; got %q", out)
	}
}

// ---- validate ----

func TestValidate_Good(t *testing.T) {
	out, _, code := runCLI("validate", "amlala")
	if code != 0 {
		t.Fatalf("validate exit %d", code)
	}
	if !strings.Contains(out, "OK") {
		t.Errorf("expected OK on good word; got %q", out)
	}
}

func TestValidate_Bad(t *testing.T) {
	out, _, code := runCLI("validate", "akx")
	if code != 1 {
		t.Errorf("expected exit 1 on bad word, got %d", code)
	}
	if !strings.Contains(out, "2.3") {
		t.Errorf("expected rule 2.3 in output; got %q", out)
	}
}
