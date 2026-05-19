package main

import (
	"bytes"
	"path/filepath"
	"strings"
	"testing"
)

func dataDir() string {
	return filepath.Join("..", "..", "..", "data")
}

// helper: run the CLI with the given args and return (stdout, stderr,
// exit code).
func runCLI(args ...string) (string, string, int) {
	var stdout, stderr bytes.Buffer
	code := run(args, strings.NewReader(""), &stdout, &stderr)
	return stdout.String(), stderr.String(), code
}

func TestRun_ArgsAndLexicon(t *testing.T) {
	out, errOut, code := runCLI("-lex", dataDir(), "malëuţřait")
	if code != 0 {
		t.Fatalf("exit code %d, want 0; stderr: %s", code, errOut)
	}
	if !strings.Contains(out, "malëuţřait") {
		t.Errorf("output missing surface; got %q", out)
	}
	if !strings.Contains(out, "linguistic utterance") {
		t.Errorf("output missing lexicon meaning; got %q", out)
	}
}

func TestRun_Stdin(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run(nil, strings.NewReader("amlala\n"), &stdout, &stderr)
	if code != 0 {
		t.Fatalf("exit code %d, want 0; stderr: %s", code, stderr.String())
	}
	if !strings.Contains(stdout.String(), "amlala") {
		t.Errorf("stdout missing word: %q", stdout.String())
	}
}

func TestRun_ValidateFlag(t *testing.T) {
	out, errOut, code := runCLI("-validate", "akx")
	if code != 0 {
		t.Fatalf("exit code %d, want 0; stdout: %s; stderr: %s", code, out, errOut)
	}
	if !strings.Contains(errOut, "2.3") {
		t.Errorf("expected rule 2.3 in stderr; got %q", errOut)
	}
}

func TestRun_NoInput(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run(nil, strings.NewReader(""), &stdout, &stderr)
	if code != 2 {
		t.Errorf("expected exit 2 for empty input, got %d", code)
	}
	if !strings.Contains(stderr.String(), "usage") {
		t.Errorf("expected usage in stderr; got %q", stderr.String())
	}
}

func TestRun_BadLexDir_Warns(t *testing.T) {
	// Bad lex dir now warns and proceeds without the lexicon.
	out, errOut, code := runCLI("-lex", "/no/such/dir", "amlala")
	if code != 0 {
		t.Errorf("expected exit 0 (warn-and-continue), got %d", code)
	}
	if !strings.Contains(errOut, "lexicon load failed") {
		t.Errorf("expected warning on stderr; got %q", errOut)
	}
	if !strings.Contains(out, "amlala") {
		t.Errorf("expected gloss on stdout; got %q", out)
	}
}

// ---- subcommand tests ----

func TestCmd_Help(t *testing.T) {
	out, _, code := runCLI("--help")
	if code != 0 {
		t.Fatalf("--help exit %d", code)
	}
	for _, want := range []string{"--lookup", "--form", "--grammar", "--biases", "--trace", "--compose"} {
		if !strings.Contains(out, want) {
			t.Errorf("--help missing %q", want)
		}
	}
}

func TestCmd_Lookup(t *testing.T) {
	out, _, code := runCLI("--lookup", "THM")
	if code != 0 {
		t.Fatalf("--lookup exit %d", code)
	}
	if !strings.Contains(out, "THM") {
		t.Errorf("--lookup missing THM; got %q", out)
	}
}

func TestCmd_Form(t *testing.T) {
	out, _, code := runCLI("--form", "a")
	if code != 0 {
		t.Fatalf("--form exit %d", code)
	}
	if !strings.Contains(out, "THM") {
		t.Errorf("--form a should mention THM; got %q", out)
	}
}

func TestCmd_Grammar(t *testing.T) {
	out, _, code := runCLI("--grammar")
	if code != 0 {
		t.Fatalf("--grammar exit %d", code)
	}
	// Many lines; smoke check a few entries.
	for _, want := range []string{"THM", "MNO", "RTR", "FAC"} {
		if !strings.Contains(out, want) {
			t.Errorf("--grammar missing %q", want)
		}
	}
}

func TestCmd_Biases(t *testing.T) {
	out, _, code := runCLI("--biases", "please")
	if code != 0 {
		t.Fatalf("--biases exit %d", code)
	}
	if !strings.Contains(out, "SOL") {
		t.Errorf("--biases please missing SOL; got %q", out)
	}
}

func TestCmd_Biases_All(t *testing.T) {
	out, _, code := runCLI("--biases")
	if code != 0 {
		t.Fatalf("--biases exit %d", code)
	}
	// 61 biases + header = 62 lines.
	lines := strings.Split(strings.TrimSpace(out), "\n")
	if len(lines) < 60 {
		t.Errorf("--biases listed %d lines, want ~62", len(lines))
	}
}

func TestCmd_Root(t *testing.T) {
	out, _, code := runCLI("-lex", dataDir(), "--root", "yellow")
	if code != 0 {
		t.Fatalf("--root exit %d", code)
	}
	if !strings.Contains(out, "-ml-") {
		t.Errorf("--root yellow should find -ml-; got %q", out)
	}
}

func TestCmd_Affix(t *testing.T) {
	out, _, code := runCLI("-lex", dataDir(), "--affix", "negation")
	if code != 0 {
		t.Fatalf("--affix exit %d", code)
	}
	if !strings.Contains(out, "NEG") {
		t.Errorf("--affix negation should find NEG; got %q", out)
	}
}

func TestCmd_Trace(t *testing.T) {
	out, _, code := runCLI("--trace", "amlala", "la", "řřx")
	if code != 0 {
		t.Fatalf("--trace exit %d", code)
	}
	for _, want := range []string{"Slot II", "Slot III", "type", "Form", "Ref", "Bias"} {
		if !strings.Contains(out, want) {
			t.Errorf("--trace output missing %q; got %q", want, out)
		}
	}
}

func TestCmd_Search(t *testing.T) {
	out, _, code := runCLI("-lex", dataDir(), "--search", "yellow")
	if code != 0 {
		t.Fatalf("--search exit %d", code)
	}
	if !strings.Contains(out, "Roots:") || !strings.Contains(out, "-ml-") {
		t.Errorf("--search yellow missing root section; got %q", out)
	}
}

func TestCmd_Compose(t *testing.T) {
	out, _, code := runCLI("--compose", "ml", "S2", "CPT", "ERG")
	if code != 0 {
		t.Fatalf("--compose exit %d", code)
	}
	got := strings.TrimSpace(out)
	if got != "imlalo" {
		t.Errorf("--compose ml S2 CPT ERG = %q, want \"imlalo\"", got)
	}
}

func TestCmd_Compose_Unknown(t *testing.T) {
	_, errOut, code := runCLI("--compose", "ml", "XYZZY")
	if code == 0 {
		t.Error("--compose with unknown flag should fail")
	}
	if !strings.Contains(errOut, "unknown") {
		t.Errorf("expected error in stderr; got %q", errOut)
	}
}
