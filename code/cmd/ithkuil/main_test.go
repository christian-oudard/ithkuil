package main

import (
	"bytes"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/store"
	"github.com/christian-oudard/ithkuil/view"
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
		"no Ca complex is written vẓ",
		"PHONETIC", "SLOT", "READS AS",
		"Cr", "Vr", "Ca",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("diagnostic missing %q; got %q", want, out)
		}
	}
	// The failing row is marked and the rest say they read, so the
	// reader can see how much of the word was understood. Without the
	// marker the table is a list of conjuncts with a sentence beside
	// it, and locating the bad slot means counting.
	for _, line := range strings.Split(out, "\n") {
		if strings.Contains(line, "-vẓ-") && !strings.Contains(line, "✗") {
			t.Errorf("the Ca row is not marked as the failure: %q", line)
		}
		if strings.Contains(line, " m-") && strings.Contains(line, "✗") {
			t.Errorf("the Cr row read fine and must not be marked: %q", line)
		}
	}
	// No grammatical values are known, so the column must be gone
	// rather than printed empty over the rows.
	if strings.Contains(out, "ENCODES") {
		t.Errorf("empty ENCODES column should be dropped; got %q", out)
	}
}

// TestParse_ShortShowsReason checks the one-line view carries the
// reason too. Its gloss for an unclassified word is the romanization
// spelled back with a "?", which repeats the type column and says
// nothing about why the word could not be read.
func TestParse_ShortShowsReason(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "parse", "--short", "mavẓorf")
	if code != 0 {
		t.Fatalf("parse --short exit %d; got %q", code, out)
	}
	if !strings.Contains(out, "no Ca complex is written vẓ") {
		t.Errorf("short view missing the reason; got %q", out)
	}
	if strings.Contains(out, "?mavẓorf") {
		t.Errorf("short view still echoing the romanization as a gloss; got %q", out)
	}
}

// TestParse_CapitalDoesNotSkipValidation pins the bypass shut. The
// invalid-word map was keyed by the raw token but read back with the
// lower-cased rom, so a capital silently skipped validation:
// "cskava" exited 1 while "Cskava" exited 0. Both must now report,
// and the message must name the word as typed.
func TestParse_CapitalDoesNotSkipValidation(t *testing.T) {
	for _, word := range []string{"cskava", "Cskava"} {
		t.Run(word, func(t *testing.T) {
			_, errOut, code := runCLI("-data", dataFile(), "parse", word)
			if code != 1 {
				t.Errorf("parse %q exit = %d, want 1; stderr=%q", word, code, errOut)
			}
			if !strings.Contains(errOut, "2.9") {
				t.Errorf("parse %q missing the rule; stderr=%q", word, errOut)
			}
		})
	}
}

// TestParse_ErrorNamesTypedWord checks the ASCII input method does not
// swallow the user's spelling. "aaaa" normalizes to "ää", and
// reporting a rule against "ää" alone describes a word never written.
func TestParse_ErrorNamesTypedWord(t *testing.T) {
	_, errOut, code := runCLI("-data", dataFile(), "parse", "aaaa")
	if code != 1 {
		t.Fatalf("parse aaaa exit = %d, want 1; stderr=%q", code, errOut)
	}
	if !strings.Contains(errOut, "aaaa → ää") {
		t.Errorf("error should show typed → normalized; got %q", errOut)
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

// --short prints the canonical gloss and nothing else: the one string
// compose reads back. It used to print the display rendering, which
// spells the affix Type as a Unicode subscript and a carrier as
// "CARR-Carrier(a)" — neither of which parses, so the view told you
// something you could not use.
func TestParse_Short(t *testing.T) {
	out, _, code := runCLI("parse", "--short", "malëuţřait")
	if code != 0 {
		t.Fatalf("parse --short exit %d", code)
	}
	if !strings.Contains(out, "SYS/5_2") {
		t.Errorf("--short should print the canonical gloss; got %q", out)
	}
	if strings.Contains(out, "₂") {
		t.Errorf("--short printed the display rendering, not the syntax; got %q", out)
	}
	// The word class is legible from the gloss's own shape, so no
	// column repeats it.
	if strings.Contains(out, "Form") {
		t.Errorf("--short should not carry a type column; got %q", out)
	}
}

func TestParse_ShortFlag(t *testing.T) {
	// -s should behave the same as --short.
	out, _, code := runCLI("parse", "-s", "malëuţřait")
	if code != 0 {
		t.Fatalf("parse -s exit %d", code)
	}
	if !strings.Contains(out, "SYS/5_2") {
		t.Errorf("-s should print the canonical gloss; got %q", out)
	}
}

// What --short prints has to compose back to the word it came from.
// That is the whole claim the view makes, and nothing checked it while
// the view was printing a rendering no parser accepted.
// Each word is given in its canonical spelling, so the trip out and
// back is an identity rather than a canonicalization.
//
// "hamlal-mlala" is here because a §3.1 chain is the one word whose
// gloss is not a single token: its members are separated by a space.
// compose read its argument with gloss.ParseWord, which takes one
// token, so the chain gloss this very test would produce was the one
// string compose could not read back. The list had no chain in it.
func TestParse_ShortRoundTripsThroughCompose(t *testing.T) {
	for _, word := range []string{
		"malëuţřait", "hla", "la", "mlala", "lo", "hamlal-mlala",
	} {
		out, _, code := runCLI("-data", dataFile(), "parse", "--short", word)
		if code != 0 {
			t.Errorf("parse --short %q exit %d: %s", word, code, out)
			continue
		}
		gl := strings.TrimSpace(out)
		back, _, code := runCLI("-data", dataFile(), "compose", gl)
		if code != 0 {
			t.Errorf("%q glossed to %q, which compose rejects: %s", word, gl, back)
			continue
		}
		if got := strings.TrimSpace(strings.SplitN(back, "\n", 2)[0]); got != word {
			t.Errorf("%q glossed to %q, which composes to %q", word, gl, got)
		}
	}
}

func TestParse_Stdin(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run([]string{"parse", "--short"}, strings.NewReader("amlala\n"), &stdout, &stderr)
	if code != 0 {
		t.Fatalf("parse (stdin) exit %d; stderr=%s", code, stderr.String())
	}
	// The gloss, not the romanization: --short prints one and not the
	// other. "amlala" is the root "ml" at every default.
	if strings.TrimSpace(stdout.String()) != "ml" {
		t.Errorf("stdin path gloss = %q, want %q", stdout.String(), "ml")
	}
}

// ---- compare ----

func TestCompare_OneSlotApart(t *testing.T) {
	// marçat and marcat differ only in Ca configuration. Every other
	// slot must come out unmarked, and the differences table must
	// name just the one category that moved.
	out, _, code := runCLI("-data", dataFile(), "compare", "marc,at", "marcat")
	if code != 0 {
		t.Fatalf("compare exit %d", code)
	}
	for _, want := range []string{
		"marçat", "marcat", "DIFFERENCES",
		"configuration", "MDF", "DSS",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("compare output missing %q; got %q", want, out)
		}
	}
	if n := strings.Count(out, "≠"); n != 1 {
		t.Errorf("expected exactly one marked row, got %d; output %q", n, out)
	}
	if strings.Contains(out, "affiliation") {
		t.Errorf("unchanged categories should not be listed; got %q", out)
	}
	for _, l := range strings.Split(out, "\n") {
		if l != strings.TrimRight(l, " ") {
			t.Errorf("trailing whitespace on line %q", l)
		}
	}
}

func TestCompare_Identical(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "compare", "marcat", "marcat")
	if code != 0 {
		t.Fatalf("compare exit %d", code)
	}
	if !strings.Contains(out, "identical") {
		t.Errorf("expected 'identical'; got %q", out)
	}
	if strings.Contains(out, "≠") {
		t.Errorf("identical words should have no marked rows; got %q", out)
	}
}

func TestCompare_DifferentRoots(t *testing.T) {
	// Different Cr means different lexical entries, which belong in a
	// ROOT block rather than the code-by-code differences table.
	out, _, code := runCLI("-data", dataFile(), "compare", "marcat", "narcat")
	if code != 0 {
		t.Fatalf("compare exit %d", code)
	}
	for _, want := range []string{"ROOT", `"m" / S1 / BSC`, `"n" / S1 / BSC`} {
		if !strings.Contains(out, want) {
			t.Errorf("compare output missing %q; got %q", want, out)
		}
	}
}

func TestCompare_UnevenAffixes(t *testing.T) {
	// malëuţřait carries two affixes to marçat's one, so the extra
	// Vx₂/Cs₂ pair must get its own rows with the right side blank.
	out, _, code := runCLI("-data", dataFile(), "compare", "malëuţřait", "marçat")
	if code != 0 {
		t.Fatalf("compare exit %d", code)
	}
	for _, want := range []string{"Vx₂", "Cs₂", "SYS"} {
		if !strings.Contains(out, want) {
			t.Errorf("compare output missing %q; got %q", want, out)
		}
	}
}

func TestCompare_AgainstUnclassified(t *testing.T) {
	// mavẓorf is mavẓorff with the §3.6.1 Ca gemination removed, which
	// re-splits the word: "vẓ" stops being a Slot V affix and is read
	// as the Ca, where it isn't a legal value. Comparing the two is how
	// you see that re-split, so a word that fails to decode must still
	// line up by shape against one that doesn't.
	out, _, code := runCLI("-data", dataFile(), "compare", "mavẓorff", "mavẓorf")
	if code != 0 {
		t.Fatalf("compare exit %d; got %q", code, out)
	}
	for _, want := range []string{
		"Cs₅₁", "Ca", "Vx₁", "Cs₁",
		"UNCLASSIFIED", "no Ca complex is written vẓ",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("compare output missing %q; got %q", want, out)
		}
	}
	// Cr and Vr split the same way in both words. Shape is all the two
	// have in common, so only shape may decide what gets marked, and
	// the decoded side's codes must not mark those rows.
	for _, line := range strings.Split(out, "\n") {
		if strings.HasPrefix(line, "≠") && (strings.Contains(line, " Cr ") || strings.Contains(line, " Vr ")) {
			t.Errorf("shared shape marked as differing: %q", line)
		}
	}
	// An undecoded word has no glossary, so there is no code-by-code
	// table to print; every category would read as a difference.
	if strings.Contains(out, "DIFFERENCES") {
		t.Errorf("glossary diff should be skipped for an undecoded word; got %q", out)
	}
}

func TestCompare_UnclassifiedElisions(t *testing.T) {
	// A shape split has no placeholder rows for elided slots, so the
	// decoded side's ∅ Vv and Vc must not show up as differences.
	out, _, code := runCLI("-data", dataFile(), "compare", "mavẓorff", "mavẓorf")
	if code != 0 {
		t.Fatalf("compare exit %d", code)
	}
	if strings.Contains(out, "∅") {
		t.Errorf("elided slots should be dropped when diffing by shape; got %q", out)
	}
}

func TestCompare_UnsplittableWord(t *testing.T) {
	// Without even a shape split there is nothing to lay side by side.
	_, errOut, code := runCLI("-data", dataFile(), "compare", "hlç", "marcat")
	if code != 1 {
		t.Errorf("compare exit %d, want 1", code)
	}
	if !strings.Contains(errOut, "at least a Cr, a Vr and a Ca") {
		t.Errorf("expected the split failure in stderr; got %q", errOut)
	}
}

func TestCompare_ChainAgainstSingleWord(t *testing.T) {
	// A chain's parent comes last (§3.1.7), so it, not the leading
	// dependent, is what a standalone word is the counterpart of.
	// Pairing runs from the parent end and the spare dependent is
	// reported rather than silently dropped.
	out, _, code := runCLI("-data", dataFile(), "compare", "hakšal-uḑfarf", "marcat")
	if code != 0 {
		t.Fatalf("compare exit %d; got %q", code, out)
	}
	for _, want := range []string{
		"uḑfarf [head]", "marcat",
		"UNPAIRED", "hakšal", "Type1 dependent of hakšal-uḑfarf",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("compare output missing %q; got %q", want, out)
		}
	}
	// Only the parent is compared, so the dependent's own slots must
	// not appear in a table.
	if strings.Contains(out, "Type1 concat") {
		t.Errorf("unpaired dependent should not get a slot table; got %q", out)
	}
	if n := strings.Count(out, "SLOT"); n != 1 {
		t.Errorf("want one slot table, got %d; output %q", n, out)
	}
}

func TestCompare_ChainAgainstChain(t *testing.T) {
	// Equal-length chains pair member for member, each with its own
	// table. Only the dependent differs here, so the parent's table
	// must come out clean.
	out, _, code := runCLI("-data", dataFile(), "compare", "hakšal-uḑfarf", "hakšol-uḑfarf")
	if code != 0 {
		t.Fatalf("compare exit %d; got %q", code, out)
	}
	if n := strings.Count(out, "SLOT"); n != 2 {
		t.Errorf("want a table per member, got %d; output %q", n, out)
	}
	for _, want := range []string{
		"hakšal [Type1 dependent]", "hakšol [Type1 dependent]",
		"uḑfarf [head]", "function", "STA", "DYN", "identical",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("compare output missing %q; got %q", want, out)
		}
	}
	if strings.Contains(out, "UNPAIRED") {
		t.Errorf("equal-length chains leave nothing unpaired; got %q", out)
	}
}

func TestCompare_WrongArgCount(t *testing.T) {
	for _, args := range [][]string{{"compare", "marcat"}, {"compare"}, {"compare", "a", "b", "c"}} {
		_, errOut, code := runCLI(args...)
		if code != 2 {
			t.Errorf("%v: exit %d, want 2", args, code)
		}
		if !strings.Contains(errOut, "usage") {
			t.Errorf("%v: expected usage in stderr; got %q", args, errOut)
		}
	}
}

func TestCompare_InvalidWord(t *testing.T) {
	_, errOut, code := runCLI("-data", dataFile(), "compare", "tttest", "marcat")
	if code != 1 {
		t.Errorf("compare exit %d, want 1", code)
	}
	if !strings.Contains(errOut, "tttest") {
		t.Errorf("expected the bad word in stderr; got %q", errOut)
	}
}

func TestCompare_NoSlotBreakdown(t *testing.T) {
	_, errOut, code := runCLI("-data", dataFile(), "compare", "ho", "ha")
	if code != 1 {
		t.Errorf("compare exit %d, want 1", code)
	}
	if !strings.Contains(errOut, "no slot breakdown") {
		t.Errorf("expected refusal in stderr; got %q", errOut)
	}
}

// ---- compose ----

func TestCompose_Expression(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "compose", "S2.CPT-ml-ERG")
	if code != 0 {
		t.Fatalf("compose exit %d", code)
	}
	lines := strings.Split(strings.TrimSpace(out), "\n")
	if len(lines) != 2 {
		t.Fatalf("compose output = %q, want 2 lines (romanization + gloss)", out)
	}
	if lines[0] != "wimlo" {
		t.Errorf("compose rom = %q, want canonical \"wimlo\" (Cc shortcut form)", lines[0])
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

// TestCompose_WordClasses drives the classes that are not formatives.
// compose called gloss.ParseFormative directly, so every one of these came
// back as "no root in ..." — the command documented a syntax it could
// not run.
func TestCompose_WordClasses(t *testing.T) {
	for _, tc := range []struct{ expr, want string }{
		{"1m-ERG", "lo"},           // referential
		{"DOL", "řřx"},             // bias adjunct
		{"[QUO]-ERG", "hmo"},       // carrier adjunct
		{"\"Emily\"", "Emily"},     // foreign word
		{"S2.CPT-ml-ERG", "wimlo"}, // still a formative
	} {
		out, errOut, code := runCLI("-data", dataFile(), "compose", tc.expr)
		if code != 0 {
			t.Errorf("compose %q: exit %d, stderr %q", tc.expr, code, errOut)
			continue
		}
		lines := strings.Split(strings.TrimSpace(out), "\n")
		if lines[0] != tc.want {
			t.Errorf("compose %q = %q, want %q", tc.expr, lines[0], tc.want)
		}
	}
}

// TestCompose_Adjuncts covers the two classes that had no renderer at
// all, so composing one failed on the way out rather than on the way
// in. The parsing adjunct is not among them: it declares stress rather
// than meaning, so it is not a word compose builds.
func TestCompose_Adjuncts(t *testing.T) {
	for _, tc := range []struct{ expr, want string }{
		{"RTR.SUB", "ahw"}, // §4.3 modular
		{"DEV/3", "eb"},    // §4.1.1 single-affix
	} {
		out, errOut, code := runCLI("-data", dataFile(), "compose", tc.expr)
		if code != 0 {
			t.Errorf("compose %q: exit %d, stderr %q", tc.expr, code, errOut)
			continue
		}
		lines := strings.Split(strings.TrimSpace(out), "\n")
		if lines[0] != tc.want {
			t.Errorf("compose %q = %q, want %q", tc.expr, lines[0], tc.want)
		}
	}
}

// TestCompose_Unmarked covers the one word that composes to nothing:
// NRR is the default register, so there is no adjunct to write.
func TestCompose_Unmarked(t *testing.T) {
	out, errOut, code := runCLI("-data", dataFile(), "compose", "NRR")
	if code == 0 {
		t.Errorf("expected non-zero exit, got output %q", out)
	}
	if !strings.Contains(errOut, "unmarked") {
		t.Errorf("stderr = %q, want it to say the register is unmarked", errOut)
	}
}

func TestCompose_BadValue(t *testing.T) {
	_, errOut, code := runCLI("compose", "ml-XYZZY")
	if code == 0 {
		t.Error("expected non-zero exit on unknown abbreviation")
	}
	// The message has to name the token and the inventory it was
	// looked for in. Which inventory is the part a reader acts on:
	// XYZZY not being a grammatical value says to go and look at the
	// value tables, where "unknown" alone said only that something
	// went wrong somewhere in the line.
	for _, want := range []string{"XYZZY", "grammatical value"} {
		if !strings.Contains(errOut, want) {
			t.Errorf("stderr missing %q; got %q", want, errOut)
		}
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
	// A romanization is a grammar question; the lexicon has no
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

// The README documents these, and a reader copies them into the shell
// rather than into a Go call. TestDocumentedSyntaxExamples checks the
// library function; this checks the command, which is not the same
// path and did not always agree with it. Routing compose through
// gloss.ParseWord broke "m-SYS/5_2-{Ca}-DCD/1_2": the "{Ca}" there marks
// the Slot V/VII boundary, and the affixual-adjunct recognizer took
// any brace as proof of an affixual adjunct.
func TestCompose_DocumentedExamples(t *testing.T) {
	for _, c := range []struct{ expr, want string }{
		{"ml", "mlala"},
		{"S2.CPT-ml-ERG", "wimlo"},
		{"m-SYS/5_2-{Ca}-DCD/1_2", "maţřëullait"},
		{"ml-Ca:PRX-ERG", "mlalüödo"},
		{"ml-ACC/INS-ERG", "mlaläswo"},
		{"ml-(1m)/AFF-ERG", "mlaleölo"},
		{"1m-ERG", "lo"},
		{"[CAR]", "hla"},
		{"DSV_END", "hai"},
		// A C_S root in parentheses, which the affixual-adjunct
		// discriminator used to claim: "(CTR)" is a root, not an affix.
		{"(CTR)/1", "ëilal"},
	} {
		out, errOut, code := runCLI("-data", dataFile(), "compose", c.expr)
		if code != 0 {
			t.Errorf("compose %q: exit %d: %s", c.expr, code, errOut)
			continue
		}
		if got := strings.SplitN(strings.TrimSpace(out), "\n", 2)[0]; got != c.want {
			t.Errorf("compose %q = %q, want %q", c.expr, got, c.want)
		}
	}
}

// Both spellings of a word have to read back to the same grammar, so
// --stressless is a different channel rather than a different word.
func TestCompose_StresslessMatchesNormal(t *testing.T) {
	for _, expr := range []string{"ml", "S2.CPT-ml-ERG", "1m-ERG", "m-SYS/5_2-{Ca}-DCD/1_2"} {
		plain, _, code := runCLI("-data", dataFile(), "compose", expr)
		if code != 0 {
			t.Errorf("compose %q failed", expr)
			continue
		}
		sung, errOut, code := runCLI("-data", dataFile(), "compose", "--stressless", expr)
		if code != 0 {
			t.Errorf("compose --stressless %q: exit %d: %s", expr, code, errOut)
			continue
		}
		normalWord := strings.SplitN(strings.TrimSpace(plain), "\n", 2)[0]
		sungWords := strings.SplitN(strings.TrimSpace(sung), "\n", 2)[0]
		if normalWord == sungWords {
			t.Errorf("%q: --stressless produced the same spelling %q", expr, sungWords)
		}
		// Reading either one has to give the same gloss.
		a, _, _ := runCLI("-data", dataFile(), "parse", "--short", normalWord)
		b, _, _ := runCLI("-data", dataFile(), "parse", "--short", sungWords)
		if strings.TrimSpace(a) != strings.TrimSpace(b) {
			t.Errorf("%q: %q glosses as %q but %q glosses as %q",
				expr, normalWord, strings.TrimSpace(a), sungWords, strings.TrimSpace(b))
		}
	}
}

// What compose prints has to be what compose accepts, and what parse
// prints for the same word.
//
// compose used to echo the display rendering: given "S2.CPT-ml-ERG" it
// answered "S2.CPT--ml-'gold (color)'-ERG", which neither command
// takes and which disagreed with parse. The same divergence was fixed
// in parse earlier and left here, and in the MCP server's gloss field.
func TestCompose_EchoesTheGlossItAccepts(t *testing.T) {
	for _, expr := range []string{
		"ml", "S2.CPT-ml-ERG", "1m-ERG", "[CAR]",
		"m-SYS/5_2-{Ca}-DCD/1_2",
	} {
		out, errOut, code := runCLI("-data", dataFile(), "compose", expr)
		if code != 0 {
			t.Errorf("compose %q: exit %d: %s", expr, code, errOut)
			continue
		}
		lines := strings.SplitN(strings.TrimSpace(out), "\n", 2)
		if len(lines) != 2 {
			t.Errorf("compose %q printed %d lines, want the word and its gloss", expr, len(lines))
			continue
		}
		word, echoed := lines[0], strings.TrimSpace(lines[1])

		// The echo composes to the same word.
		back, errOut, code := runCLI("-data", dataFile(), "compose", echoed)
		if code != 0 {
			t.Errorf("compose %q echoed %q, which compose rejects: %s", expr, echoed, errOut)
			continue
		}
		if got := strings.SplitN(strings.TrimSpace(back), "\n", 2)[0]; got != word {
			t.Errorf("%q echoed %q, which composes to %q not %q", expr, echoed, got, word)
		}

		// And parse says the same thing about that word.
		p, _, code := runCLI("-data", dataFile(), "parse", "--short", word)
		if code != 0 {
			continue
		}
		if got := strings.TrimSpace(p); got != echoed {
			t.Errorf("compose %q echoed %q but parse says %q", expr, echoed, got)
		}
	}
}

// TestParse_ModularAdjunct covers the one word class with a slot
// breakdown that no CLI test reached. renderModular is live in the
// binary and was never run by a test, so the phonetic table it prints
// had nothing pinning it: the elided-slot marking below is the visible
// half of a fix, and by hand was the only way it had been checked.
//
// §4.3's bare Slot 4 form writes the V_N alone, so "a" is a whole RTR
// adjunct with no C_N in it, and the column must say so rather than
// print a -w- the word does not contain.
func TestParse_ModularAdjunct(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "parse", "a")
	if code != 0 {
		t.Fatalf("exit %d\n%s", code, out)
	}
	for _, want := range []string{"RTR", "Vn\u2081", "Cn\u2081", "FAC", view.ElidedMark} {
		if !strings.Contains(out, want) {
			t.Errorf("output missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "-w") {
		t.Errorf("the C_N is elided, so no -w should be printed:\n%s", out)
	}
}

// TestParse_ModularMultiSlot covers the multi-entry shape, whose slots
// are numbered and whose §4.3 Slot 3 separator is still unaccounted for
// (see BUGS.md). What is pinned here is the categories it names.
func TestParse_ModularMultiSlot(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "parse", "w\u00e4h\u0148ainui")
	if code != 0 {
		t.Fatalf("exit %d\n%s", code, out)
	}
	for _, want := range []string{"Vn\u2081", "Vn\u2082", "Vn\u2083", "PRL", "HYP", "RSM", "IRP"} {
		if !strings.Contains(out, want) {
			t.Errorf("output missing %q:\n%s", want, out)
		}
	}
}

// TestSearch_FormWithCategory covers filterByCategory, which is live in
// the binary and was reached by no test. It only runs for a written-form
// lookup narrowed by category, the one combination the search tests did
// not pair.
func TestSearch_FormWithCategory(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "search", "a", "--form", "--category", "Case")
	if code != 0 {
		t.Fatalf("exit %d\n%s", code, out)
	}
	if !strings.Contains(out, "THM") {
		t.Errorf("a is the THM case vowel:\n%s", out)
	}
	// The same form under a category it does not belong to keeps nothing.
	out, _, code = runCLI("-data", dataFile(), "search", "a", "--form", "--category", "Bias")
	if code != 0 {
		t.Fatalf("exit %d\n%s", code, out)
	}
	if strings.Contains(out, "THM") {
		t.Errorf("a is not a bias form:\n%s", out)
	}
}
