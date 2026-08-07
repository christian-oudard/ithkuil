package main

import (
	"strings"
	"testing"
)

// TestParse_ExpandsCodesForEveryClass is the bug. `parse` without
// --short is the view that explains a word, and for a formative it
// does: a CATEGORY/CODE/NAME/MEANING row per code, so ERG is named
// Ergative and described. For the other classes it prints the gloss,
// the word class, and nothing else, so
//
//	$ ithkuil parse arqla
//	ařla
//	  VMC/1
//
//	  Affix
//
// leaves the reader to look VMC up by hand, which is the one thing the
// detailed view is for. --short is meant to be the terse one.
//
// The cause is the default arm of renderDetailed in parse.go, which
// prints view.Type and stops. Formatives and modular adjuncts have the
// arms above it and are the only classes with a glossary function:
// view.Glossary takes a Formative, view.GlossaryModular takes modular
// segments, and nothing covers an affixual adjunct, a carrier, a
// referential, a bias or a register marker.
//
// Writing a glossary function per class is the obvious fix and it is
// the wrong shape. The codes in these glosses come from three different
// places, an affix abbreviation from the lexicon, a scope or a carrier
// type from the grammar table, a referent from the grammar package, and
// per-class code would rediscover that split five times.
//
// The tractable fix is newer than the bug: gloss.Tokens splits a gloss
// line into its pieces and marks which are codes, and a code resolves
// through search.LookupGrammar or the affix lexicon. That is one
// expansion for every class, including classes nobody has written yet,
// and it is the same lookup the browser makes when a reader clicks a
// code.
// The wanted strings come from the sources, not from the output:
// affixes_reference.md §VMC is "Volumetric Measurement C", the affix
// for minims and fluid drams. This test first read it as "Vocal",
// which is a guess at what three letters stand for and matches
// nothing, and would have failed the fix it was written to describe.
func TestParse_ExpandsCodesForEveryClass(t *testing.T) {
	for _, tc := range []struct {
		word, code, want string
	}{
		{"arqla", "VMC", "Volumetric"}, // single-affix adjunct
		{"ex", "SIZ", "Size"},          // single-affix adjunct
		{"hla", "CAR", "Carrier"},      // carrier adjunct
		{"ha", "DSV", "Discursive"},    // register marker
		{"la", "THM", "Thematic"},      // referential
	} {
		out, _, _ := runCLI("parse", "--data", dataFile(), tc.word)
		if !strings.Contains(out, tc.code) {
			t.Fatalf("parse %s does not even print %s:\n%s", tc.word, tc.code, out)
		}
		if !strings.Contains(out, tc.want) {
			t.Errorf("parse %s prints %s but never says what it means; want %q in:\n%s",
				tc.word, tc.code, tc.want, out)
		}
	}
}

// ACC is two things at once: the Accidental bias of §4.1 and the
// case-accessor family of §3.9.2. Nothing in the three letters
// separates them, and they cannot share a key in the name tables, so
// the reading is decided by shape the way the gloss itself decides it —
// a bias is a whole word, an accessor binds a case after it.
//
// Looked up flat, "mlaläswa" (ml-ACC/INS) told the reader its accessor
// meant "as luck would have it", which is the wrong sense of the only
// abbreviation in this language that has two.
func TestParse_AccessorIsNotTheAccidentalBias(t *testing.T) {
	out, _, code := runCLI("-data", dataFile(), "parse", "mlaläswa")
	if code != 0 {
		t.Fatalf("parse exit %d\n%s", code, out)
	}
	if !strings.Contains(out, "Case-Accessor") {
		t.Errorf("ACC before a case is the accessor family:\n%s", out)
	}
	if strings.Contains(out, "Accidental") {
		t.Errorf("ACC before a case is not the bias:\n%s", out)
	}
	// The case the accessor names has to be explained too; it was not
	// in the segments, because the Cs chunk carries a raw cluster.
	if !strings.Contains(out, "Instrumental") {
		t.Errorf("the case the accessor names is unexplained:\n%s", out)
	}

	// A bias adjunct is still a bias.
	out, _, code = runCLI("-data", dataFile(), "parse", "lf")
	if code != 0 {
		t.Fatalf("parse lf exit %d\n%s", code, out)
	}
	if !strings.Contains(out, "Accidental") {
		t.Errorf("a lone ACC is the bias:\n%s", out)
	}
}
