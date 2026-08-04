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
