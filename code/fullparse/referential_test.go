package fullparse

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
)

// The classifier used to accept a referential without checking its
// phonotactics, so "lxa" read as a referential while the validator
// called it unpronounceable. Two components disagreeing about what a
// word is, with nothing forcing them to agree.
//
// §4.6.1 settles which of them was right: the epenthetic -ë- appears
// "if necessary due to phonotactic rules", so a referential is subject
// to the ordinary cluster constraints like any other word.
func TestReferential_ObeysPhonotactics(t *testing.T) {
	for _, w := range []string{
		"lxa", // NOMIC suffixed to 1m: "lx" cannot open a word
		"lça", // the other NOMIC form, same problem
		"ļla", // AGGLOMERATIVE prefixed as "ļ": "ļl" cannot either
	} {
		if err := phonology.CheckText(w); err == nil {
			t.Fatalf("%q is valid after all; this test rests on it not being", w)
		}
		if _, err := Referential(w); err == nil {
			t.Errorf("Referential(%q) succeeded on a word the validator rejects", w)
		}
	}
}

// The same category on the same referent, attached on the side the
// cluster rules do allow.
func TestReferential_CategoryOnThePermissibleSide(t *testing.T) {
	for _, c := range []struct {
		word string
		cat  g.RefCategory
		ref  g.Referent
	}{
		{"çla", g.Nomic, g.R1m},
		{"xla", g.Nomic, g.R1m},
		{"sxa", g.Nomic, g.R2m},
		{"tļla", g.Agglomerative, g.R1m},
		{"lwa", g.Abstract, g.R1m},
	} {
		r, err := Referential(c.word)
		if err != nil {
			t.Errorf("Referential(%q): %v", c.word, err)
			continue
		}
		head, ok := r.Head.(g.PersonalHead)
		if !ok {
			t.Errorf("%q: head is %T, want PersonalHead", c.word, r.Head)
			continue
		}
		if head.Category == nil || *head.Category != c.cat {
			t.Errorf("%q: category = %v, want %v", c.word, head.Category, c.cat)
		}
		if len(head.Refs) != 1 || head.Refs[0].Referent != c.ref {
			t.Errorf("%q: refs = %v, want [%v]", c.word, head.Refs, c.ref)
		}
	}
}

// §4.6.1 leaves V_C1 unparenthesized, so a referential always carries
// a case.
func TestReferential_RequiresACase(t *testing.T) {
	for _, w := range []string{"l", "sml", "tļl"} {
		if _, err := Referential(w); err == nil {
			t.Errorf("Referential(%q) succeeded without a case vowel", w)
		}
	}
}

// §4.6.2 slot 6 gives ultimate stress the RPV Essence reading. The
// diacritic used to survive into the affix vowel lookup, which put
// every stressed combination referential a degree off: the section's
// own example ëlsuoxxéd came back with the affix at degree 0.
func TestCombinationReferential_UltimateStressIsRPV(t *testing.T) {
	stressed, err := CombinationReferential("ëlsuoxxéd")
	if err != nil {
		t.Fatalf("ëlsuoxxéd: %v", err)
	}
	if !stressed.RpvEssence {
		t.Error("ëlsuoxxéd: RpvEssence = false, want true")
	}
	plain, err := CombinationReferential("ëlsuoxxed")
	if err != nil {
		t.Fatalf("ëlsuoxxed: %v", err)
	}
	if plain.RpvEssence {
		t.Error("ëlsuoxxed: RpvEssence = true, want false")
	}
	// Stress marks Essence and nothing else, so the two words agree on
	// every other field, the affix degree included.
	if len(stressed.Affixes) != 1 || len(plain.Affixes) != 1 {
		t.Fatalf("affixes: stressed %v, plain %v", stressed.Affixes, plain.Affixes)
	}
	if stressed.Affixes[0] != plain.Affixes[0] {
		t.Errorf("stress changed the affix: %+v vs %+v",
			stressed.Affixes[0], plain.Affixes[0])
	}
	if stressed.Case != plain.Case || stressed.Spec != plain.Spec {
		t.Errorf("stress changed case or spec: %v/%v vs %v/%v",
			stressed.Case, stressed.Spec, plain.Case, plain.Spec)
	}
}

// The four §4.6.2 examples the section prints.
func TestCombinationReferential_SourceExamples(t *testing.T) {
	for _, w := range []string{"slex", "poxtanz", "ëtkexpa", "ëlsuoxxéd"} {
		if _, err := CombinationReferential(w); err != nil {
			t.Errorf("CombinationReferential(%q): %v", w, err)
		}
	}
}

// The §4.6.1 examples, which the section prints as a run of words.
func TestReferential_SourceExamples(t *testing.T) {
	for _, w := range []string{
		"to", "zua", "laiwe", "ëpgói", "ëztewim",
		"smoyút", "triwejvë", "sme'e", "ka'u",
	} {
		if _, err := Referential(w); err != nil {
			t.Errorf("Referential(%q): %v", w, err)
		}
	}
}

// §4.6.1 says the epenthetic -ë- appears "before or within C_1
// combinations if necessary due to phonotactic rules", and gives
// "zëmse" as its own example: the referent chain is z+m+s, with the
// vowel sitting inside the cluster rather than in front of it. We read
// only a leading -ë-, so the word comes apart as C1="z", V_C1="ë",
// and then fails on the leftover "mse".
//
// The fix is not to strip every "ë" before decomposing: that vowel is
// a real Vc value elsewhere, and "ëztewim" three examples earlier is
// the leading case we do handle. What is needed is for the C1
// decomposition to be allowed to consume an interior "ë" as cluster
// padding, which means it has to run over the conjunct boundary that
// SplitConjuncts puts there.
func TestReferential_EpentheticVowelWithinC1(t *testing.T) {
	t.Skip("§4.6.1 epenthetic -ë- inside the C_1 cluster is not read; see the comment above")

	if _, err := Referential("zëmse"); err != nil {
		t.Errorf("Referential(zëmse): %v", err)
	}
}

// §4.6.1 notes that "Sec. 1.7, Rule 3, applies to Slot 2 V_C1 and
// Slot 3 V_C2 for Cases 37 through 52", and prints "fo'we'is" among
// its examples. Both vowels there carry a glottal stop marking the
// higher case group, and the word shows one §1.7 placement each: V_C1
// is not word-final, so Rule 1 leaves its glottal after the vowel-form
// and against the Slot 3 w, which absorbRule1Glottals puts back; V_C2
// is word-final and takes Rule 3's epenthetic "e'i", which
// MergeGlottalVowels rejoins.
func TestReferential_GlottalInBothCaseVowels(t *testing.T) {
	if _, err := Referential("fo'we'is"); err != nil {
		t.Errorf("Referential(fo'we'is): %v", err)
	}
}
