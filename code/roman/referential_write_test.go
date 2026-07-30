package roman

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// Every referential the grammar can express must render to a word, and
// that word must read back as the same value. Until now referentials
// had no renderer at all, so this loop had no way to close and a
// defect in the parse arm had nothing to contradict it.
func TestReferential_RoundTrip(t *testing.T) {
	cases := []g.Referential{
		{Head: personal(g.R1m), Case: g.THM},
		{Head: personal(g.R2m), Case: g.ERG},
		{Head: personal(g.Rpvs), Case: g.DAT},
		{Head: g.PersonalHead{Refs: []g.PersonalRef{
			{Referent: g.R2m}, {Referent: g.Rma}, {Referent: g.R1m},
		}}, Case: g.ERG},
		{Head: personal(g.R1m), Case: g.ERG,
			Second: &g.SecondReferent{Case: g.DAT}, RpvEssence: true},
		{Head: personal(g.R1m), Case: g.THM,
			Second: &g.SecondReferent{Case: g.ERG}},
		{Head: personal(g.R1m), Case: g.THM,
			Second: &g.SecondReferent{Case: g.IND, Refs: []g.PersonalRef{{Referent: g.R2m}}}},
		{Head: g.SuppletiveHead{Type: g.Quotative}, Case: g.ERG,
			Second: &g.SecondReferent{Case: g.DAT}},
	}
	for _, cat := range g.AllRefCategories {
		c := cat
		cases = append(cases, g.Referential{
			Head: g.PersonalHead{
				Refs:     []g.PersonalRef{{Referent: g.R1m}},
				Category: &c,
			},
			Case: g.ERG,
		})
	}
	for _, want := range cases {
		word, err := Referential(want)
		if err != nil {
			t.Errorf("Referential(%+v): %v", want, err)
			continue
		}
		got, err := ParseReferential(word)
		if err != nil {
			t.Errorf("%+v rendered %q, which does not parse: %v", want, word, err)
			continue
		}
		if !sameReferential(got, want) {
			t.Errorf("%q round-tripped\n  want: %s\n  got:  %s",
				word, showRef(want), showRef(got))
		}
	}
}

func TestCombinationReferential_RoundTrip(t *testing.T) {
	dat := g.DAT
	thm := g.THM
	for _, want := range []g.CombinationReferential{
		{Head: personal(g.R1m), Case: g.THM, Spec: g.BSC},
		{Head: personal(g.R2m), Case: g.ERG, Spec: g.CTE},
		{Head: personal(g.Rma), Case: g.ABS, Spec: g.CSV},
		{Head: personal(g.R1m), Case: g.CMM, Spec: g.OBJ},
		{Head: personal(g.R1m), Case: g.ERG, Spec: g.BSC,
			Affixes: []g.Affix{{Type: g.Type1Affix, Degree: 3, Consonant: "r"}}},
		{Head: personal(g.R1m), Case: g.ERG, Spec: g.CTE,
			Affixes: []g.Affix{{Type: g.Type1Affix, Degree: 3, Consonant: "r"}},
			Case2:   &dat},
		// §4.6.2 spells a stacked THM as "-üa", "a" alone being the
		// epenthetic vowel that means no second case at all.
		{Head: personal(g.R1m), Case: g.ERG, Spec: g.BSC,
			Affixes: []g.Affix{{Type: g.Type1Affix, Degree: 3, Consonant: "r"}},
			Case2:   &thm},
		{Head: personal(g.R1m), Case: g.ERG, Spec: g.BSC, RpvEssence: true,
			Affixes: []g.Affix{{Type: g.Type1Affix, Degree: 3, Consonant: "r"}}},
	} {
		word, err := CombinationReferential(want)
		if err != nil {
			t.Errorf("CombinationReferential(%+v): %v", want, err)
			continue
		}
		got, err := ParseCombinationReferential(word)
		if err != nil {
			t.Errorf("%+v rendered %q, which does not parse: %v", want, word, err)
			continue
		}
		if !sameCombination(got, want) {
			t.Errorf("%q round-tripped wrong\n  want: %+v\n  got:  %+v", word, want, got)
		}
	}
}

// The §4.6 category affix goes on whichever side the cluster rules
// allow. These are the four the section's own forms produce on 1m and
// 2m; only one spelling of each is a sayable word.
func TestReferential_CategoryPlacement(t *testing.T) {
	nomic, agm, abs := g.Nomic, g.Agglomerative, g.Abstract
	for _, c := range []struct {
		ref  g.Referent
		cat  g.RefCategory
		want string
	}{
		// "lça" and "lxa" cannot open a word, so NOMIC prefixes here.
		{g.R1m, nomic, "çla"},
		// On 2m it is the other way round: "çsa" and "sça" both break
		// cluster rules, so the affix suffixes.
		{g.R2m, nomic, "sxa"},
		// "ļl" cannot open a word either, so the "tļ" form is taken.
		{g.R1m, agm, "tļla"},
		// §4.6 writes ABSTRACT with a leading hyphen only.
		{g.R1m, abs, "lwa"},
	} {
		cat := c.cat
		got, err := Referential(g.Referential{
			Head: g.PersonalHead{
				Refs:     []g.PersonalRef{{Referent: c.ref}},
				Category: &cat,
			},
			Case: g.THM,
		})
		if err != nil {
			t.Errorf("%v on %v: %v", c.cat, c.ref, err)
			continue
		}
		if got != c.want {
			t.Errorf("%v on %v = %q, want %q", c.cat, c.ref, got, c.want)
		}
	}
}

func personal(r g.Referent) g.PersonalHead {
	return g.PersonalHead{Refs: []g.PersonalRef{{Referent: r}}}
}

func sameRefs(a, b []g.PersonalRef) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func sameHead(a, b g.RefHead) bool {
	switch x := a.(type) {
	case g.SuppletiveHead:
		y, ok := b.(g.SuppletiveHead)
		return ok && x.Type == y.Type
	case g.PersonalHead:
		y, ok := b.(g.PersonalHead)
		if !ok || !sameRefs(x.Refs, y.Refs) {
			return false
		}
		if (x.Category == nil) != (y.Category == nil) {
			return false
		}
		return x.Category == nil || *x.Category == *y.Category
	}
	return false
}

func sameReferential(a, b g.Referential) bool {
	if !sameHead(a.Head, b.Head) || a.Case != b.Case || a.RpvEssence != b.RpvEssence {
		return false
	}
	if (a.Second == nil) != (b.Second == nil) {
		return false
	}
	if a.Second == nil {
		return true
	}
	return a.Second.Case == b.Second.Case && sameRefs(a.Second.Refs, b.Second.Refs)
}

func sameCombination(a, b g.CombinationReferential) bool {
	if !sameHead(a.Head, b.Head) || a.Case != b.Case || a.Spec != b.Spec ||
		a.RpvEssence != b.RpvEssence || len(a.Affixes) != len(b.Affixes) {
		return false
	}
	for i := range a.Affixes {
		if a.Affixes[i] != b.Affixes[i] {
			return false
		}
	}
	if (a.Case2 == nil) != (b.Case2 == nil) {
		return false
	}
	return a.Case2 == nil || *a.Case2 == *b.Case2
}

func showRef(r g.Referential) string {
	s := ""
	switch h := r.Head.(type) {
	case g.SuppletiveHead:
		s = h.Type.String()
	case g.PersonalHead:
		for _, x := range h.Refs {
			s += x.Referent.String() + "/" + x.Effect.String() + " "
		}
		if h.Category != nil {
			s = h.Category.String() + ":" + s
		}
	}
	s += "case=" + r.Case.String()
	if r.Second != nil {
		s += " second=" + r.Second.Case.String()
	}
	if r.RpvEssence {
		s += " RPV"
	}
	return s
}

// §4.6.1 groups "monosyllabic or penultimate" together as the default
// and gives ultimate stress the RPV Essence reading, so a one-syllable
// referential has no way to express RPV: its only syllable is already
// the unmarked case. Rendering one has to fail rather than quietly
// produce a word that means something else.
func TestReferential_MonosyllabicCannotCarryRPV(t *testing.T) {
	_, err := Referential(g.Referential{
		Head: personal(g.R1m), Case: g.THM, RpvEssence: true,
	})
	if err == nil {
		t.Error("rendering a monosyllabic RPV referential succeeded, want an error")
	}
	// The same value with a second referent has somewhere to put the
	// mark, so it renders.
	if _, err := Referential(g.Referential{
		Head: personal(g.R1m), Case: g.THM, RpvEssence: true,
		Second: &g.SecondReferent{Case: g.ERG},
	}); err != nil {
		t.Errorf("disyllabic RPV referential: %v", err)
	}
}

// §4.6.1's own example "ëztewim" needs the epenthetic -ë-: its head
// cluster "zt" cannot open a word without it. The Slot 3 separator and
// the prefix are chosen together for that reason, since which separator
// is sayable depends on the cluster the prefix leaves in front of it.
func TestReferential_EpentheticPrefixWithSecondReferent(t *testing.T) {
	r, err := ParseReferential("ëztewim")
	if err != nil {
		t.Fatalf("Referential(ëztewim): %v", err)
	}
	got, err := Referential(r)
	if err != nil {
		t.Fatalf("Referential(%+v): %v", r, err)
	}
	if got != "ëztewim" {
		t.Errorf("ëztewim wrote back as %q", got)
	}
}

// §1.7 gives a case vowel's glottal stop two placements, and §4.6.1
// prints examples of both. Rule 1 puts it after the whole vowel-form
// and is the default; Rule 3 reduplicates or goes intervocalic, and
// overrides Rule 1 only where Rule 1 cannot serve, word-finally above
// all. So a V_C1 with a Slot 3 behind it takes Rule 1 and a V_C1 at
// the end of the word takes Rule 3, and both spell one case.
//
// The lookup tables are keyed on the Rule 3 spelling, because a
// formative's word-final V_C always lands there, which is why the
// writer used to put Rule 3 everywhere and could not reproduce the
// document's own "fo'we'is".
func TestReferential_Rule1GlottalPlacement(t *testing.T) {
	for _, w := range []string{
		"fo'we'is", // V_C1 Rule 1 before the Slot 3 w, V_C2 Rule 3
		"lai'wiš",  // V_C1 Rule 1 on a diphthong: ai + '
		"sme'e",    // no Slot 3, so V_C1 is final and takes Rule 3
		"ka'u",     // the same, on a diphthong
	} {
		r, err := ParseReferential(w)
		if err != nil {
			t.Errorf("ParseReferential(%s): %v", w, err)
			continue
		}
		out, err := Referential(r)
		if err != nil {
			t.Errorf("%s: %v", w, err)
			continue
		}
		if out != w {
			t.Errorf("%s wrote back as %q", w, out)
		}
	}
}
