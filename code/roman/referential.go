package roman

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
)

// Referential renders a §4.6.1 single- or dual-referential to its
// canonical romanization. Ultimate stress marks RPV Essence; every other
// form is left unmarked, which §4.6.1 gives as the default.
func Referential(r g.Referential) (string, error) {
	head, err := refHeadForm(r.Head)
	if err != nil {
		return "", err
	}
	vc1 := parse.CaseToVc(r.Case)
	bodies := []string{head + vc1}
	if s := r.Second; s != nil {
		tail := parse.CaseToVc(s.Case)
		if len(s.Refs) > 0 {
			tail += refChainForm(s.Refs)
		}
		// A V_C1 with a Slot 3 behind it is not word-final, so §1.7
		// Rule 1 can serve and Rule 3 only overrides it where it
		// cannot: the document's own "fo'we'is" writes case 43 as
		// "o'" rather than "o'o". The tables are keyed on the Rule 3
		// spelling, so the Rule 1 one is derived and offered first.
		vc1s := []string{vc1}
		if r1, ok := phonology.Rule1Glottal(vc1); ok {
			vc1s = []string{r1, vc1}
		}
		// §4.6.1 slot 3 is written "w/y + V_C2": one morpheme with two
		// spellings. Every combination goes forward and the cluster
		// rules choose.
		bodies = nil
		for _, v := range vc1s {
			for _, sep := range []string{"w", "y"} {
				bodies = append(bodies, head+v+sep+tail)
			}
		}
	}
	// §4.6.3: a suppletive cluster here takes "üo-", so the word is not
	// read as a modular adjunct.
	return finishReferential(bodies, r.Head, "üo", r.RpvEssence)
}

// CombinationReferential renders the §4.6.2 shape.
func CombinationReferential(c g.CombinationReferential) (string, error) {
	head, err := refHeadForm(c.Head)
	if err != nil {
		return "", err
	}
	spec, err := combinationSpecForm(c.Spec)
	if err != nil {
		return "", err
	}
	body := head + parse.CaseToVc(c.Case) + spec
	for _, a := range c.Affixes {
		v := parse.AffixVowel(a.Type, a.Degree)
		if v == "" {
			return "", fmt.Errorf("affix %+v has no V_X form", a)
		}
		body += v + a.Consonant
	}
	// §4.6.2 slot 5 is "(V_C2 or epenthetic -a)". The epenthetic vowel
	// keeps the word from ending on the affix consonant; with no
	// affixes and no second case there is nothing to separate, so it is
	// only needed when something precedes it.
	switch {
	case c.Case2 != nil:
		if *c.Case2 == g.THM {
			// §4.6.2 gives THM the dedicated form -üa here, "a" alone
			// being the epenthetic vowel that means no second case.
			body += "üa"
		} else {
			body += parse.CaseToVc(*c.Case2)
		}
	case len(c.Affixes) > 0:
		body += "a"
	}
	// §4.6.3: a suppletive cluster here takes "a-", so the word is not
	// read as a concatenated formative.
	return finishReferential([]string{body}, c.Head, "a", c.RpvEssence)
}

// finishReferential applies the epenthetic prefix a head may need and
// then the stress mark, and checks the result is a word that can be
// said.
//
// §4.6.3 makes the prefix obligatory in front of a suppletive cluster,
// and the two shapes use different vowels, so the caller names it. For
// a personal head the vowel is instead §4.6.1's epenthetic "-ë-",
// which appears only "if necessary due to phonotactic rules".
//
// The bodies are the spellings the caller could not choose between,
// and the prefix is chosen against a whole word rather than against
// one of them. The two are not independent: which Slot 3 separator is
// sayable depends on the cluster the prefix leaves in front of it, and
// judging the separator on the prefix-less body rejected "ëztewim",
// whose "zt" cannot open a word without the prefix that was not yet in
// play. Prefixes are the outer loop, so a spelling that needs no
// epenthetic vowel wins over one that does, which is the only ranking
// §4.6.1 states.
func finishReferential(bodies []string, head g.RefHead, suppletivePrefix string, rpv bool) (string, error) {
	prefixes := []string{"", "ë"}
	if _, ok := head.(g.SuppletiveHead); ok {
		prefixes = []string{suppletivePrefix}
	}
	var candidates []string
	for _, prefix := range prefixes {
		for _, body := range bodies {
			candidates = append(candidates, prefix+body)
		}
	}
	word, err := pickValid(candidates...)
	if err != nil {
		return "", err
	}
	if rpv {
		// §4.6.1 reads ultimate stress as RPV Essence and groups
		// "monosyllabic or penultimate" together as the default. A
		// one-syllable word therefore has no way to say RPV: its only
		// syllable is already the unmarked case, and Apply has nowhere
		// to put the mark. Say so rather than return a word that means
		// something else.
		marked := phonology.Apply(word, phonology.Ultimate)
		if marked == word {
			return "", fmt.Errorf(
				"%q is monosyllabic, so it cannot carry the ultimate stress that reads as RPV Essence (§4.6.1)", word)
		}
		word = marked
	}
	return phonology.DissimilateGlides(word), nil
}

// pickValid returns the first candidate that can be said.
func pickValid(candidates ...string) (string, error) {
	for _, w := range candidates {
		if phonology.Legal(w) {
			return w, nil
		}
	}
	return "", fmt.Errorf("no phonotactically valid spelling among %s",
		strings.Join(candidates, ", "))
}

// refHeadForm spells a referential head.
func refHeadForm(head g.RefHead) (string, error) {
	switch h := head.(type) {
	case g.SuppletiveHead:
		return parse.CarrierTypeForm(h.Type), nil
	case g.PersonalHead:
		if len(h.Refs) == 0 {
			return "", fmt.Errorf("personal head with no referents")
		}
		chain := refChainForm(h.Refs)
		if h.Category == nil {
			return chain, nil
		}
		return categoryForm(chain, *h.Category)
	}
	return "", fmt.Errorf("unknown referential head %T", head)
}

// refChainForm concatenates the consonant forms of a referent chain.
func refChainForm(refs []g.PersonalRef) string {
	var b strings.Builder
	for _, r := range refs {
		b.WriteString(parse.RefC1(r))
	}
	return phonology.DissimilateGlides(b.String())
}

// categoryForm attaches a §4.6 category affix to a referent chain.
//
// §4.6 says to add it "immediately preceding or following ... as
// phonotactically permissible", which usually settles the question on
// its own: of lça, lxa, çla and xla, only the two prefixed forms are
// clusters Ithkuil lets a word open with. Where more than one spelling
// survives the section does not choose between them, so we take the
// first it lists, its own order being the only ranking on offer. That
// ranking is ours, not Quijada's: çla and xla are both legal, as are
// tļma, mtļa and ļma, and the canonicalization heuristics in SPEC.md
// have nothing to say here, the candidates being identical in syllable
// count, glottal count and length. See ERRATA.md §4.6.
func categoryForm(chain string, cat g.RefCategory) (string, error) {
	var tried []string
	for _, f := range parse.RefCategoryForms {
		if f.Category != cat {
			continue
		}
		for _, candidate := range []struct {
			use  bool
			form string
		}{
			{f.Prefix, f.Form + chain},
			{f.Suffix, chain + f.Form},
		} {
			if !candidate.use {
				continue
			}
			// The category affix has to survive as a cluster on its
			// own; whether the whole word says is settled later, once
			// the case vowel is attached.
			if phonology.ClusterLegal(candidate.form) {
				return candidate.form, nil
			}
			tried = append(tried, candidate.form)
		}
	}
	return "", fmt.Errorf("no phonotactically valid %v spelling on %q among %s",
		cat, chain, strings.Join(tried, ", "))
}

// combinationSpecForm spells the §4.6.2 Specification marker.
func combinationSpecForm(s g.Specification) (string, error) {
	switch s {
	case g.BSC:
		return "x", nil
	case g.CTE:
		return "xt", nil
	case g.CSV:
		return "xp", nil
	case g.OBJ:
		return "xx", nil
	}
	return "", fmt.Errorf("unknown specification %v", s)
}
