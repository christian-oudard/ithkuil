package render

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
)

// Referential renders a §4.6.1 single- or dual-referential to its
// canonical surface. Ultimate stress marks RPV Essence; every other
// form is left unmarked, which §4.6.1 gives as the default.
func Referential(r g.Referential) (string, error) {
	head, err := refHeadForm(r.Head)
	if err != nil {
		return "", err
	}
	body := head + g.CaseToVc(r.Case)
	if s := r.Second; s != nil {
		// §4.6.1 slot 3 is written "w/y + V_C2". The two are one
		// morpheme with two spellings, so take whichever the cluster
		// rules admit next to the vowel that follows.
		tail, err := pickValid(func(sep string) string {
			out := body + sep + g.CaseToVc(s.Case)
			if len(s.Refs) > 0 {
				out += refChainForm(s.Refs)
			}
			return out
		}, "w", "y")
		if err != nil {
			return "", fmt.Errorf("referential second referent: %w", err)
		}
		body = tail
	}
	// §4.6.3: a suppletive cluster here takes "üo-", so the word is not
	// read as a modular adjunct.
	return finishReferential(body, r.Head, "üo", r.RpvEssence)
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
	body := head + g.CaseToVc(c.Case) + spec
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
			body += g.CaseToVc(*c.Case2)
		}
	case len(c.Affixes) > 0:
		body += "a"
	}
	// §4.6.3: a suppletive cluster here takes "a-", so the word is not
	// read as a concatenated formative.
	return finishReferential(body, c.Head, "a", c.RpvEssence)
}

// finishReferential applies the epenthetic prefix a head may need and
// then the stress mark, and checks the result is a word that can be
// said.
//
// §4.6.3 makes the prefix obligatory in front of a suppletive cluster,
// and the two shapes use different vowels, so the caller names it. For
// a personal head the vowel is instead §4.6.1's epenthetic "-ë-",
// which appears only "if necessary due to phonotactic rules", so the
// bare form is tried first.
func finishReferential(body string, head g.RefHead, suppletivePrefix string, rpv bool) (string, error) {
	prefixes := []string{"", "ë"}
	if _, ok := head.(g.SuppletiveHead); ok {
		prefixes = []string{suppletivePrefix}
	}
	word, err := pickValid(func(prefix string) string { return prefix + body }, prefixes...)
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
				"%q is monosyllabic, so it cannot carry the ultimate stress §4.6.1 reads as RPV Essence", word)
		}
		word = marked
	}
	return word, nil
}

// pickValid builds a candidate from each option in turn and returns
// the first that passes the phonotactic checks.
func pickValid(build func(string) string, options ...string) (string, error) {
	var tried []string
	for _, o := range options {
		w := build(o)
		if phonology.Legal(w) {
			return w, nil
		}
		tried = append(tried, w)
	}
	return "", fmt.Errorf("no phonotactically valid spelling among %s",
		strings.Join(tried, ", "))
}

// refHeadForm spells a referential head.
func refHeadForm(head g.RefHead) (string, error) {
	switch h := head.(type) {
	case g.SuppletiveHead:
		return g.CarrierTypeForm(h.Type), nil
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
	return b.String()
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
// count, glottal count and length.
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
