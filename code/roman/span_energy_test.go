package roman_test

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/roman"
)

// What the effort model is for, now that rules do the choosing.
//
// roman.Text picks a spelling per word from two rules: end in a vowel
// when another word follows (§1.5), and never end in a bare -h. Neither
// mentions effort. phonology.Energy measures effort and nothing else,
// on a scale fitted to a speaker's pairwise judgments. So the model is
// an independent check on the rules, and it answers the question a rule
// cannot answer about itself: is the spelling the rules chose actually
// easy to say, or did a tidy rule land on an awkward word?
//
// The check is local rather than global. Junction costs couple only
// adjacent words, so swapping one word's spelling and re-scoring the
// span finds a rule that left an easy win on the table without a
// Viterbi pass that would need its own test.
//
// It stands at 34 of 452 positions, and the 34 are not noise. They are
// two families, each a decision the rules do not currently make:
//
//  1. Twenty-seven want the §3.2 Ca shortcut, which the canonical
//     ranking takes only when it buys a syllable: onţlal -> wonţla,
//     avsal -> wavsa, učral -> wučra, ebglahlá -> webglahlá. The
//     mechanism is §1.2's unwritten glottal onset. A word spelled
//     vowel-initially still begins with a glottal stop that "must
//     still be pronounced", and the shortcut puts a w- or y- there
//     instead, which is cheaper. Most of these are span-final, where
//     rule 1 does not reach.
//
//  2. Seven want the §3.9.1 glottal left where it was rather than
//     moved earlier: za'lëi -> zalë'i, ušvi'lei -> ušvile'i,
//     amtri'lëi -> amtrilë'i, kši'lütřackoi -> kšilütřacko'i. In every
//     one the two spellings have the same syllable count, so moving
//     the glottal buys nothing and costs the intervocalic position.
//
// Family 2 also settles what looked like a contradiction in the
// speaker's judgments. ma'ala over ma'la and mala'i over ma'lai put the
// glottal between two vowels; kši'la over kšila'a does not. The
// difference is length: the first two are free, kšila'a costs a
// syllable. One rule covers all three, and it is the rule this family
// is asking for.
//
// Pinned rather than asserted at zero, because closing either family is
// a decision about the language and not a bug fix. A failure is
// informative either way: the rules regressing shows up as a span made
// harder to say, and a weight moving in effort.go shows up here as a
// disagreement about a real word rather than as a number in a log.
func TestRulesChooseSpellingsTheEffortModelCannotBeat(t *testing.T) {
	const wantBeaten = 34
	const wantPositions = 452

	beaten, spans, positions := 0, 0, 0
	var worst string
	var worstGap float64

	for _, ex := range corpus.Examples() {
		span, err := roman.ParseText(ex.Ithkuil)
		if err != nil {
			continue
		}
		out, err := roman.Text(span)
		if err != nil {
			continue
		}
		chosen := strings.Fields(out)
		if len(chosen) != len(span) {
			continue // a chain writes more words than the span has tokens
		}
		spans++
		base := phonology.TextEnergy(chosen)
		for i, w := range span {
			cands, err := roman.Spellings(w)
			if err != nil || len(cands) < 2 {
				continue
			}
			positions++
			for _, c := range cands {
				if c == chosen[i] {
					continue
				}
				trial := append([]string(nil), chosen...)
				trial[i] = c
				gap := base - phonology.TextEnergy(trial)
				if gap > 1e-9 {
					beaten++
					if gap > worstGap {
						worstGap, worst = gap, chosen[i]+" -> "+c+" in "+out
					}
					break
				}
			}
		}
	}

	if positions != wantPositions {
		t.Errorf("swept %d positions with a choice, want %d", positions, wantPositions)
	}
	if beaten != wantBeaten {
		t.Errorf("effort model beats the rules at %d of %d positions, want %d; worst is %s (%.3f)",
			beaten, positions, wantBeaten, worst, worstGap)
	}
	t.Logf("%d spans, %d positions with a choice, %d beaten", spans, positions, beaten)
}
