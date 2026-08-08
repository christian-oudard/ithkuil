package slots

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/phonology"
)

// The effort model is fitted to a speaker (see phonology/effort.go),
// and this is the other check on it: does it agree with Quijada about
// where a Slot IX default is worth writing?
//
// Every corpus position where the default could go either way is one
// judgment. He wrote it in 126 of them and elided it in 18. The model
// scores both spellings in the context of the following word, since
// §1.5 makes the choice depend on it, and prefers whichever is cheaper.
//
// Pinned rather than asserted as a floor, so it fails in either
// direction. A weight that moves this unnoticed is the failure mode:
// the numbers in effort.go are set rather than fitted, and nothing else
// in the tree would show a change.
//
// Read the base rate before reading the score. He writes the default far
// more often than he elides it, so a model that always filled would
// score the written positions perfectly and the elided ones at zero,
// which is most of the corpus and looks like skill. An earlier version
// did exactly that and was reported at 87.5% agreement; that number was
// the majority class and nothing else. The model discriminates now, and
// scores below what always-filling would. That is worse arithmetic and
// better evidence, and it is the reason to correct the model before
// wiring it into pickValid rather than after.
func TestEffortAgreesWithTheCorpusOnElision(t *testing.T) {
	const (
		wantWroteAgree  = 77
		wantWroteTotal  = 112
		wantElidedAgree = 14
		wantElidedTotal = 18
	)

	var wroteAgree, wroteTotal, elidedAgree, elidedTotal int
	for _, ex := range corpus.Examples() {
		for _, clause := range strings.FieldsFunc(ex.Ithkuil, func(r rune) bool {
			return strings.ContainsRune(".,;:!?", r)
		}) {
			words := strings.Fields(clause)
			for i := 0; i+1 < len(words); i++ {
				a := bareWord(words[i])
				b := bareWord(words[i+1])
				if a == "" || b == "" {
					continue
				}
				runes := []rune(a)
				if !phonology.IsVowel(runes[len(runes)-1]) {
					// He elided; would the model have?
					filled := a + "a"
					if !phonology.Legal(filled) {
						continue
					}
					elidedTotal++
					if phonology.TextEnergy([]string{a, b}) <=
						phonology.TextEnergy([]string{filled, b}) {
						elidedAgree++
					}
					continue
				}
				// He wrote it. Only count where eliding was available.
				lay, err := Parse(a)
				if err != nil || lay.Vc != "a" {
					continue
				}
				elided := strings.TrimSuffix(a, "a")
				if elided == "" || !phonology.Legal(elided) {
					continue
				}
				wroteTotal++
				if phonology.TextEnergy([]string{a, b}) <=
					phonology.TextEnergy([]string{elided, b}) {
					wroteAgree++
				}
			}
		}
	}

	if wroteTotal != wantWroteTotal || elidedTotal != wantElidedTotal {
		t.Errorf("corpus offers %d written and %d elided positions, want %d and %d; "+
			"the corpus or the parser changed, not the model",
			wroteTotal, elidedTotal, wantWroteTotal, wantElidedTotal)
	}
	if wroteAgree != wantWroteAgree {
		t.Errorf("agrees on %d of %d written positions, want %d",
			wroteAgree, wroteTotal, wantWroteAgree)
	}
	if elidedAgree != wantElidedAgree {
		t.Errorf("agrees on %d of %d elided positions, want %d. If this rose, "+
			"say so, and check it against the written positions: a model "+
			"that stops filling scores better here and worse there",
			elidedAgree, elidedTotal, wantElidedAgree)
	}
}

func bareWord(w string) string {
	s, _ := phonology.Strip(strings.ToLower(strings.Trim(w, ".,;:!?\"()[]«»")))
	return s
}
