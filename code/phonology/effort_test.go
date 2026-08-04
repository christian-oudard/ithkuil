package phonology

import (
	"math"
	"testing"
)

func ph(t *testing.T, s string) Phoneme {
	t.Helper()
	for _, r := range s {
		p, ok := phonemeOf[r]
		if !ok {
			t.Fatalf("no phoneme for %q", s)
		}
		return p
	}
	t.Fatalf("empty segment")
	return nil
}

// The whole design rests on effort not being monotonic in ALINE's
// distance. ALINE was built to align cognates, so its distance falls
// to zero for a repeated segment; effort does not, because adjacent
// near-identical segments are what §2 spends most of its rules
// barring. A geminate must therefore cost more than a moderately
// different pair at the same place.
func TestGeminateCostsMoreThanANearNeighbour(t *testing.T) {
	pp := Transition(ph(t, "p"), ph(t, "p"), false)
	pt := Transition(ph(t, "p"), ph(t, "t"), false)
	if pp <= pt {
		t.Errorf("pp costs %.3f and pt %.3f; a geminate should cost more "+
			"than a pair one place apart", pp, pt)
	}
	// But finitely more: §1.7 permits geminates and bars only the
	// triple, so this arm must not behave like a prohibition.
	if math.IsInf(pp, 0) || pp > 4*pt {
		t.Errorf("pp costs %.3f against pt %.3f; §1.7 permits geminates, so "+
			"the similarity penalty is too steep", pp, pt)
	}
}

// The two arms of the U, checked against its minimum rather than
// against each other. Cost falls as a pair becomes less alike, reaches
// a minimum, then rises as the articulators have further to travel.
// Asserting that cost rises monotonically with distance would
// contradict the design; the actual claim is that both extremes exceed
// the middle.
func TestCostIsUShapedInDistance(t *testing.T) {
	cost := func(d float64) float64 {
		return d*travelWeight + similarityPenalty/(d+similarityFloor)
	}
	best, at := math.Inf(1), 0.0
	for d := 0.0; d <= 150; d++ {
		if c := cost(d); c < best {
			best, at = c, d
		}
	}
	t.Logf("minimum %.3f at distance %.0f; identical pair %.3f, furthest %.3f",
		best, at, cost(0), cost(150))
	if cost(0) <= best || cost(150) <= best {
		t.Errorf("not U-shaped: identical %.3f, minimum %.3f, furthest %.3f",
			cost(0), best, cost(150))
	}
	if at <= 0 || at >= 150 {
		t.Errorf("minimum at distance %.0f is at an end, not in the middle", at)
	}
}

// §1.5's subject: a consonant meeting a consonant across a word
// boundary is the case the grammar says usually needs a vowel
// inserted. The boundary flag has to make that pair dearer than the
// same pair inside a word, or nothing will ever prefer the spelling
// that avoids it.
func TestABoundaryCostsMoreThanNoBoundary(t *testing.T) {
	within := Transition(ph(t, "l"), ph(t, "h"), false)
	across := Transition(ph(t, "l"), ph(t, "h"), true)
	if across <= within {
		t.Errorf("l|h across a boundary costs %.3f, within a word %.3f", across, within)
	}
}

// Filling the elided Slot IX default is what §1.5 prescribes as the
// remedy, so it must come out cheaper. mlaläh and mlaläha are the same
// grammar; only the second ends in a vowel.
func TestFillingSlotIXIsCheaperBeforeAConsonant(t *testing.T) {
	bare := TextEnergy([]string{"mlaläh", "hla"})
	filled := TextEnergy([]string{"mlaläha", "hla"})
	if filled >= bare {
		t.Errorf("mlaläha hla costs %.3f and mlaläh hla %.3f; §1.5 says the "+
			"filled form is the remedy", filled, bare)
	}
}

// §2's opening sentence gives its own reason: the conjuncts it bars
// are those that are difficult or indistinguishable. The model does
// not decide legality, but if it scored the barred pairs as cheap it
// would be measuring something other than effort. Checked over every
// two-consonant pair in the inventory.
func TestProhibitedPairsCostMoreOnAverage(t *testing.T) {
	var barred, allowed []float64
	for _, a := range Consonants {
		ca, ok := a.Phoneme.(Consonant)
		if !ok || a.Text == "'" {
			continue
		}
		for _, b := range Consonants {
			cb, ok := b.Phoneme.(Consonant)
			if !ok || b.Text == "'" {
				continue
			}
			cost := Transition(ca, cb, false)
			if ClusterLegal(a.Text + b.Text) {
				allowed = append(allowed, cost)
			} else {
				barred = append(barred, cost)
			}
		}
	}
	if len(barred) == 0 || len(allowed) == 0 {
		t.Fatalf("degenerate split: %d barred, %d allowed", len(barred), len(allowed))
	}
	mb, ma := mean(barred), mean(allowed)
	t.Logf("%d pairs barred by §2 average %.3f; %d permitted average %.3f",
		len(barred), mb, len(allowed), ma)
	if mb <= ma {
		t.Errorf("§2-barred pairs average %.3f, permitted %.3f; the barred set "+
			"should be the dearer one", mb, ma)
	}
}

func mean(xs []float64) float64 {
	var s float64
	for _, x := range xs {
		s += x
	}
	return s / float64(len(xs))
}
