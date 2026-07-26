package allomorph

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/validation"
)

// allSlotVI enumerates the whole Ca space: 9 configurations x 4
// affiliations x 4 perspectives x 6 extensions x 2 essences.
func allSlotVI(yield func(g.SlotVI)) {
	for _, c := range g.AllConfigurations {
		for _, a := range g.AllAffiliations {
			for _, p := range g.AllPerspectives {
				for _, e := range g.AllExtensions {
					for _, ess := range g.AllEssences {
						yield(g.SlotVI{
							Configuration: c, Affiliation: a,
							Perspective: p, Extension: e, Essence: ess,
						})
					}
				}
			}
		}
	}
}

// TestCa_EveryFormIsPronounceable sweeps the entire Ca space and
// checks each form against the phonotactics. The allomorphic
// substitutions in §3.6 exist precisely to keep the composed cluster
// sayable, so a form that fails here means a substitution is missing
// or wrong — which is how the absent ngn → ňn rule let MSC compose to
// the §2.23-prohibited "nň".
//
// The space is small enough to enumerate outright. Sampling it, as the
// fullparse fuzz does, reaches a form like that only by chance.
func TestCa_EveryFormIsPronounceable(t *testing.T) {
	allSlotVI(func(s g.SlotVI) {
		ca := ConstructCa(s)
		if r := validation.ValidateClusterAt(validation.Medial, ca); !r.Valid {
			t.Errorf("Ca %v composes to %q (raw %q): %v",
				s, ca, ConstructCaRaw(s), r.Errors)
		}
	})
}

// TestCa_EveryGeminateIsPronounceable is the same sweep for the §3.6.1
// geminated forms, which is what Slot V affixes put on the surface.
func TestCa_EveryGeminateIsPronounceable(t *testing.T) {
	allSlotVI(func(s g.SlotVI) {
		ca := ConstructCa(s)
		gem := GeminateCa(ca)
		if r := validation.ValidateClusterAt(validation.Medial, gem); !r.Valid {
			t.Errorf("Ca %v geminates %q to %q: %v", s, ca, gem, r.Errors)
		}
	})
}

// TestCa_EveryFormParsesBack checks that the surface form identifies
// its SlotVI uniquely. A substitution that collapses two distinct Ca
// values onto one cluster is unrecoverable at parse time.
func TestCa_EveryFormParsesBack(t *testing.T) {
	allSlotVI(func(s g.SlotVI) {
		ca := ConstructCa(s)
		got, ok := ParseCa(ca)
		if !ok {
			t.Errorf("Ca %v composes to %q, which ParseCa rejects", s, ca)
			return
		}
		if got != s {
			t.Errorf("Ca %q parses back as %v, want %v", ca, got, s)
		}
	})
}
