package gloss

import (
	"fmt"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// MCSCs is the affix consonant for the MCS (Mood and Case-Scoping)
// affix per §3.8.1.1. Used on FRAMED verbal formatives to surface
// both a Mood and a Case-Scope simultaneously — the Slot VIII Cn
// covers one (Case-Scope by default for nominal/framed stress), the
// MCS affix in Slot VII covers the other.
const MCSCs = "bẓ"

// CHCCs is the affix consonant for the CHC (Degree of Choice by
// Externally-Induced Agent) affix per §5.8 — a gradient affix with
// nine degrees expressing how willing the induced agent is.
const CHCCs = "rz"

// mcsMoodToDegree is the MCS affix degree that encodes a non-default
// Mood. FAC has no MCS form (it is the unmarked default).
var mcsMoodToDegree = map[g.Mood]int{
	g.SUB: 1,
	g.ASM: 2,
	g.SPC: 3,
	g.COU: 4,
	g.HYP: 5,
}

// mcsCaseScopeToDegree is the MCS affix degree that encodes a non-default
// Case-Scope. CCN has no MCS form. Note degree 0 (CCV) per the spec
// lexicon note "0=(CCV) Successive".
var mcsCaseScopeToDegree = map[g.CaseScope]int{
	g.CCV: 0,
	g.CCA: 6,
	g.CCS: 7,
	g.CCQ: 8,
	g.CCP: 9,
}

// MCSDegreeForMood returns the MCS affix degree that encodes m. FAC
// is the unmarked default and has no MCS form — returns (0, false).
func MCSDegreeForMood(m g.Mood) (int, bool) {
	d, ok := mcsMoodToDegree[m]
	return d, ok
}

// MCSDegreeForCaseScope returns the MCS affix degree that encodes cs.
// CCN is the unmarked default — returns (0, false).
func MCSDegreeForCaseScope(cs g.CaseScope) (int, bool) {
	d, ok := mcsCaseScopeToDegree[cs]
	return d, ok
}

// WithMCSMood appends an MCS affix to Slot VII of f encoding the given
// Mood. Used per §3.8.1.1 on FRAMED verbal formatives to convey Mood
// in addition to the Case-Scope that Slot VIII's Cn already carries.
//
// Returns an error for FAC (the unmarked default — no MCS form exists).
// Does not validate the formative's Final variant; the caller is
// responsible for choosing the FRAMED context where MCS is meaningful.
func WithMCSMood(f g.Formative, m g.Mood) (g.Formative, error) {
	d, ok := MCSDegreeForMood(m)
	if !ok {
		return f, fmt.Errorf("mood %v has no MCS encoding (default FAC needs no MCS)", m)
	}
	f.SlotVII = append(f.SlotVII, g.Affix{
		Type:      g.Type1Affix,
		Degree:    d,
		Consonant: MCSCs,
	})
	return f, nil
}

// WithMCSCaseScope appends an MCS affix to Slot VII of f encoding the
// given Case-Scope. Mirror of WithMCSMood for nominal contexts.
//
// Returns an error for CCN (the unmarked default).
func WithMCSCaseScope(f g.Formative, cs g.CaseScope) (g.Formative, error) {
	d, ok := MCSDegreeForCaseScope(cs)
	if !ok {
		return f, fmt.Errorf("case-scope %v has no MCS encoding (default CCN needs no MCS)", cs)
	}
	f.SlotVII = append(f.SlotVII, g.Affix{
		Type:      g.Type1Affix,
		Degree:    d,
		Consonant: MCSCs,
	})
	return f, nil
}

// WithCHC appends a CHC affix at the given degree (1-9) to Slot VII of
// f. Per §5.8, the nine degrees rank the induced agent's willingness
// from "cannot stop it" (1) to "intentionally helps for own benefit" (9).
//
// Returns an error for degrees outside 1-9.
func WithCHC(f g.Formative, degree int) (g.Formative, error) {
	if degree < 1 || degree > 9 {
		return f, fmt.Errorf("CHC degree %d out of range (must be 1-9)", degree)
	}
	f.SlotVII = append(f.SlotVII, g.Affix{
		Type:      g.Type1Affix,
		Degree:    degree,
		Consonant: CHCCs,
	})
	return f, nil
}
