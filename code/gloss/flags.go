package gloss

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
)

var concatPrefix = map[string]g.ConcatenationStatus{
	"T1": g.Type1,
	"T2": g.Type2,
}

// ApplyFlag mutates f according to one grammar-abbreviation flag like
// "S2", "DYN", "OBJ", "ERG", "RTR", "PEN". Case-insensitive. Returns
// an error for unrecognized flags.
//
// Recognized flag families:
//
//	S0..S3                Stem
//	PRC | CPT             Version
//	STA | DYN             Function
//	BSC | CTE | CSV | OBJ Specification
//	EXS | FNC | RPS | AMG Context
//	MON | PEN | ULT | ANT Stress
//	<Case>                Slot IX case (any of 68)
//	<Aspect>              Slot VIII aspect (with CCN case-scope)
//	<Valence>             Slot VIII valence (with FAC mood)
//	<Mood>                Slot VIII mood (wraps existing Slot VIII)
//	<Illocution>          Slot IX illocution (forces ultimate stress)
func ApplyFlag(f *g.Formative, flag string) error {
	flag = strings.ToUpper(flag)

	// Concatenation status.
	if c, ok := concatPrefix[flag]; ok {
		f.Concat = c
		return nil
	}

	// Stem (CrRoot only).
	switch flag {
	case "S0", "S1", "S2", "S3":
		stem := map[string]g.Stem{"S0": g.S0, "S1": g.S1, "S2": g.S2, "S3": g.S3}[flag]
		cr, ok := f.Root.(g.CrRoot)
		if !ok {
			return fmt.Errorf("stem %s only applies to CrRoot formatives", flag)
		}
		cr.Stem = stem
		f.Root = cr
		return nil
	}

	// Version (CrRoot / CsRoot / RefRoot).
	switch flag {
	case "PRC", "CPT":
		v := g.PRC
		if flag == "CPT" {
			v = g.CPT
		}
		switch r := f.Root.(type) {
		case g.CrRoot:
			r.Version = v
			f.Root = r
		case g.CsRoot:
			r.Version = v
			f.Root = r
		case g.RefRoot:
			r.Version = v
			f.Root = r
		}
		return nil
	}

	// Function.
	switch flag {
	case "STA", "DYN":
		fn := g.STA
		if flag == "DYN" {
			fn = g.DYN
		}
		switch r := f.Root.(type) {
		case g.CrRoot:
			r.SlotIV.Function = fn
			f.Root = r
		case g.CsRoot:
			r.Function = fn
			f.Root = r
		case g.RefRoot:
			r.SlotIV.Function = fn
			f.Root = r
		}
		return nil
	}

	// Specification (CrRoot / RefRoot — CsRoot is implicitly BSC).
	switch flag {
	case "BSC", "CTE", "CSV", "OBJ":
		s := map[string]g.Specification{"BSC": g.BSC, "CTE": g.CTE, "CSV": g.CSV, "OBJ": g.OBJ}[flag]
		switch r := f.Root.(type) {
		case g.CrRoot:
			r.SlotIV.Specification = s
			f.Root = r
		case g.RefRoot:
			r.SlotIV.Specification = s
			f.Root = r
		default:
			return fmt.Errorf("specification %s only applies to CrRoot/RefRoot", flag)
		}
		return nil
	}

	// Context.
	switch flag {
	case "EXS", "FNC", "RPS", "AMG":
		c := map[string]g.Context{"EXS": g.EXS, "FNC": g.FNC, "RPS": g.RPS, "AMG": g.AMG}[flag]
		switch r := f.Root.(type) {
		case g.CrRoot:
			r.SlotIV.Context = c
			f.Root = r
		case g.CsRoot:
			r.Context = c
			f.Root = r
		case g.RefRoot:
			r.SlotIV.Context = c
			f.Root = r
		}
		return nil
	}

	// Stress is encoded in the Final variant. PEN/MON → nominal,
	// ANT → framed verbal, ULT → verbal (default Assertive/OBS).
	switch flag {
	case "PEN", "MON":
		f.Final = g.UnframedNominal{Case: currentCase(f.Final)}
		return nil
	case "ANT":
		f.Final = g.FramedVerbal{Case: currentCase(f.Final)}
		return nil
	case "ULT":
		if _, ok := f.Final.(g.UnframedVerbal); !ok {
			f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
		}
		return nil
	}

	// Case (any of 68): nominal or framed-nominal Final.
	for _, c := range g.AllCases {
		if c.String() == flag {
			if _, framed := f.Final.(g.FramedVerbal); framed {
				f.Final = g.FramedVerbal{Case: c}
			} else {
				f.Final = g.UnframedNominal{Case: c}
			}
			return nil
		}
	}

	// Aspect.
	for _, a := range g.AllAspects {
		if a.String() == flag {
			f.SlotVIII = g.VnCnAspect{Aspect: a, MoodScope: g.FAC}
			return nil
		}
	}

	// Valence.
	for _, v := range g.AllValences {
		if v.String() == flag {
			f.SlotVIII = g.VnCnValence{Valence: v, MoodScope: g.FAC}
			return nil
		}
	}

	// Phase.
	for _, p := range g.AllPhases {
		if p.String() == flag {
			f.SlotVIII = g.VnCnPhase{Phase: p, MoodScope: g.FAC}
			return nil
		}
	}

	// Effect.
	for _, e := range g.AllEffects {
		if e.String() == flag {
			f.SlotVIII = g.VnCnEffect{Effect: e, MoodScope: g.FAC}
			return nil
		}
	}

	// Level.
	for _, lv := range g.AllLevels {
		if lv.String() == flag {
			f.SlotVIII = g.VnCnLevel{Level: lv, MoodScope: g.FAC}
			return nil
		}
	}

	// CaseScope: same underlying field as Mood (Cn position encodes
	// both). Map to the Mood counterpart via CaseScopeToMood.
	for _, c := range g.AllCaseScopes {
		if c.String() == flag {
			return applyMoodScope(f, g.CaseScopeToMood(c))
		}
	}

	// Mood: replaces MoodScope on whatever SlotVIII variant is there.
	for _, m := range g.AllMoods {
		if m.String() == flag {
			return applyMoodScope(f, m)
		}
	}

	// Illocution: forces UnframedVerbal Final.
	if vk, ok := illocutionByName(flag); ok {
		f.Final = g.UnframedVerbal{Vk: vk}
		return nil
	}

	// Validation: only meaningful on Assertive illocution. Replace
	// the Vk if the current Final is already Assertive; otherwise
	// promote to UnframedVerbal{Assertive{Validation: v}}.
	for _, v := range g.AllValidations {
		if v.String() == flag {
			f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: v}}
			return nil
		}
	}

	// Ca components (Affiliation / Configuration / Extension /
	// Perspective / Essence). Each enum is disjoint so dispatch by
	// matching the abbreviation against every enum's String().
	if applyCaFlag(f, flag) {
		return nil
	}

	return unlisted(flag, "grammatical value", flag)
}

// applyMoodScope sets the MoodScope on the existing SlotVIII variant,
// or creates a Valence-MNO SlotVIII to carry it when absent.
func applyMoodScope(f *g.Formative, m g.Mood) error {
	switch s := f.SlotVIII.(type) {
	case g.VnCnValence:
		s.MoodScope = m
		f.SlotVIII = s
	case g.VnCnAspect:
		s.MoodScope = m
		f.SlotVIII = s
	case g.VnCnPhase:
		s.MoodScope = m
		f.SlotVIII = s
	case g.VnCnEffect:
		s.MoodScope = m
		f.SlotVIII = s
	case g.VnCnLevel:
		s.MoodScope = m
		f.SlotVIII = s
	default:
		f.SlotVIII = g.VnCnValence{Valence: g.MNO, MoodScope: m}
	}
	return nil
}

// applyCaFlag tries to interpret flag as one of the five Ca-complex
// abbreviations and mutate f.SlotVI accordingly. Returns true if the
// flag matched a Ca component.
func applyCaFlag(f *g.Formative, flag string) bool {
	for _, c := range g.AllConfigurations {
		if c.String() == flag {
			f.SlotVI.Configuration = c
			return true
		}
	}
	for _, a := range g.AllAffiliations {
		if a.String() == flag {
			f.SlotVI.Affiliation = a
			return true
		}
	}
	for _, e := range g.AllExtensions {
		if e.String() == flag {
			f.SlotVI.Extension = e
			return true
		}
	}
	for _, p := range g.AllPerspectives {
		if p.String() == flag {
			f.SlotVI.Perspective = p
			return true
		}
	}
	for _, e := range g.AllEssences {
		if e.String() == flag {
			f.SlotVI.Essence = e
			return true
		}
	}
	return false
}

// currentCase pulls the Case out of a nominal/framed-verbal Final, or
// returns THM when the Final is verbal.
func currentCase(fin g.Final) g.Case {
	switch v := fin.(type) {
	case g.UnframedNominal:
		return v.Case
	case g.FramedVerbal:
		return v.Case
	}
	return g.THM
}

// illocutionByName returns the Vk variant for a 3-letter illocution
// abbreviation.
func illocutionByName(name string) (g.Vk, bool) {
	switch name {
	case "ASR":
		return g.Assertive{Validation: g.OBS}, true
	case "DIR":
		return g.Directive{}, true
	case "DEC":
		return g.Declarative{}, true
	case "IRG":
		return g.Interrogative{}, true
	case "VER":
		return g.Verificative{}, true
	case "ADM":
		return g.Admonitive{}, true
	case "POT":
		return g.Potentiative{}, true
	case "HOR":
		return g.Hortative{}, true
	case "CNJ":
		return g.Conjectural{}, true
	}
	return nil, false
}
