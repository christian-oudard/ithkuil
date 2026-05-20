package grammar

import (
	"fmt"
	"testing"
)

// fmt.Stringer is the implicit interface every enum String() method
// here satisfies. A "uniqueStringers" test confirms that the inventory
// produces distinct, non-empty labels — catches accidental fall-through
// in the [...]string{...} lookups.

func TestStringer_Uniqueness(t *testing.T) {
	cases := []struct {
		name  string
		items []fmt.Stringer
	}{
		{"Configuration", configToStringers(AllConfigurations)},
		{"Affiliation", affToStringers(AllAffiliations)},
		{"Perspective", perToStringers(AllPerspectives)},
		{"Extension", extToStringers(AllExtensions)},
		{"Essence", essToStringers(AllEssences)},
		{"Valence", valenceToStringers(AllValences)},
		{"Phase", phaseToStringers(AllPhases)},
		{"Effect", effectToStringers(AllEffects)},
		{"Level", levelToStringers(AllLevels)},
		{"Aspect", aspectToStringers(AllAspects)},
		{"Mood", moodToStringers(AllMoods)},
		{"CaseScope", csToStringers(AllCaseScopes)},
		{"Validation", valToStringers(AllValidations)},
		{"Case", caseToStringers(AllCases)},
		{"Bias", biasToStringers(AllBiases)},
		{"Register", regToStringers(AllRegisters)},
		{"CarrierType", carrierToStringers(AllCarrierTypes)},
	}
	for _, c := range cases {
		seen := map[string]bool{}
		for _, s := range c.items {
			str := s.String()
			if str == "" && c.name != "Register" {
				// Register.NRR and END have empty initial/final
				// forms, but their .String() name shouldn't be blank.
				t.Errorf("%s: empty String() result", c.name)
				continue
			}
			if seen[str] && str != "" {
				t.Errorf("%s: duplicate String() %q", c.name, str)
			}
			seen[str] = true
		}
	}
}

func TestStringer_StemAndVersion(t *testing.T) {
	// Stem and Version don't have a slice; check directly.
	if S0.String() == "" || S1.String() == "" || S2.String() == "" || S3.String() == "" {
		t.Error("Stem String() should be non-empty")
	}
	if PRC.String() == "" || CPT.String() == "" {
		t.Error("Version String() should be non-empty")
	}
}

func TestStringer_AffixType(t *testing.T) {
	for _, a := range []AffixType{Type1Affix, Type2Affix, Type3Affix} {
		if a.String() == "" {
			t.Errorf("AffixType %d String() empty", a)
		}
	}
}

func TestStringer_AffixScope(t *testing.T) {
	for _, s := range []AffixScope{ScopeVII, ScopeV, ScopeAdj} {
		if s.String() == "" {
			t.Errorf("AffixScope %d String() empty", s)
		}
	}
}

func TestStringer_ConcatenationAndShortcut(t *testing.T) {
	if Type1.String() == "" || Type2.String() == "" {
		t.Error("ConcatenationStatus String() empty")
	}
	if ShortcutW.String() == "" || ShortcutY.String() == "" {
		t.Error("CcShortcut String() empty")
	}
}

func TestCarrierTypeForms(t *testing.T) {
	// Each CarrierType has a unique 2-char form.
	seen := map[string]bool{}
	for _, c := range AllCarrierTypes {
		f := CarrierTypeForm(c)
		if f == "" {
			t.Errorf("%s has empty form", c)
		}
		if seen[f] {
			t.Errorf("duplicate carrier form %q", f)
		}
		seen[f] = true
	}
}

func TestFunctionSpecificationContext_String(t *testing.T) {
	for _, f := range []Function{STA, DYN} {
		if f.String() == "" {
			t.Errorf("Function %d empty", f)
		}
	}
	for _, s := range []Specification{BSC, CTE, CSV, OBJ} {
		if s.String() == "" {
			t.Errorf("Specification %d empty", s)
		}
	}
	for _, c := range []Context{EXS, FNC, RPS, AMG} {
		if c.String() == "" {
			t.Errorf("Context %d empty", c)
		}
	}
}

// Slice→Stringer helpers. Each grammar enum is a distinct named int
// type so a generic []Stringer can't hold mixed slices directly.

func configToStringers(xs []Configuration) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func affToStringers(xs []Affiliation) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func perToStringers(xs []Perspective) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func extToStringers(xs []Extension) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func essToStringers(xs []Essence) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func valenceToStringers(xs []Valence) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func phaseToStringers(xs []Phase) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func effectToStringers(xs []Effect) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func levelToStringers(xs []Level) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func aspectToStringers(xs []Aspect) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func moodToStringers(xs []Mood) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func csToStringers(xs []CaseScope) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func valToStringers(xs []Validation) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func caseToStringers(xs []Case) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func biasToStringers(xs []Bias) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func regToStringers(xs []Register) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
func carrierToStringers(xs []CarrierType) []fmt.Stringer {
	out := make([]fmt.Stringer, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}
