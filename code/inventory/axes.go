package inventory

import (
	g "github.com/christian-oudard/ithkuil/grammar"
)

// Axis is one grammatical dimension of a formative, holding the values
// it can take as edits to a baseline. Samples walks an axis one value
// at a time; Pairs walks two axes at once.
//
// Both read this one list. Two lists of every value in the language
// would drift, and the drift would be the invisible kind: a value the
// second list had missed would still be swept singly and look covered.
type Axis struct {
	// Name is the dimension a failure message points at. It is also
	// what pairs are formed over, so the nine Case groups are one axis
	// here and nine categories in the tables.
	Name string
	// Verbal marks an axis whose values only exist on a verbal
	// formative: illocution, validation, and Mood, which is the Slot
	// VIII field a nominal formative labels as case-scope instead.
	Verbal bool
	Values []Value
}

// Value is one setting of an Axis.
type Value struct {
	// Category is the value's coordinate in search.Table, which is the
	// Axis name except for Case. Empty means the Axis name.
	Category string
	Abbrev   string
	// Default marks the category's unmarked value, which the language
	// expresses by saying nothing.
	Default bool
	Apply   func(*g.Formative)
}

func (v Value) category(a Axis) string {
	if v.Category != "" {
		return v.Category
	}
	return a.Name
}

// Baseline is the formative an axis's values are applied to.
func (a Axis) Baseline() g.Formative {
	if a.Verbal {
		return verbal()
	}
	return nominal()
}

// AffixSlotAxis names the one axis that is not a value in any published
// table, so Samples can leave it out while Pairs sweeps it.
const AffixSlotAxis = "AffixSlot"

// AffixCs is the affix every affix-carrying sample uses: -r-, Negation,
// at degree 6. Any attested affix would do. What matters is that it is
// one consonant, so the slot it sits in is the only thing that moves.
const AffixCs = "r"

func affix() g.Affix {
	return g.Affix{Type: g.Type1Affix, Degree: 6, Consonant: AffixCs}
}

// Axes returns every formative-carried dimension, in the order the
// slots appear in a word.
func Axes() []Axis {
	var out []Axis
	add := func(a Axis) { out = append(out, a) }

	// Case (§4.4). The nine groups are one category to the grammar and
	// nine to the tables; keep the table's names so a failure points at
	// the page it is on.
	caseAxis := Axis{Name: "Case"}
	for _, c := range g.AllCases {
		caseAxis.Values = append(caseAxis.Values, Value{
			Category: "Case/" + c.Group().String(),
			Abbrev:   c.String(),
			Default:  c == g.THM,
			Apply:    func(f *g.Formative) { f.Final = g.UnframedNominal{Case: c} },
		})
	}
	add(caseAxis)

	// Slot II: stem and version.
	add(rootAxis("Stem", []g.Stem{g.S1, g.S2, g.S3, g.S0}, g.S1,
		func(r *g.CrRoot, x g.Stem) { r.Stem = x }))
	add(rootAxis("Version", []g.Version{g.PRC, g.CPT}, g.PRC,
		func(r *g.CrRoot, x g.Version) { r.Version = x }))

	// Slot IV: function, specification, context.
	add(rootAxis("Function", []g.Function{g.STA, g.DYN}, g.STA,
		func(r *g.CrRoot, x g.Function) { r.SlotIV.Function = x }))
	add(rootAxis("Specification", []g.Specification{g.BSC, g.CTE, g.CSV, g.OBJ}, g.BSC,
		func(r *g.CrRoot, x g.Specification) { r.SlotIV.Specification = x }))
	add(rootAxis("Context", []g.Context{g.EXS, g.FNC, g.RPS, g.AMG}, g.EXS,
		func(r *g.CrRoot, x g.Context) { r.SlotIV.Context = x }))

	// Slot V and Slot VII hold the same affix and mean different things
	// by it: in Slot V it applies to the stem alone, in Slot VII it has
	// scope over the whole Ca complex. Position relative to Ca is the
	// only thing telling them apart, and an all-default Ca is elided,
	// which is why §3.6.1 geminates it. So this axis is not a value in
	// any published table — it is the structural choice the tables
	// cannot express, and the one a sweep over values alone cannot
	// reach.
	add(Axis{Name: AffixSlotAxis, Values: []Value{
		{Abbrev: "none", Default: true, Apply: func(*g.Formative) {}},
		{Abbrev: "V", Apply: func(f *g.Formative) { f.SlotV = []g.Affix{affix()} }},
		{Abbrev: "VII", Apply: func(f *g.Formative) { f.SlotVII = []g.Affix{affix()} }},
		{Abbrev: "V+VII", Apply: func(f *g.Formative) {
			f.SlotV = []g.Affix{affix()}
			f.SlotVII = []g.Affix{affix()}
		}},
	}})

	// Slot VI: the five Ca components, whose defaults are the unmarked
	// Ca that g.DefaultSlotVI holds.
	d := g.DefaultSlotVI
	add(caAxis("Configuration", g.AllConfigurations, d.Configuration,
		func(s *g.SlotVI, x g.Configuration) { s.Configuration = x }))
	add(caAxis("Affiliation", g.AllAffiliations, d.Affiliation,
		func(s *g.SlotVI, x g.Affiliation) { s.Affiliation = x }))
	add(caAxis("Perspective", g.AllPerspectives, d.Perspective,
		func(s *g.SlotVI, x g.Perspective) { s.Perspective = x }))
	add(caAxis("Extension", g.AllExtensions, d.Extension,
		func(s *g.SlotVI, x g.Extension) { s.Extension = x }))
	add(caAxis("Essence", g.AllEssences, d.Essence,
		func(s *g.SlotVI, x g.Essence) { s.Essence = x }))

	// Slot VIII, V_N half: the five series, one value each. Only the
	// valence series has a default, MNO, which is why the other four
	// always take a written slot.
	valence := Axis{Name: "Valence"}
	for _, x := range g.AllValences {
		valence.Values = append(valence.Values, Value{
			Abbrev:  x.String(),
			Default: x == g.MNO,
			Apply: func(f *g.Formative) {
				if x != g.MNO {
					f.SlotVIII = g.VnCnValence{Valence: x}
				}
			},
		})
	}
	add(valence)
	add(vnAxis("Phase", g.AllPhases, func(x g.Phase) g.SlotVIII { return g.VnCnPhase{Phase: x} }))
	add(vnAxis("Effect", g.AllEffects, func(x g.Effect) g.SlotVIII { return g.VnCnEffect{Effect: x} }))
	add(vnAxis("Level", g.AllLevels, func(x g.Level) g.SlotVIII { return g.VnCnLevel{Level: x} }))
	add(vnAxis("Aspect", g.AllAspects, func(x g.Aspect) g.SlotVIII { return g.VnCnAspect{Aspect: x} }))

	// Slot VIII, C_N half. One consonant encodes both, and which label
	// it takes is a fact about the formative's ending, so mood needs a
	// verbal carrier and case-scope a nominal one.
	mood := Axis{Name: "Mood", Verbal: true}
	for _, x := range g.AllMoods {
		mood.Values = append(mood.Values, Value{
			Abbrev:  x.String(),
			Default: x == g.FAC,
			Apply: func(f *g.Formative) {
				if x != g.FAC {
					f.SlotVIII = g.VnCnValence{MoodScope: x}
				}
			},
		})
	}
	add(mood)
	caseScope := Axis{Name: "CaseScope"}
	for _, x := range g.AllCaseScopes {
		caseScope.Values = append(caseScope.Values, Value{
			Abbrev:  x.String(),
			Default: x == g.CCN,
			Apply: func(f *g.Formative) {
				if x != g.CCN {
					f.SlotVIII = g.VnCnValence{MoodScope: g.CaseScopeToMood(x)}
				}
			},
		})
	}
	add(caseScope)

	// Slot IX on a verbal formative: illocution, and the validations
	// that only the assertive takes. ASR is the default illocution but
	// is written anyway, the ending being what makes a formative verbal
	// at all; OBS is the default validation and is not.
	illocution := Axis{Name: "Illocution", Verbal: true}
	for _, vk := range g.AllVk {
		illocution.Values = append(illocution.Values, Value{
			Abbrev: vk.Tag(),
			Apply:  func(f *g.Formative) { f.Final = g.UnframedVerbal{Vk: vk} },
		})
	}
	add(illocution)
	validation := Axis{Name: "Validation", Verbal: true}
	for _, x := range g.AllValidations {
		validation.Values = append(validation.Values, Value{
			Abbrev:  x.String(),
			Default: x == g.OBS,
			Apply:   func(f *g.Formative) { f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: x}} },
		})
	}
	add(validation)

	return out
}

// rootAxis builds an axis over a field of the CrRoot, which is held as
// an interface and so cannot be edited in place.
func rootAxis[T comparable](name string, values []T, def T, set func(*g.CrRoot, T)) Axis {
	a := Axis{Name: name}
	for _, x := range values {
		a.Values = append(a.Values, Value{
			Abbrev:  toString(x),
			Default: x == def,
			Apply:   func(f *g.Formative) { *f = withRoot(*f, func(r *g.CrRoot) { set(r, x) }) },
		})
	}
	return a
}

// caAxis builds an axis over one of the five Slot VI components.
func caAxis[T comparable](name string, values []T, def T, set func(*g.SlotVI, T)) Axis {
	a := Axis{Name: name}
	for _, x := range values {
		a.Values = append(a.Values, Value{
			Abbrev:  toString(x),
			Default: x == def,
			Apply:   func(f *g.Formative) { set(&f.SlotVI, x) },
		})
	}
	return a
}

// vnAxis builds an axis over one of the four V_N series that have no
// default, so every value takes a written slot.
func vnAxis[T any](name string, values []T, wrap func(T) g.SlotVIII) Axis {
	a := Axis{Name: name}
	for _, x := range values {
		a.Values = append(a.Values, Value{
			Abbrev: toString(x),
			Apply:  func(f *g.Formative) { f.SlotVIII = wrap(x) },
		})
	}
	return a
}

func toString(x any) string { return x.(interface{ String() string }).String() }
