// Package inventory pairs every grammatical value in the language with
// a word that carries it.
//
// The two peripheral arms, romanization and gloss, are each supposed to
// handle the whole grammar, and neither can be checked against a corpus
// for that: attested text uses what people happen to say, so a category
// nobody writes down is invisible to a corpus test no matter how large
// the corpus is. What is needed instead is the inventory itself, driven
// exhaustively.
//
// Each Sample differs from the same baseline in exactly one value, so a
// round trip that loses it says which one was lost. The categories, and
// the values in them, come from the same AllX slices that search.Table
// is built from, and search's own tests hold those against the store,
// so "every value" is a checked claim rather than a list someone kept
// up to date by hand.
package inventory

import (
	g "github.com/christian-oudard/ithkuil/grammar"
)

// Sample is one grammatical value together with a word carrying it.
// Category and Abbrev name the value the way search.Table does, so a
// failure reads as a coordinate in the published tables rather than as
// a Go struct.
//
// Unwritten marks a value that is real grammar but has no romanization:
// NRR is the unmarked register, so a stretch of narrative is in it by
// saying nothing. Such a value still glosses, so it is a sample rather
// than an omission, and stating it here keeps the romanization sweep
// from having to carry a hardcoded exception.
type Sample struct {
	Category  string
	Abbrev    string
	Word      g.Word
	Unwritten bool
}

// Cr is the root every formative sample is built on. Any attested root
// would do; this one is short, so a failing romanization is mostly the
// slot under test.
const Cr = "ml"

// nominal is the baseline: the shortest well-formed formative, every
// category at its default. Samples take a copy and change one thing.
func nominal() g.Formative { return g.MinimalFormative(Cr) }

// verbal is the baseline for the categories that only exist on a verbal
// formative: illocution, validation, and Mood, which is the same Slot
// VIII field that a nominal formative labels as case-scope.
func verbal() g.Formative {
	f := nominal()
	f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
	return f
}

// withRoot rebuilds the formative around a modified root, since Root is
// held as an interface and cannot be edited in place.
func withRoot(f g.Formative, edit func(*g.CrRoot)) g.Formative {
	r := f.Root.(g.CrRoot)
	edit(&r)
	f.Root = r
	return f
}

// Samples returns one sample per grammatical value, in the order the
// categories appear in the grammar.
func Samples() []Sample {
	var out []Sample
	add := func(category, abbrev string, w g.Word) {
		out = append(out, Sample{Category: category, Abbrev: abbrev, Word: w})
	}

	// Case (§4.4). The nine groups are one category to the grammar and
	// nine to the tables; keep the table's names so a failure points at
	// the page it is on.
	for _, c := range g.AllCases {
		f := nominal()
		f.Final = g.UnframedNominal{Case: c}
		add("Case/"+c.Group().String(), c.String(), f)
	}

	// Slot II: stem and version.
	for _, s := range []g.Stem{g.S1, g.S2, g.S3, g.S0} {
		add("Stem", s.String(), withRoot(nominal(), func(r *g.CrRoot) { r.Stem = s }))
	}
	for _, v := range []g.Version{g.PRC, g.CPT} {
		add("Version", v.String(), withRoot(nominal(), func(r *g.CrRoot) { r.Version = v }))
	}

	// Slot IV: function, specification, context.
	for _, x := range []g.Function{g.STA, g.DYN} {
		add("Function", x.String(), withRoot(nominal(), func(r *g.CrRoot) { r.SlotIV.Function = x }))
	}
	for _, x := range []g.Specification{g.BSC, g.CTE, g.CSV, g.OBJ} {
		add("Specification", x.String(), withRoot(nominal(), func(r *g.CrRoot) { r.SlotIV.Specification = x }))
	}
	for _, x := range []g.Context{g.EXS, g.FNC, g.RPS, g.AMG} {
		add("Context", x.String(), withRoot(nominal(), func(r *g.CrRoot) { r.SlotIV.Context = x }))
	}

	// Slot VI: the five Ca components.
	for _, x := range g.AllConfigurations {
		f := nominal()
		f.SlotVI.Configuration = x
		add("Configuration", x.String(), f)
	}
	for _, x := range g.AllAffiliations {
		f := nominal()
		f.SlotVI.Affiliation = x
		add("Affiliation", x.String(), f)
	}
	for _, x := range g.AllPerspectives {
		f := nominal()
		f.SlotVI.Perspective = x
		add("Perspective", x.String(), f)
	}
	for _, x := range g.AllExtensions {
		f := nominal()
		f.SlotVI.Extension = x
		add("Extension", x.String(), f)
	}
	for _, x := range g.AllEssences {
		f := nominal()
		f.SlotVI.Essence = x
		add("Essence", x.String(), f)
	}

	// Slot VIII, V_N half: the five series, one sample per value.
	for _, x := range g.AllValences {
		f := nominal()
		f.SlotVIII = g.VnCnValence{Valence: x}
		add("Valence", x.String(), f)
	}
	for _, x := range g.AllPhases {
		f := nominal()
		f.SlotVIII = g.VnCnPhase{Phase: x}
		add("Phase", x.String(), f)
	}
	for _, x := range g.AllEffects {
		f := nominal()
		f.SlotVIII = g.VnCnEffect{Effect: x}
		add("Effect", x.String(), f)
	}
	for _, x := range g.AllLevels {
		f := nominal()
		f.SlotVIII = g.VnCnLevel{Level: x}
		add("Level", x.String(), f)
	}
	for _, x := range g.AllAspects {
		f := nominal()
		f.SlotVIII = g.VnCnAspect{Aspect: x}
		add("Aspect", x.String(), f)
	}

	// Slot VIII, C_N half. One consonant encodes both, and which label
	// it takes is a fact about the formative's ending, so mood needs a
	// verbal carrier and case-scope a nominal one.
	for _, x := range g.AllMoods {
		f := verbal()
		f.SlotVIII = g.VnCnValence{MoodScope: x}
		add("Mood", x.String(), f)
	}
	for _, x := range g.AllCaseScopes {
		f := nominal()
		f.SlotVIII = g.VnCnValence{MoodScope: g.CaseScopeToMood(x)}
		add("CaseScope", x.String(), f)
	}

	// Slot IX on a verbal formative: illocution, and the validations
	// that only the assertive takes.
	for _, vk := range g.AllVk {
		f := verbal()
		f.Final = g.UnframedVerbal{Vk: vk}
		add("Illocution", vk.Tag(), f)
	}
	for _, x := range g.AllValidations {
		f := verbal()
		f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: x}}
		add("Validation", x.String(), f)
	}

	// Word classes that are their own carrier.
	for _, b := range g.AllBiases {
		add("Bias", b.String(), b)
	}
	// A register is opened by an adjunct and closed by another, and two
	// of the seven exist at one end only: END is the closer shared by
	// every register and has no opening form, and NRR is the unmarked
	// default with neither.
	for _, r := range g.AllRegisters {
		s := Sample{Category: "Register", Abbrev: r.String()}
		switch r {
		case g.NRR:
			s.Word, s.Unwritten = g.RegisterMarker{Register: r}, true
		case g.END:
			s.Word = g.RegisterMarker{Register: r, End: true}
		default:
			s.Word = g.RegisterMarker{Register: r}
		}
		out = append(out, s)
	}
	for _, ct := range g.AllCarrierTypes {
		add("CarrierType", ct.Abbrev(), g.CarrierAdjunct{Type: ct, Case: g.THM})
	}
	return out
}
