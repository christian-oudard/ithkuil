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
// Two kinds of value are real grammar expressed by saying nothing, and
// a sweep that did not know which would demand a mark that should not
// be there.
//
// Unwritten: the value has no romanization at all. NRR is the unmarked
// register, so a stretch of narrative is in it by never being marked
// out of it. It still glosses, as NRR, so it is a sample and not an
// omission.
//
// Unmarked: the value is its category's default, so the gloss shows
// nothing for it. Its carrier is the baseline word untouched, which is
// how the language says it: a formative at THM, S1, PRC and the rest is
// written and glossed as though none of them had been chosen.
type Sample struct {
	Category  string
	Abbrev    string
	Word      g.Word
	Unwritten bool
	Unmarked  bool
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
	// addIf records a value that may be its category's default. A
	// default's carrier is the baseline untouched, never a formative
	// with the default written into it: the two are the same grammar,
	// and the baseline is the only one either arm produces.
	addIf := func(isDefault bool, category, abbrev string, w g.Word) {
		out = append(out, Sample{
			Category: category, Abbrev: abbrev, Word: w, Unmarked: isDefault,
		})
	}

	// Case (§4.4). The nine groups are one category to the grammar and
	// nine to the tables; keep the table's names so a failure points at
	// the page it is on.
	for _, c := range g.AllCases {
		f := nominal()
		f.Final = g.UnframedNominal{Case: c}
		addIf(c == g.THM, "Case/"+c.Group().String(), c.String(), f)
	}

	// Slot II: stem and version.
	for _, x := range []g.Stem{g.S1, g.S2, g.S3, g.S0} {
		f := withRoot(nominal(), func(r *g.CrRoot) { r.Stem = x })
		addIf(x == g.S1, "Stem", x.String(), f)
	}
	for _, x := range []g.Version{g.PRC, g.CPT} {
		f := withRoot(nominal(), func(r *g.CrRoot) { r.Version = x })
		addIf(x == g.PRC, "Version", x.String(), f)
	}

	// Slot IV: function, specification, context.
	for _, x := range []g.Function{g.STA, g.DYN} {
		f := withRoot(nominal(), func(r *g.CrRoot) { r.SlotIV.Function = x })
		addIf(x == g.STA, "Function", x.String(), f)
	}
	for _, x := range []g.Specification{g.BSC, g.CTE, g.CSV, g.OBJ} {
		f := withRoot(nominal(), func(r *g.CrRoot) { r.SlotIV.Specification = x })
		addIf(x == g.BSC, "Specification", x.String(), f)
	}
	for _, x := range []g.Context{g.EXS, g.FNC, g.RPS, g.AMG} {
		f := withRoot(nominal(), func(r *g.CrRoot) { r.SlotIV.Context = x })
		addIf(x == g.EXS, "Context", x.String(), f)
	}

	// Slot VI: the five Ca components, whose defaults are the unmarked
	// Ca that g.DefaultSlotVI holds.
	d := g.DefaultSlotVI
	for _, x := range g.AllConfigurations {
		f := nominal()
		f.SlotVI.Configuration = x
		addIf(x == d.Configuration, "Configuration", x.String(), f)
	}
	for _, x := range g.AllAffiliations {
		f := nominal()
		f.SlotVI.Affiliation = x
		addIf(x == d.Affiliation, "Affiliation", x.String(), f)
	}
	for _, x := range g.AllPerspectives {
		f := nominal()
		f.SlotVI.Perspective = x
		addIf(x == d.Perspective, "Perspective", x.String(), f)
	}
	for _, x := range g.AllExtensions {
		f := nominal()
		f.SlotVI.Extension = x
		addIf(x == d.Extension, "Extension", x.String(), f)
	}
	for _, x := range g.AllEssences {
		f := nominal()
		f.SlotVI.Essence = x
		addIf(x == d.Essence, "Essence", x.String(), f)
	}

	// Slot VIII, V_N half: the five series, one sample per value. Only
	// the valence series has a default, MNO, which is why the other four
	// always take a written slot.
	for _, x := range g.AllValences {
		f := nominal()
		if x != g.MNO {
			f.SlotVIII = g.VnCnValence{Valence: x}
		}
		addIf(x == g.MNO, "Valence", x.String(), f)
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
		if x != g.FAC {
			f.SlotVIII = g.VnCnValence{MoodScope: x}
		}
		addIf(x == g.FAC, "Mood", x.String(), f)
	}
	for _, x := range g.AllCaseScopes {
		f := nominal()
		if x != g.CCN {
			f.SlotVIII = g.VnCnValence{MoodScope: g.CaseScopeToMood(x)}
		}
		addIf(x == g.CCN, "CaseScope", x.String(), f)
	}

	// Slot IX on a verbal formative: illocution, and the validations
	// that only the assertive takes. ASR is the default illocution but
	// is written anyway, the ending being what makes a formative verbal
	// at all; OBS is the default validation and is not.
	for _, vk := range g.AllVk {
		f := verbal()
		f.Final = g.UnframedVerbal{Vk: vk}
		add("Illocution", vk.Tag(), f)
	}
	for _, x := range g.AllValidations {
		f := verbal()
		f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: x}}
		addIf(x == g.OBS, "Validation", x.String(), f)
	}

	// Word classes that are their own carrier.
	for _, b := range g.AllBiases {
		add("Bias", b.String(), b)
	}
	// A register is opened by an adjunct and closed by another, and two
	// of the seven exist at one end only: END is the closer shared by
	// every register and has no opening form, and NRR is the unmarked
	// default with neither. NRR is still named in a gloss, so it is
	// Unwritten without being Unmarked.
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

	// §4.6 referents and their Effects. A referential's whole content
	// is its referent, and until now no sample carried one, so the
	// sweeps could say "every grammatical value" about a set that left
	// the referentials out entirely. NEU is the unmarked Effect, so it
	// rides the same baseline the other two vary from.
	ref := func(r g.Referent, e g.RefEffect) g.Referential {
		return g.Referential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{{Referent: r, Effect: e}}},
			Case: g.THM,
		}
	}
	for _, r := range g.AllReferents {
		add("Referent", r.String(), ref(r, g.NEU))
	}
	for _, e := range g.AllRefEffects {
		addIf(e == g.NEU, "RefEffect", e.String(), ref(g.R1m, e))
	}
	return out
}
