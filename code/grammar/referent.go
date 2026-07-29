package grammar

// This file holds the §4.6 personal-reference vocabulary: the values a
// referential is built from, with no knowledge of the romanization. The consonant
// forms that spell them live in parse, alongside the other lookup
// tables.

// Referent is one of the eleven referent categories of the §4.6 table.
type Referent int

const (
	R1m  Referent = iota // monadic speaker ("I")
	R2m                  // monadic addressee ("you sg.")
	R2p                  // polyadic addressee ("you pl.")
	Rma                  // monadic animate 3rd party ("he/she")
	Rpa                  // polyadic animate 3rd party ("they")
	Rmi                  // monadic inanimate 3rd party ("it")
	Rpi                  // polyadic inanimate 3rd party ("those things")
	Rmx                  // mixed animate/inanimate
	Rrdp                 // reduplicative (resumptive reference)
	Robv                 // obviative (other 3rd party)
	Rpvs                 // provisional ("whatever")
)

var referentAbbrevs = [...]string{
	"1m", "2m", "2p",
	"ma", "pa",
	"mi", "pi",
	"Mx",
	"Rdp", "Obv", "PVS",
}

var referentLabels = [...]string{
	"I", "you(sg.)", "you(pl.)",
	"he/she", "they(anim.)",
	"it", "them(inanim.)",
	"it+they(mixed)",
	"aforementioned", "the other one", "whatever",
}

func (r Referent) String() string { return referentAbbrevs[r] }

// Label returns a longer English gloss for the referent.
func (r Referent) Label() string { return referentLabels[r] }

// AllReferents enumerates the eleven referent categories in declaration
// order.
var AllReferents = []Referent{
	R1m, R2m, R2p, Rma, Rpa, Rmi, Rpi, Rmx, Rrdp, Robv, Rpvs,
}

// RefEffect is the referent's effect on the speaker or event. §4.6
// gives referents three of these, a coarser set than the nine §3.8
// Effect values a formative's Slot VIII carries, so the two stay
// distinct types.
type RefEffect int

const (
	NEU RefEffect = iota // Neutral
	BEN                  // Beneficial
	DET                  // Detrimental
)

func (e RefEffect) String() string {
	return [...]string{"NEU", "BEN", "DET"}[e]
}

// AllRefEffects enumerates the three referent effects.
var AllRefEffects = []RefEffect{NEU, BEN, DET}

// PersonalRef is a (Referent, RefEffect) pair. One referential consonant
// cluster encodes a chain of one or more of these.
type PersonalRef struct {
	Referent Referent
	Effect   RefEffect
}

// RefCategory is the optional §4.6 modifier on a referent chain:
//
//   - Agglomerative ("each/every X")
//   - Nomic ("someone/something")
//   - Abstract ("everything about X")
//
// §4.6.4 bars all three from a specialized personal-reference root,
// where the same distinctions are carried by the Slot VI Perspective
// instead, so RefRoot has no field for one.
type RefCategory int

const (
	Agglomerative RefCategory = iota
	Nomic
	Abstract
)

func (c RefCategory) String() string {
	return [...]string{"AGM", "NOM", "ABS"}[c]
}

// AllRefCategories enumerates the three categories in declaration order.
var AllRefCategories = []RefCategory{Agglomerative, Nomic, Abstract}
