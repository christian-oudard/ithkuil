package grammar

// CarrierType is one of the four suppletive adjunct categories used
// for embedding foreign words, quotations, names, or phrasal markers.
type CarrierType int

const (
	Carrier   CarrierType = iota // General carrier
	Quotative                    // Carrier + discursive register
	Naming                       // A name referred to as a name
	Phrasal                      // Meta-level grammatical info for phrase
)

func (c CarrierType) String() string {
	return enumName(c, "CarrierType",
		"Carrier", "Quotative", "Naming", "Phrasal")
}

// Abbrev returns the three-letter abbreviation, which is what §4.5's
// own section headings use: "4.5.1 CAR Carrier Adjunct", "4.5.2 QUO
// Quotative Adjunct", and so on. Every other category in the language
// is named by its abbreviation and spelled out by String; this one has
// String spelling it out, so the abbreviation needs a method of its
// own rather than the two being the same string.
func (c CarrierType) Abbrev() string {
	return enumName(c, "CarrierType", "CAR", "QUO", "NAM", "PHR")
}

// AllCarrierTypes enumerates the four types in declaration order.
var AllCarrierTypes = []CarrierType{Carrier, Quotative, Naming, Phrasal}

// CarrierAdjunct is a carrier word marked by a CarrierType consonant
// cluster and a Case that scopes the embedded content. The romanization
// Vc vowel is derivable via CaseToVc(adjunct.Case).
type CarrierAdjunct struct {
	Type CarrierType
	Case Case
}
