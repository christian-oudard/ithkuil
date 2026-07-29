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
	return [...]string{"Carrier", "Quotative", "Naming", "Phrasal"}[c]
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
