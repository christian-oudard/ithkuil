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

// carrierForms maps each CarrierType to its h-consonant cluster.
var carrierForms = [...]string{
	Carrier:   "hl",
	Quotative: "hm",
	Naming:    "hn",
	Phrasal:   "hň",
}

// CarrierTypeForm returns the surface consonant cluster for a CarrierType.
func CarrierTypeForm(c CarrierType) string { return carrierForms[c] }

// AllCarrierTypes enumerates the four types in declaration order.
var AllCarrierTypes = []CarrierType{Carrier, Quotative, Naming, Phrasal}

// CarrierAdjunct is a carrier word marked by a CarrierType consonant
// cluster and a Vc case vowel that scopes the embedded content.
type CarrierAdjunct struct {
	Type CarrierType
	Vc   string // Case vowel
}
