package grammar

// AffixType is the gradient class of an affix: Type 1, 2, or 3.
// The class is grammatical; the surface vowel that encodes it is a
// rendering concern handled in the parse/render packages.
type AffixType int

const (
	Type1Affix AffixType = iota
	Type2Affix
	Type3Affix
)

func (a AffixType) String() string {
	return [...]string{"Type1Affix", "Type2Affix", "Type3Affix"}[a]
}

// Affix is a single grammar-level affix. Vx degree (0-9) and Type are
// the grammatical content; Cs is the affix identifier. The surface
// vowel (Vx) is derived from (Type, Degree) at render time and does
// not appear here — keeping phonetic and grammatical data apart.
type Affix struct {
	Type      AffixType
	Degree    int
	Consonant string
}
