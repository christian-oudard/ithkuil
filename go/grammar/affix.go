package grammar

// AffixType is the gradient class of an affix: Type 1, 2, or 3.
// The class is encoded in the affix vowel (Vx): Series 1 = Type 1,
// Series 2 = Type 2, Series 3 = Type 3.
type AffixType int

const (
	Type1Affix AffixType = iota
	Type2Affix
	Type3Affix
)

func (a AffixType) String() string {
	return [...]string{"Type1Affix", "Type2Affix", "Type3Affix"}[a]
}

// Affix is a single Vx+Cs (or Cs+Vx in Slot V) affix.
// Vowel encodes the degree (1-9, with 0 reserved for "ae"/"ea"/"üo").
// Consonant is the affix identifier; the vowel's series determines Type.
type Affix struct {
	Vowel     string
	Consonant string
	Type      AffixType
}
