package grammar

// AffixType is the gradient class of an affix: Type 1, 2, or 3, plus
// CaStackAffix, which is not a gradient class at all but occupies the
// same position. The class is grammatical; the surface vowel that
// encodes it is a rendering concern handled in the parse/render
// packages.
//
// CaStackAffix is §3.5/§3.7's specialized Vx value -üö-, marking the
// following Cs as a Ca complex stacked on the Slot VI Ca. It belongs
// here rather than in a separate type because the surface treats it as
// one more value of the Vx that otherwise carries Type and Degree —
// Chapter 12 lists it in the affix degree row, between degree 9 and
// degree 0. A CaStackAffix carries no Degree.
type AffixType int

const (
	Type1Affix AffixType = iota
	Type2Affix
	Type3Affix
	CaStackAffix
)

func (a AffixType) String() string {
	return [...]string{"Type1Affix", "Type2Affix", "Type3Affix", "CaStackAffix"}[a]
}

// Affix is a single grammar-level affix. Vx degree (0-9) and Type are
// the grammatical content; Cs is the affix identifier. The surface
// vowel (Vx) is derived from (Type, Degree) at render time and does
// not appear here — keeping phonetic and grammatical data apart.
//
// For a CaStackAffix, Consonant holds the stacked Ca cluster rather
// than an affix identifier, and Degree is unused. Use StackedCa to
// read it back as grammatical values.
type Affix struct {
	Type      AffixType
	Degree    int
	Consonant string
}

// IsCaStack reports whether a is the §3.5/§3.7 Ca-stacking affix,
// whose Consonant is a Ca complex rather than an affix Cs.
func (a Affix) IsCaStack() bool { return a.Type == CaStackAffix }
