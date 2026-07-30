package grammar

// AffixType is the gradient class of an affix: Type 1, 2, or 3, plus
// CaStackAffix, which is not a gradient class at all but occupies the
// same position. The class is grammatical; the written vowel that
// encodes it is a rendering concern handled in the parse/render
// packages.
//
// CaStackAffix is §3.5/§3.7's specialized Vx value -üö-, marking the
// following Cs as a Ca complex stacked on the Slot VI Ca. It belongs
// here rather than in a separate type because the romanization treats it as
// one more value of the Vx that otherwise carries Type and Degree —
// the script document lists it in the affix degree row, between degree
// 9 and degree 0. A CaStackAffix carries no Degree.
type AffixType int

// Column4Affix is §4.6.5's shortcut for the Transrelative cases: a
// Column-4 vowel from the Standard Vowel Sequence on a referential Cs.
// Unlike the Type-3 referential shortcut, which is triggered by being
// alone in its slot, this one may be used "regardless of other V_X C_S
// affixes being present in the same Slot". Its Degree is the 1-9
// column-4 form number; TransrelativeCase turns that into the Case.
const (
	Type1Affix AffixType = iota
	Type2Affix
	Type3Affix
	CaStackAffix
	Column4Affix
)

func (a AffixType) String() string {
	return [...]string{
		"Type1Affix", "Type2Affix", "Type3Affix", "CaStackAffix", "Column4Affix",
	}[a]
}

// TransrelativeCase maps a Column-4 form number (1-9) to the case it
// marks. The nine Transrelative cases are the first nine of AllCases,
// in the order §4.6.5 lists them, so the mapping is positional.
func TransrelativeCase(degree int) (Case, bool) {
	if degree < 1 || degree > 9 {
		return THM, false
	}
	return Case(degree - 1), true
}

// TransrelativeDegree is the inverse of TransrelativeCase: the
// Column-4 form number that marks c, or false if c is not one of the
// nine Transrelative cases.
func TransrelativeDegree(c Case) (int, bool) {
	if c < THM || c > IND {
		return 0, false
	}
	return int(c) + 1, true
}

// AccessorKind is one of §3.9.2's seven case-bearing affixes. Each has
// two Cs increments: one for cases 1-36 and one for cases 37-68. The
// Vx then carries the case, its series picking the group of nine (or
// eight, above case 36) and its degree the case within that group.
//
// The seven are two families of three plus one: §3.9.2 names them
// "Case-Accessor, Type-1/-2/-3", "Inverse Case-Accessor, Type-1/-2/-3"
// and "Case-Stacking Affix". The Type there is the same Type an
// ordinary VxCs affix carries — §3.9.2's closing note groups a "Type-3
// case-accessor" with a "standard Type-3 VxCs affix" under one rule —
// so Family and Type are the two axes, not seven opaque values.
type AccessorKind int

const (
	CaseAccessor1 AccessorKind = iota
	CaseAccessor2
	CaseAccessor3
	InverseAccessor1
	InverseAccessor2
	InverseAccessor3
	CaseStacking
)

// Family is the gloss head for the kind: ACC for a case-accessor, IAC
// for an inverse case-accessor, CST for case-stacking.
func (k AccessorKind) Family() string {
	return [...]string{"ACC", "ACC", "ACC", "IAC", "IAC", "IAC", "CST"}[k]
}

// Type is the §3.9.2 Type of the kind. Case-stacking has no Type
// distinction and reports Type1Affix.
func (k AccessorKind) Type() AffixType {
	return [...]AffixType{
		Type1Affix, Type2Affix, Type3Affix,
		Type1Affix, Type2Affix, Type3Affix,
		Type1Affix,
	}[k]
}

// String is the kind's canonical name, family plus the "_2"/"_3" Type
// suffix the gloss uses. Type 1 is unmarked, as it is on any affix.
func (k AccessorKind) String() string {
	switch k.Type() {
	case Type2Affix:
		return k.Family() + "_2"
	case Type3Affix:
		return k.Family() + "_3"
	}
	return k.Family()
}

// LookupAccessorKind is the inverse of Family and Type.
func LookupAccessorKind(family string, t AffixType) (AccessorKind, bool) {
	for _, k := range AllAccessorKinds {
		if k.Family() == family && k.Type() == t {
			return k, true
		}
	}
	return 0, false
}

// AllAccessorKinds enumerates the seven in declaration order.
var AllAccessorKinds = []AccessorKind{
	CaseAccessor1, CaseAccessor2, CaseAccessor3,
	InverseAccessor1, InverseAccessor2, InverseAccessor3,
	CaseStacking,
}

// accessorCs holds each kind's two Cs increments, low (cases 1-36)
// then high (cases 37-68). Transcribed from Quijada's §3.9.2 table;
// see ISSUES.md G34 for why the markdown copy could not be trusted.
var accessorCs = [...][2]string{
	CaseAccessor1:    {"sw", "sy"},
	CaseAccessor2:    {"zw", "zy"},
	CaseAccessor3:    {"čw", "čy"},
	InverseAccessor1: {"šw", "šy"},
	InverseAccessor2: {"žw", "žy"},
	InverseAccessor3: {"jw", "jy"},
	CaseStacking:     {"lw", "ly"},
}

// AccessorCs returns the Cs increment for a kind and case range.
// high selects the cases-37-68 increment.
func AccessorCs(k AccessorKind, high bool) string {
	if high {
		return accessorCs[k][1]
	}
	return accessorCs[k][0]
}

// ParseAccessorCs decodes a Cs increment into its kind and range, and
// reports whether the cluster is one of the fourteen at all.
func ParseAccessorCs(cs string) (k AccessorKind, high, ok bool) {
	for _, kind := range AllAccessorKinds {
		if accessorCs[kind][0] == cs {
			return kind, false, true
		}
		if accessorCs[kind][1] == cs {
			return kind, true, true
		}
	}
	return 0, false, false
}

// Affix is a single grammar-level affix. Vx degree (0-9) and Type are
// the grammatical content; Cs is the affix identifier. The romanization
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

// VxSeries returns the Standard Vowel Sequence series (1-4) that an
// affix Type encodes with, and whether the Type has one. §3.9.2 reads
// the series as a case-group selector rather than a gradient class, so
// the accessor affixes reuse the same four Vx tables.
func VxSeries(t AffixType) (int, bool) {
	switch t {
	case Type1Affix:
		return 1, true
	case Type2Affix:
		return 2, true
	case Type3Affix:
		return 3, true
	case Column4Affix:
		return 4, true
	}
	return 0, false
}

// SeriesAffixType is the inverse of VxSeries.
func SeriesAffixType(series int) (AffixType, bool) {
	switch series {
	case 1:
		return Type1Affix, true
	case 2:
		return Type2Affix, true
	case 3:
		return Type3Affix, true
	case 4:
		return Column4Affix, true
	}
	return Type1Affix, false
}

// AccessorCase decodes a §3.9.2 affix's Vx into the case it names.
// Below case 37 each series covers nine cases; at and above it each
// covers eight, because those four groups have no ü-tier — so degree
// 8 is unused there and degree 9 fills the eighth slot.
func AccessorCase(series, degree int, high bool) (Case, bool) {
	if series < 1 || series > 4 || degree < 1 || degree > 9 {
		return THM, false
	}
	if !high {
		return Case((series-1)*9 + degree - 1), true
	}
	offset := degree - 1
	switch {
	case degree == 8:
		return THM, false
	case degree == 9:
		offset = 7
	}
	n := 37 + (series-1)*8 + offset
	if n > 68 {
		return THM, false
	}
	return Case(n - 1), true
}

// AccessorVx is the inverse of AccessorCase: the Vx series and degree
// that encode c, and whether c needs the cases-37-68 Cs increment.
func AccessorVx(c Case) (series, degree int, high, ok bool) {
	n := int(c) + 1
	switch {
	case n < 1 || n > 68:
		return 0, 0, false, false
	case n <= 36:
		series = (n-1)/9 + 1
		degree = (n-1)%9 + 1
		return series, degree, false, true
	}
	off := n - 37
	series = off/8 + 1
	degree = off%8 + 1
	if degree == 8 {
		degree = 9 // the ü-tier is skipped in these four groups
	}
	return series, degree, true, true
}
