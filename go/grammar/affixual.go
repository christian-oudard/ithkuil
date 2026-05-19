package grammar

// AffixScope determines which slot an affixual adjunct attaches to in
// the adjacent formative.
type AffixScope int

const (
	ScopeVII AffixScope = iota // Slot VII (formative-scoped)
	ScopeV                     // Slot V (stem-scoped)
	ScopeAdj                   // Adjacent word
)

func (s AffixScope) String() string {
	return [...]string{"VII", "V", "Adj"}[s]
}

// SingleAffixAdjunct carries a single Vx+Cs affix as its own word,
// scoping it onto the next formative via Scope. Vs is an optional
// trailing scope vowel that, when present, refines the affix's
// scope ("a"/"u"/"e"/"i").
type SingleAffixAdjunct struct {
	Vx    string
	Cs    string
	Vs    string // empty if absent
	Scope AffixScope
}

// MultipleAffixAdjunct chains several Vx+Cs pairs into one adjunct word
// with the structure [ë] Cs Vx Cz (VxCs)+ [Vz]. The first affix (Cs,Vx)
// is followed by a Cz scope consonant that determines its scope; then
// additional VxCs pairs and an optional Vz final scope vowel.
type MultipleAffixAdjunct struct {
	First   AffixPair   // first Cs Vx (consonant before vowel on the surface)
	Cz      string      // scope consonant: h, 'h, 'hl, 'hr, hw, 'hw
	Affixes []AffixPair // subsequent VxCs pairs
	Vz      string      // optional final scope vowel; empty if absent
	Scope   AffixScope
}

// AffixPair is the vowel+consonant content of one affix slot within a
// multiple-affix adjunct.
type AffixPair struct {
	Vx string
	Cs string
}
