package grammar

// AffixScope is the scope of an affix introduced by an affixual
// adjunct over the adjacent formative (§4.1). Six values, derived from
// the Vs vowel on a single-affix adjunct, the Cz consonant on a multi-
// affix adjunct's first affix, or the Vz vowel on the trailing affixes.
type AffixScope int

const (
	ScopeVDom      AffixScope = iota // Slot V, last (default)
	ScopeVSub                        // Slot V, first
	ScopeVIIDom                      // Slot VII, last
	ScopeVIISub                      // Slot VII, first
	ScopeFormative                   // whole formative
	ScopeAdjacent                    // formative + adjacent adjuncts
)

func (s AffixScope) String() string {
	return [...]string{"VDom", "VSub", "VIIDom", "VIISub", "formative", "adjacent"}[s]
}

// AllAffixScopes enumerates every AffixScope in declaration order.
var AllAffixScopes = []AffixScope{
	ScopeVDom, ScopeVSub, ScopeVIIDom, ScopeVIISub, ScopeFormative, ScopeAdjacent,
}

// VsScope maps a single-affix adjunct's Vs vowel to an AffixScope.
// Empty Vs (omitted) and explicit "a" both default to ScopeVDom.
func VsScope(vs string) (AffixScope, bool) {
	switch vs {
	case "", "a":
		return ScopeVDom, true
	case "u":
		return ScopeVSub, true
	case "e":
		return ScopeVIIDom, true
	case "i":
		return ScopeVIISub, true
	case "o":
		return ScopeFormative, true
	case "ö":
		return ScopeAdjacent, true
	}
	return 0, false
}

// CzScope maps a multi-affix adjunct's Cz consonant (the boundary
// after the first affix) to the scope of that first affix.
func CzScope(cz string) (AffixScope, bool) {
	switch cz {
	case "h":
		return ScopeVDom, true
	case "'h":
		return ScopeVSub, true
	case "'hl":
		return ScopeVIIDom, true
	case "'hr":
		return ScopeVIISub, true
	case "hw":
		return ScopeFormative, true
	case "'hw":
		return ScopeAdjacent, true
	}
	return 0, false
}

// VzScope maps a multi-affix adjunct's trailing Vz vowel to the scope
// of its 2nd-and-later affixes. ok=false when the Vz value signals
// "same as Cz" (an explicit "ai" or omitted Vz), so the caller should
// propagate the first-affix scope.
func VzScope(vz string) (AffixScope, bool) {
	switch vz {
	case "a":
		return ScopeVDom, true
	case "u":
		return ScopeVSub, true
	case "e":
		return ScopeVIIDom, true
	case "i":
		return ScopeVIISub, true
	case "o":
		return ScopeFormative, true
	case "ö":
		return ScopeAdjacent, true
	}
	return 0, false
}

// SingleAffixAdjunct carries one affix as its own word, scoping it
// onto the next formative via Scope. The affix's surface Vx vowel is
// decoded into (Type, Degree) at parse time; surface fields don't
// appear here.
type SingleAffixAdjunct struct {
	Affix Affix
	Scope AffixScope
}

// MultipleAffixAdjunct chains two or more affixes into one adjunct
// word. The first affix takes FirstScope; the rest take RestScope.
// Both scopes are decoded from the surface Cz/Vz vowels at parse time.
type MultipleAffixAdjunct struct {
	First      Affix
	Rest       []Affix
	FirstScope AffixScope
	RestScope  AffixScope
}
