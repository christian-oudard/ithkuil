package view

import g "github.com/christian-oudard/ithkuil/grammar"

// Type returns a short tag naming the word class: Form, Concat, Ref,
// CombRef, Bias, Reg, Mod, Affix, Affixes, Carrier or (fgn). Used by
// the MCP per-word type field.
//
// A word that could not be read has no type to give — it never becomes
// a Word at all, and the reason comes back from the parser instead.
func Type(t g.Word) string {
	switch t.(type) {
	case g.Formative:
		return "Form"
	case *g.Chain:
		return "Concat"
	case g.Referential:
		return "Ref"
	case g.CombinationReferential:
		return "CombRef"
	case g.Bias:
		return "Bias"
	case g.RegisterMarker:
		return "Reg"
	case g.ModularAdjunct:
		return "Mod"
	case g.SingleAffixAdjunct:
		return "Affix"
	case g.MultipleAffixAdjunct:
		return "Affixes"
	case g.CarrierAdjunct:
		return "Carrier"
	case g.Foreign:
		return "(fgn)"
	}
	return "?"
}
