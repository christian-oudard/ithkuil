package view

import "github.com/christian-oudard/ithkuil/tokenize"

// Type returns a short tag identifying the token kind: Form, Concat,
// Ref, CombRef, Bias, Reg, Mod, Affix, Affixes, Carrier, (fgn), or "?"
// for unknown. Used by the CLI's --short view and the MCP per-token
// type field.
func Type(t tokenize.WordToken) string {
	switch t.(type) {
	case tokenize.FormativeWord:
		return "Form"
	case tokenize.ConcatenatedFormativeWord:
		return "Concat"
	case tokenize.ReferentialWord:
		return "Ref"
	case tokenize.CombinationRefWord:
		return "CombRef"
	case tokenize.BiasWord:
		return "Bias"
	case tokenize.RegisterStartWord, tokenize.RegisterEndWord:
		return "Reg"
	case tokenize.ModularWord:
		return "Mod"
	case tokenize.SingleAffixWord:
		return "Affix"
	case tokenize.MultipleAffixWord:
		return "Affixes"
	case tokenize.CarrierWord:
		return "Carrier"
	case tokenize.ForeignWord:
		return "(fgn)"
	case tokenize.UnknownWord:
		return "?"
	}
	return "?"
}
