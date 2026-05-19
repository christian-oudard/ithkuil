package parse

import "github.com/coudard/ithkuil/go/grammar"

// IsSpecialVv reports whether v is one of the six special Vv markers
// that select a Cs-root or reference-root formative shape.
func IsSpecialVv(v string) bool {
	switch NormalizeAccents(v) {
	case "ëi", "eë", "ëu", "oë", "ae", "ea":
		return true
	}
	return false
}

// IsRefRootVv reports whether v is one of the two reference-root Vv
// markers (ae/ea). In these forms the "root" position holds a
// referential C1 rather than a regular Cr.
func IsRefRootVv(v string) bool {
	switch NormalizeAccents(v) {
	case "ae", "ea":
		return true
	}
	return false
}

// SpecialVv encodes the meaning of a special Vv marker.
//
//	Function is set for Cs-root markers (ëi/eë/ëu/oë), where it
//	encodes the formative's verbal/stative class.
//	For reference-root markers (ae/ea) Function is nil and the caller
//	uses STA as the default class.
type SpecialVv struct {
	Version  grammar.Version
	Function *grammar.Function
}

// ParseSpecialVv decodes one of the six special Vv markers. Returns
// ok=false for any other input.
func ParseSpecialVv(v string) (SpecialVv, bool) {
	sta := grammar.STA
	dyn := grammar.DYN
	switch NormalizeAccents(v) {
	case "ëi":
		return SpecialVv{Version: grammar.PRC, Function: &sta}, true
	case "eë":
		return SpecialVv{Version: grammar.PRC, Function: &dyn}, true
	case "ëu":
		return SpecialVv{Version: grammar.CPT, Function: &sta}, true
	case "oë":
		return SpecialVv{Version: grammar.CPT, Function: &dyn}, true
	case "ae":
		return SpecialVv{Version: grammar.PRC}, true
	case "ea":
		return SpecialVv{Version: grammar.CPT}, true
	}
	return SpecialVv{}, false
}

// affixVrSpecialDegrees holds the 4 Vr forms that encode degree 0
// (one per context series).
var affixVrSpecialDegrees = map[string]struct {
	degree int
	ctx    grammar.Context
}{
	"ae": {0, grammar.EXS},
	"ea": {0, grammar.FNC},
	"üo": {0, grammar.RPS},
	"üö": {0, grammar.AMG},
}

// ParseAffixVr decodes a Vr vowel in Cs-root context, where it encodes
// (degree, Context) instead of the usual (Function, Specification,
// Context). The vowel's series gives the context (1=EXS, 2=FNC,
// 3=RPS, 4=AMG); the form gives the degree (0-9). Returns false on
// an unrecognized input.
func ParseAffixVr(v string) (degree int, ctx grammar.Context, ok bool) {
	nv := NormalizeAccents(v)
	if e, found := affixVrSpecialDegrees[nv]; found {
		return e.degree, e.ctx, true
	}
	// We can't import phonology here without a cycle, so reuse VvSeries
	// (which lives in this package) plus a small form lookup.
	series := VvSeries(nv)
	form, ok := vowelFormNumber(nv)
	if !ok {
		return 0, 0, false
	}
	switch series {
	case 1:
		ctx = grammar.EXS
	case 2:
		ctx = grammar.FNC
	case 3:
		ctx = grammar.RPS
	case 4:
		ctx = grammar.AMG
	default:
		ctx = grammar.EXS
	}
	return form, ctx, true
}

// vowelFormNumber returns the form (1-9) of a vowel, if it matches
// any series/form cell in the form table. Series 3 alternates resolve
// to their canonical form number.
func vowelFormNumber(v string) (int, bool) {
	// Series 1.
	switch v {
	case "a":
		return 1, true
	case "ä":
		return 2, true
	case "e":
		return 3, true
	case "i":
		return 4, true
	case "ëi":
		return 5, true
	case "ö":
		return 6, true
	case "o":
		return 7, true
	case "ü":
		return 8, true
	case "u":
		return 9, true
	}
	// Series 2.
	switch v {
	case "ai":
		return 1, true
	case "au":
		return 2, true
	case "ei":
		return 3, true
	case "eu":
		return 4, true
	case "ëu":
		return 5, true
	case "ou":
		return 6, true
	case "oi":
		return 7, true
	case "iu":
		return 8, true
	case "ui":
		return 9, true
	}
	// Series 3 canonical.
	switch v {
	case "ia":
		return 1, true
	case "ie":
		return 2, true
	case "io":
		return 3, true
	case "iö":
		return 4, true
	case "eë":
		return 5, true
	case "uö":
		return 6, true
	case "uo":
		return 7, true
	case "ue":
		return 8, true
	case "ua":
		return 9, true
	}
	// Series 3 alternates.
	switch v {
	case "uä":
		return 1, true
	case "uë":
		return 2, true
	case "üä":
		return 3, true
	case "üë":
		return 4, true
	case "öë":
		return 6, true
	case "öä":
		return 7, true
	case "ië":
		return 8, true
	case "iä":
		return 9, true
	}
	// Series 4.
	switch v {
	case "ao":
		return 1, true
	case "aö":
		return 2, true
	case "eo":
		return 3, true
	case "eö":
		return 4, true
	case "oë":
		return 5, true
	case "öe":
		return 6, true
	case "oe":
		return 7, true
	case "öa":
		return 8, true
	case "oa":
		return 9, true
	}
	return 0, false
}
