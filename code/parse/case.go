package parse

import "github.com/christian-oudard/ithkuil/grammar"

// caseFromVc maps every recognized Vc romanization to its Case. Canonical
// vowels come from grammar.CaseToVc; this table additionally registers
// series-3 alternate forms (used after y- / w- glides) so the parser
// accepts both spellings.
var caseFromVc = func() map[string]grammar.Case {
	m := make(map[string]grammar.Case, len(grammar.AllCases)+16)
	for _, c := range grammar.AllCases {
		m[grammar.CaseToVc(c)] = c
	}
	// Series 3 alternates (Associative)
	m["uä"] = grammar.APL
	m["uë"] = grammar.PUR
	m["üä"] = grammar.TRA
	m["üë"] = grammar.DFR
	m["öë"] = grammar.TSP
	m["öä"] = grammar.CMM
	m["ië"] = grammar.CMP
	m["iä"] = grammar.CSD
	// Series 3 alternates + glottal stop (Spatio-Temporal I). Cases 53-60
	// are §3.9.1's series-3 forms minus vowel-tier 8, so the tier-8
	// alternate ië has no case here and NAV, being form 9 (ua / iä),
	// takes i'ä.
	m["u'ä"] = grammar.LOC
	m["u'ë"] = grammar.ATD
	m["ü'ä"] = grammar.ALL
	m["ü'ë"] = grammar.ABL
	m["ö'ë"] = grammar.IRL
	m["ö'ä"] = grammar.INV
	m["i'ä"] = grammar.NAV
	return m
}()

// ParseCase decodes a Vc vowel into a Case. Stress marks are normalized
// before lookup.
func ParseCase(v string) (grammar.Case, bool) {
	c, ok := caseFromVc[NormalizeAccents(v)]
	return c, ok
}
