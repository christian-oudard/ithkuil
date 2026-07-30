package parse

import (
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
)

// This file holds the written form of a grammatical value: what each
// enum spells as in the romanization.
//
// It sits here rather than in grammar because grammar is the
// phoneme-independent centre — a Case is a Case whether or not anyone
// ever says it — and because every one of these tables already had its
// decoder in this package. Each is now beside its own inverse.

// canonicalCaseVowel is the canonical Vc form for each case (the one
// produced by the encoder). Series-3 alternates and other parse-only
// variants live in parse.casePatterns.
var canonicalCaseVowel = map[g.Case]string{
	// Transrelative (Series 1)
	g.THM: "a", g.INS: "ä", g.ABS: "e", g.AFF: "i", g.STM: "ëi",
	g.EFF: "ö", g.ERG: "o", g.DAT: "ü", g.IND: "u",
	// Appositive (Series 2)
	g.POS: "ai", g.PRP: "au", g.GEN: "ei", g.ATT: "eu", g.PDC: "ëu",
	g.ITP: "ou", g.OGN: "oi", g.IDP: "iu", g.PAR: "ui",
	// Associative (Series 3)
	g.APL: "ia", g.PUR: "ie", g.TRA: "io", g.DFR: "iö", g.CRS: "eë",
	g.TSP: "uö", g.CMM: "uo", g.CMP: "ue", g.CSD: "ua",
	// Adverbial (Series 4)
	g.FUN: "ao", g.TFM: "aö", g.CLA: "eo", g.RSL: "eö", g.CSM: "oë",
	g.CON: "öe", g.AVR: "oe", g.CVS: "öa", g.SIT: "oa",
	// Relational (Series 1 + glottal stop)
	g.PRN: "a'a", g.DSP: "ä'ä", g.COR: "e'e", g.CPS: "i'i",
	g.COM: "ë'i", g.UTL: "ö'ö", g.PRD: "o'o", g.RLT: "u'u",
	// Affinitive (Series 2 + glottal stop)
	g.ACT: "a'i", g.ASI: "a'u", g.ESS: "e'i", g.TRM: "e'u",
	g.SEL: "ë'u", g.CFM: "o'u", g.DEP: "o'i", g.VOC: "u'i",
	// Spatio-Temporal I (Series 3 + glottal stop)
	g.LOC: "i'a", g.ATD: "i'e", g.ALL: "i'o", g.ABL: "i'ö",
	g.ORI: "e'ë", g.IRL: "u'ö", g.INV: "u'o", g.NAV: "u'a",
	// Spatio-Temporal II (Series 4 + glottal stop)
	g.CNR: "a'o", g.ASS: "a'ö", g.PER: "e'o", g.PRO: "e'ö",
	g.PCV: "o'ë", g.PCR: "ö'e", g.ELP: "o'e", g.PLM: "o'a",
}

// CaseToVc returns the canonical Vc form for a case.
func CaseToVc(c g.Case) string {
	return canonicalCaseVowel[c]
}

// biasForms is the written consonant cluster for each g.Bias.
var biasForms = [...]string{
	g.DOL: "řřx",
	g.DIS: "kff",
	g.DRS: "pfc",
	g.PES: "ksp",
	g.DUB: "mmf",
	g.SKP: "rnž",
	g.TRP: "llč",
	g.APH: "vvz",
	g.IPT: "žžv",
	g.ANP: "lst",
	g.DPB: "ffx",
	g.CTP: "kšš",
	g.IDG: "pšš",
	g.EXA: "kçç",
	g.RPU: "šštļ",
	g.IVD: "řřn",
	g.VEX: "ksk",
	g.STU: "ļļč",
	g.PPX: "llh",
	g.DCC: "gzj",
	g.RVL: "mmļ",
	g.FSC: "žžj",
	g.EUH: "gzz",
	g.GRT: "mmh",
	g.SAT: "ļţ",
	g.DLC: "ẓmm",
	g.IFT: "vvr",
	g.SOL: "ňňs",
	g.RAC: "kll",
	g.MAN: "msk",
	g.EXG: "rrs",
	g.ATE: "ňj",
	g.APB: "řs",
	g.OPT: "ççk",
	g.CNV: "rrj",
	g.ACC: "lf",
	g.ACH: "mçt",
	g.IRO: "mmž",
	g.PSM: "nnţ",
	g.CRR: "ňţ",
	g.EUP: "vvt",
	g.PSC: "žžt",
	g.CMD: "pļļ",
	g.PPV: "sl",
	g.SGS: "ltç",
	g.DFD: "cč",
	g.RFL: "llm",
	g.DES: "mřř",
	g.COI: "ššč",
	g.FOR: "lzp",
	g.ANN: "drr",
	g.RSG: "msf",
	g.ISP: "lçp",
	g.IPL: "vll",
	g.MNF: "pss",
	g.ARB: "xtļ",
	g.PPT: "mll",
	g.CTV: "gvv",
	g.CRP: "gžž",
	g.DEJ: "žžg",
	g.ADS: "lļ",
}

// BiasForm returns the written consonant cluster for a Bias.
func BiasForm(b g.Bias) string { return biasForms[b] }

// carrierForms maps each g.CarrierType to its h-consonant cluster.
var carrierForms = [...]string{
	g.Carrier:   "hl",
	g.Quotative: "hm",
	g.Naming:    "hn",
	g.Phrasal:   "hň",
}

// CarrierTypeForm returns the written consonant cluster for a g.CarrierType.
func CarrierTypeForm(c g.CarrierType) string { return carrierForms[c] }

// registerInitialForms hold the opening h+vowel form for each register.
// g.NRR and g.END have no opening form.
var registerInitialForms = [...]string{
	g.NRR: "",
	g.DSV: "ha",
	g.PNT: "he",
	g.SPF: "hi",
	g.EXM: "ho",
	g.CGT: "hu",
	g.END: "",
}

// registerFinalForms hold the closing form for each register. g.NRR has
// no closing form; g.END is itself a finalizer (hüi).
var registerFinalForms = [...]string{
	g.NRR: "",
	g.DSV: "hai",
	g.PNT: "hei",
	g.SPF: "hiu",
	g.EXM: "hoi",
	g.CGT: "hui",
	g.END: "hüi",
}

// RegisterInitialForm returns the opening adjunct romanization for r.
// Empty if r has no opening form (g.NRR, g.END).
func RegisterInitialForm(r g.Register) string { return registerInitialForms[r] }

// RegisterFinalForm returns the closing adjunct romanization for r.
// Empty if r has no closing form (g.NRR).
func RegisterFinalForm(r g.Register) string { return registerFinalForms[r] }

// SlotIIToVv encodes a g.SlotII as its Vv vowel using the Series 1 row of
// the vowel form table. Form 5 ("ëi") is reserved for the Cs-root special
// and never produced from a regular g.SlotII.
func SlotIIToVv(s g.SlotII) string {
	return SlotIIToVvSeries(s, 1)
}

// SlotIIToVvSeries encodes a g.SlotII as its Vv vowel in the given series
// (1-4). Series 1 is the canonical non-shortcut form; series 2-4 are
// used in Cc-shortcut forms to encode the elided Slot VI Ca alongside
// the Vv.
func SlotIIToVvSeries(s g.SlotII, series int) string {
	return phonology.VowelForm(series, slotIIForm(s))
}

// slotIIForm returns the form number (1-9) corresponding to a g.SlotII.
// Form 5 is reserved for the Cs-root special and is never produced
// from a regular g.SlotII.
func slotIIForm(s g.SlotII) int {
	switch s {
	case g.SlotII{Stem: g.S1, Version: g.PRC}:
		return 1
	case g.SlotII{Stem: g.S1, Version: g.CPT}:
		return 2
	case g.SlotII{Stem: g.S2, Version: g.PRC}:
		return 3
	case g.SlotII{Stem: g.S2, Version: g.CPT}:
		return 4
	case g.SlotII{Stem: g.S0, Version: g.CPT}:
		return 6
	case g.SlotII{Stem: g.S0, Version: g.PRC}:
		return 7
	case g.SlotII{Stem: g.S3, Version: g.CPT}:
		return 8
	case g.SlotII{Stem: g.S3, Version: g.PRC}:
		return 9
	}
	panic("grammar: unreachable g.SlotII")
}

// DefaultSlotII is the unmarked Vv: Stem 1, Processual.
// SlotIVToVr encodes a g.SlotIV as its Vr vowel. The series is determined
// by Context (g.EXS=1, FNC=2, RPS=3, AMG=4) and the form by the
// Function×Specification combination. Form 5 is reserved for Cs-roots.
func SlotIVToVr(s g.SlotIV) string {
	series := int(s.Context) + 1
	var form int
	switch {
	case s.Function == g.STA && s.Specification == g.BSC:
		form = 1
	case s.Function == g.STA && s.Specification == g.CTE:
		form = 2
	case s.Function == g.STA && s.Specification == g.CSV:
		form = 3
	case s.Function == g.STA && s.Specification == g.OBJ:
		form = 4
	case s.Function == g.DYN && s.Specification == g.OBJ:
		form = 6
	case s.Function == g.DYN && s.Specification == g.CSV:
		form = 7
	case s.Function == g.DYN && s.Specification == g.CTE:
		form = 8
	case s.Function == g.DYN && s.Specification == g.BSC:
		form = 9
	default:
		panic("grammar: unreachable g.SlotIV")
	}
	return phonology.VowelForm(series, form)
}

// ConcatMarker returns the romanization Slot I consonant for a
// ConcatenationStatus, or "" for the None state.
func ConcatMarker(s g.ConcatenationStatus) string {
	switch s {
	case g.Type1:
		return "h"
	case g.Type2:
		return "hw"
	}
	return ""
}

// VsForm is the inverse of grammar.VsScope: the V_S vowel that writes a
// scope on a §4.1.1 single-affix adjunct. ScopeVDom is the default and
// writes nothing, which is what the source's parenthesised "(a)" marks.
func VsForm(s g.AffixScope) string {
	return [...]string{"", "u", "e", "i", "o", "ö"}[s]
}

// CzForm is the inverse of grammar.CzScope: the C_Z consonant that
// writes the first affix's scope on a §4.1.2 multiple-affix adjunct.
// C_Z is structural — it is what tells the adjunct apart from a
// formative — so unlike V_S and V_Z it has no elided form and every
// scope writes one.
func CzForm(s g.AffixScope) string {
	return [...]string{"h", "'h", "'hl", "'hr", "hw", "'hw"}[s]
}

// VzForm is the inverse of grammar.VzScope: the V_Z vowel closing a
// multiple-affix adjunct. Pass the first affix's scope as well: §4.1.2
// gives "(ai)" for "same scope as C_Z", and an omitted V_Z says it more
// briefly, so the two scopes agreeing writes nothing at all.
func VzForm(rest, first g.AffixScope) string {
	if rest == first {
		return ""
	}
	return [...]string{"a", "u", "e", "i", "o", "ö"}[rest]
}
