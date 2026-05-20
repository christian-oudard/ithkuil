package grammar

// Case is one of the 68 cases of Ithkuil V4, organized into 8 groups:
// Transrelative, Appositive, Associative, Adverbial (9 each),
// Relational, Affinitive, SpatioTemporal1, SpatioTemporal2 (8 each).
// The enum value is a flat ordinal; Group() recovers the classification.
type Case int

const (
	// Transrelative (Series 1)
	THM Case = iota
	INS
	ABS
	AFF
	STM
	EFF
	ERG
	DAT
	IND

	// Appositive (Series 2)
	POS
	PRP
	GEN
	ATT
	PDC
	ITP
	OGN
	IDP
	PAR

	// Associative (Series 3)
	APL
	PUR
	TRA
	DFR
	CRS
	TSP
	CMM
	CMP
	CSD

	// Adverbial (Series 4)
	FUN
	TFM
	CLA
	RSL
	CSM
	CON
	AVR
	CVS
	SIT

	// Relational (Series 1 + glottal stop)
	PRN
	DSP
	COR
	CPS
	COM
	UTL
	PRD
	RLT

	// Affinitive (Series 2 + glottal stop)
	ACT
	ASI
	ESS
	TRM
	SEL
	CFM
	DEP
	VOC

	// Spatio-Temporal I (Series 3 + glottal stop)
	LOC
	ATD
	ALL
	ABL
	ORI
	IRL
	INV
	NAV

	// Spatio-Temporal II (Series 4 + glottal stop)
	CNR
	ASS
	PER
	PRO
	PCV
	PCR
	ELP
	PLM
)

// caseNames is the 3-letter abbreviation for each Case in declaration order.
var caseNames = [...]string{
	"THM", "INS", "ABS", "AFF", "STM", "EFF", "ERG", "DAT", "IND",
	"POS", "PRP", "GEN", "ATT", "PDC", "ITP", "OGN", "IDP", "PAR",
	"APL", "PUR", "TRA", "DFR", "CRS", "TSP", "CMM", "CMP", "CSD",
	"FUN", "TFM", "CLA", "RSL", "CSM", "CON", "AVR", "CVS", "SIT",
	"PRN", "DSP", "COR", "CPS", "COM", "UTL", "PRD", "RLT",
	"ACT", "ASI", "ESS", "TRM", "SEL", "CFM", "DEP", "VOC",
	"LOC", "ATD", "ALL", "ABL", "ORI", "IRL", "INV", "NAV",
	"CNR", "ASS", "PER", "PRO", "PCV", "PCR", "ELP", "PLM",
}

func (c Case) String() string { return caseNames[c] }

// CaseGroup classifies a case into one of the 8 case-series groups.
type CaseGroup int

const (
	Transrelative CaseGroup = iota
	Appositive
	Associative
	Adverbial
	Relational
	Affinitive
	SpatioTemporal1
	SpatioTemporal2
)

func (g CaseGroup) String() string {
	return [...]string{
		"Transrelative", "Appositive", "Associative", "Adverbial",
		"Relational", "Affinitive", "SpatioTemporal1", "SpatioTemporal2",
	}[g]
}

// Group returns the case-series group this Case belongs to.
func (c Case) Group() CaseGroup {
	switch {
	case c <= IND:
		return Transrelative
	case c <= PAR:
		return Appositive
	case c <= CSD:
		return Associative
	case c <= SIT:
		return Adverbial
	case c <= RLT:
		return Relational
	case c <= VOC:
		return Affinitive
	case c <= NAV:
		return SpatioTemporal1
	default:
		return SpatioTemporal2
	}
}

// AllCases lists every Case in declaration order. Useful for testing that
// the parsing/encoding tables are exhaustive.
var AllCases = []Case{
	THM, INS, ABS, AFF, STM, EFF, ERG, DAT, IND,
	POS, PRP, GEN, ATT, PDC, ITP, OGN, IDP, PAR,
	APL, PUR, TRA, DFR, CRS, TSP, CMM, CMP, CSD,
	FUN, TFM, CLA, RSL, CSM, CON, AVR, CVS, SIT,
	PRN, DSP, COR, CPS, COM, UTL, PRD, RLT,
	ACT, ASI, ESS, TRM, SEL, CFM, DEP, VOC,
	LOC, ATD, ALL, ABL, ORI, IRL, INV, NAV,
	CNR, ASS, PER, PRO, PCV, PCR, ELP, PLM,
}

// canonicalCaseVowel is the canonical Vc form for each case (the one
// produced by the encoder). Series-3 alternates and other parse-only
// variants live in parse.casePatterns.
var canonicalCaseVowel = map[Case]string{
	// Transrelative (Series 1)
	THM: "a", INS: "ä", ABS: "e", AFF: "i", STM: "ëi",
	EFF: "ö", ERG: "o", DAT: "ü", IND: "u",
	// Appositive (Series 2)
	POS: "ai", PRP: "au", GEN: "ei", ATT: "eu", PDC: "ëu",
	ITP: "ou", OGN: "oi", IDP: "iu", PAR: "ui",
	// Associative (Series 3)
	APL: "ia", PUR: "ie", TRA: "io", DFR: "iö", CRS: "eë",
	TSP: "uö", CMM: "uo", CMP: "ue", CSD: "ua",
	// Adverbial (Series 4)
	FUN: "ao", TFM: "aö", CLA: "eo", RSL: "eö", CSM: "oë",
	CON: "öe", AVR: "oe", CVS: "öa", SIT: "oa",
	// Relational (Series 1 + glottal stop)
	PRN: "a'a", DSP: "ä'ä", COR: "e'e", CPS: "i'i",
	COM: "ë'i", UTL: "ö'ö", PRD: "o'o", RLT: "u'u",
	// Affinitive (Series 2 + glottal stop)
	ACT: "a'i", ASI: "a'u", ESS: "e'i", TRM: "e'u",
	SEL: "ë'u", CFM: "o'u", DEP: "o'i", VOC: "u'i",
	// Spatio-Temporal I (Series 3 + glottal stop)
	LOC: "i'a", ATD: "i'e", ALL: "i'o", ABL: "i'ö",
	ORI: "e'ë", IRL: "u'ö", INV: "u'o", NAV: "u'a",
	// Spatio-Temporal II (Series 4 + glottal stop)
	CNR: "a'o", ASS: "a'ö", PER: "e'o", PRO: "e'ö",
	PCV: "o'ë", PCR: "ö'e", ELP: "o'e", PLM: "ö'a",
}

// CaseToVc returns the canonical Vc form for a case.
func CaseToVc(c Case) string {
	return canonicalCaseVowel[c]
}
