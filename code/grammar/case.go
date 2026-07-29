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
