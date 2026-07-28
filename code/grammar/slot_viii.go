package grammar

// Valence is the argument structure of a verbal predicate. One of the
// five Pattern-1 Vn categories (Series 1 vowels).
type Valence int

const (
	MNO Valence = iota // Monoactive
	PRL                // Parallel
	CRO                // Corollary
	RCP                // Reciprocal
	CPL                // Complementary
	DUP                // Duplicative
	DEM                // Demonstrative
	CNG                // Contingent
	PTI                // Participative
)

func (v Valence) String() string {
	return [...]string{"MNO", "PRL", "CRO", "RCP", "CPL", "DUP", "DEM", "CNG", "PTI"}[v]
}

var AllValences = []Valence{MNO, PRL, CRO, RCP, CPL, DUP, DEM, CNG, PTI}

// Phase is the temporal contour of an action. Pattern-1 Vn (Series 2).
type Phase int

const (
	PCT Phase = iota // Punctual
	ITR              // Iterative
	REP              // Repetitive
	ITM              // Intermittent
	RCT              // Recurrent
	FRE              // Frequentative
	FRG              // Fragmentative
	VAC              // Vacillative
	FLC              // Fluctuative
)

func (p Phase) String() string {
	return [...]string{"PCT", "ITR", "REP", "ITM", "RCT", "FRE", "FRG", "VAC", "FLC"}[p]
}

var AllPhases = []Phase{PCT, ITR, REP, ITM, RCT, FRE, FRG, VAC, FLC}

// Effect names who is helped or harmed by the action. Pattern-1 Vn
// (Series 3).
type Effect int

const (
	BEN1 Effect = iota // Beneficial to speaker
	BEN2               // Beneficial to addressee
	BEN3               // Beneficial to 3rd party
	BSLF               // Beneficial to self
	UNK                // Unknown
	DSLF               // Detrimental to self
	DET3               // Detrimental to 3rd party
	DET2               // Detrimental to addressee
	DET1               // Detrimental to speaker
)

func (e Effect) String() string {
	return [...]string{"BEN1", "BEN2", "BEN3", "BSLF", "UNK", "DSLF", "DET3", "DET2", "DET1"}[e]
}

var AllEffects = []Effect{BEN1, BEN2, BEN3, BSLF, UNK, DSLF, DET3, DET2, DET1}

// Level is the degree of comparison. Pattern-1 Vn (Series 4).
type Level int

const (
	MIN Level = iota // Minimal
	SBE              // Subequative
	IFR              // Inferior
	DFT              // Deficient
	EQU              // Equative
	SUR              // Surpassive
	SPL              // Superlative
	SPQ              // Superequative
	MAX              // Maximal
)

func (l Level) String() string {
	return [...]string{"MIN", "SBE", "IFR", "DFT", "EQU", "SUR", "SPL", "SPQ", "MAX"}[l]
}

var AllLevels = []Level{MIN, SBE, IFR, DFT, EQU, SUR, SPL, SPQ, MAX}

// Aspect names the temporal phase of an event. 36 values arranged in 4
// columns of 9; selected by Pattern-2 Vn.
type Aspect int

const (
	RTR Aspect = iota
	PRS
	HAB
	PRG
	IMM
	PCS
	REG
	SMM
	ATP
	RSM
	CSS
	PAU
	RGR
	PCL
	CNT
	ICS
	EXP
	IRP
	PMP
	CLM
	DLT
	TMP
	XPD
	LIM
	EPD
	PTC
	PPR
	DCL
	CCL
	CUL
	IMD
	TRD
	TNS
	ITC
	MTV
	SQN
)

var aspectNames = [...]string{
	"RTR", "PRS", "HAB", "PRG", "IMM", "PCS", "REG", "SMM", "ATP",
	"RSM", "CSS", "PAU", "RGR", "PCL", "CNT", "ICS", "EXP", "IRP",
	"PMP", "CLM", "DLT", "TMP", "XPD", "LIM", "EPD", "PTC", "PPR",
	"DCL", "CCL", "CUL", "IMD", "TRD", "TNS", "ITC", "MTV", "SQN",
}

func (a Aspect) String() string { return aspectNames[a] }

var AllAspects = []Aspect{
	RTR, PRS, HAB, PRG, IMM, PCS, REG, SMM, ATP,
	RSM, CSS, PAU, RGR, PCL, CNT, ICS, EXP, IRP,
	PMP, CLM, DLT, TMP, XPD, LIM, EPD, PTC, PPR,
	DCL, CCL, CUL, IMD, TRD, TNS, ITC, MTV, SQN,
}

// Mood is the reality status of an utterance (verbal). Selected by Cn
// when stress is ultimate.
type Mood int

const (
	FAC Mood = iota // Factual
	SUB             // Subjunctive
	ASM             // Assumptive
	SPC             // Speculative
	COU             // Counterfactive
	HYP             // Hypothetical
)

func (m Mood) String() string {
	return [...]string{"FAC", "SUB", "ASM", "SPC", "COU", "HYP"}[m]
}

var AllMoods = []Mood{FAC, SUB, ASM, SPC, COU, HYP}

// CaseScope is the nominal counterpart of Mood, controlling how Slot IX
// case interacts with embedded clauses. Same six values as Mood, paired
// 1-to-1.
type CaseScope int

const (
	CCN CaseScope = iota // ≈ FAC
	CCA                  // ≈ SUB
	CCS                  // ≈ ASM
	CCQ                  // ≈ SPC
	CCP                  // ≈ COU
	CCV                  // ≈ HYP
)

func (c CaseScope) String() string {
	return [...]string{"CCN", "CCA", "CCS", "CCQ", "CCP", "CCV"}[c]
}

var AllCaseScopes = []CaseScope{CCN, CCA, CCS, CCQ, CCP, CCV}

// MoodToCaseScope maps a Mood to its CaseScope counterpart, used when
// a Slot VIII parsed as Mood needs to be reinterpreted nominally.
func MoodToCaseScope(m Mood) CaseScope {
	return [...]CaseScope{CCN, CCA, CCS, CCQ, CCP, CCV}[m]
}

// CaseScopeToMood maps a CaseScope to its Mood counterpart, used when
// a Slot VIII parsed as CaseScope needs to be reinterpreted verbally.
func CaseScopeToMood(c CaseScope) Mood {
	return [...]Mood{FAC, SUB, ASM, SPC, COU, HYP}[c]
}

// SlotVIII is the sealed sum type for the VnCn slot. Exactly one of:
// VnCnValence, VnCnPhase, VnCnEffect, VnCnLevel, VnCnAspect.
//
// The MoodScope field on each variant holds one of six values shared
// between the Mood enum (verbal labelling: FAC/SUB/ASM/SPC/COU/HYP)
// and the CaseScope enum (nominal labelling: CCN/CCA/CCS/CCQ/CCP/CCV).
// The same Cn consonant encodes both — the gloss layer picks the
// label based on the formative's Final category. SlotVIIIMoodScope
// extracts the field without a type switch at the call site.
type SlotVIII interface {
	slotVIII()
}

// VnCnValence: Pattern-1, Vn = Series 1 (Valence).
type VnCnValence struct {
	Valence   Valence
	MoodScope Mood
}

func (VnCnValence) slotVIII() {}

// VnCnPhase: Pattern-1, Vn = Series 2 (Phase).
type VnCnPhase struct {
	Phase     Phase
	MoodScope Mood
}

func (VnCnPhase) slotVIII() {}

// VnCnEffect: Pattern-1, Vn = Series 3 (Effect).
type VnCnEffect struct {
	Effect    Effect
	MoodScope Mood
}

func (VnCnEffect) slotVIII() {}

// VnCnLevel: Pattern-1, Vn = Series 4 (Level). Absolute marks the rare
// alternate "absolute level" reading; the default (relative) is false.
type VnCnLevel struct {
	Level     Level
	Absolute  bool
	MoodScope Mood
}

func (VnCnLevel) slotVIII() {}

// VnCnAspect: Pattern-2, Vn = any aspect column.
type VnCnAspect struct {
	Aspect    Aspect
	MoodScope Mood
}

func (VnCnAspect) slotVIII() {}

// SlotVIIIMoodScope returns the MoodScope field of any SlotVIII
// variant. Returns FAC for a nil receiver (the grammatical default
// when Slot VIII is absent).
func SlotVIIIMoodScope(s SlotVIII) Mood {
	switch v := s.(type) {
	case VnCnValence:
		return v.MoodScope
	case VnCnPhase:
		return v.MoodScope
	case VnCnEffect:
		return v.MoodScope
	case VnCnLevel:
		return v.MoodScope
	case VnCnAspect:
		return v.MoodScope
	}
	return FAC
}

// SlotVIIIVnLabel returns the abbreviation of the Vn category stored
// in a SlotVIII variant (Valence/Phase/Effect/Level/Aspect). Returns
// "" for a nil receiver.
func SlotVIIIVnLabel(s SlotVIII) string {
	switch v := s.(type) {
	case VnCnValence:
		return v.Valence.String()
	case VnCnPhase:
		return v.Phase.String()
	case VnCnEffect:
		return v.Effect.String()
	case VnCnLevel:
		return v.Level.String()
	case VnCnAspect:
		return v.Aspect.String()
	}
	return ""
}
