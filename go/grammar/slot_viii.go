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

// MoodOrScope is the {Mood | CaseScope} sum type, encoded in the Cn
// consonant of Slot VIII. Implementations: MoodVal, CaseScopeVal.
//
// The Vn/Cn parser produces one variant or the other based on which
// Cn pattern matched; callers apply DisambiguateMoodScope using the
// formative's actual stress to coerce the variant if needed.
type MoodOrScope interface {
	moodOrScope()
}

// MoodVal wraps a Mood as a MoodOrScope.
type MoodVal struct{ Mood Mood }

func (MoodVal) moodOrScope() {}

// CaseScopeVal wraps a CaseScope as a MoodOrScope.
type CaseScopeVal struct{ CaseScope CaseScope }

func (CaseScopeVal) moodOrScope() {}

// DisambiguateMoodScope converts a MoodOrScope to the right variant
// for the formative's grammatical role:
//   - Ultimate or Antepenultimate stress = verbal → Mood.
//   - Penultimate or Monosyllabic stress = nominal → CaseScope.
//
// Verbal flips CaseScopeVal → MoodVal; nominal flips MoodVal → CaseScopeVal.
// Already-correct variants pass through unchanged.
func DisambiguateMoodScope(stress Stress, ms MoodOrScope) MoodOrScope {
	switch stress {
	case Ultimate, Antepenultimate:
		if cs, ok := ms.(CaseScopeVal); ok {
			return MoodVal{Mood: CaseScopeToMood(cs.CaseScope)}
		}
		return ms
	default:
		if m, ok := ms.(MoodVal); ok {
			return CaseScopeVal{CaseScope: MoodToCaseScope(m.Mood)}
		}
		return ms
	}
}

// SlotVIII is the sealed sum type for the VnCn slot. Exactly one of:
// VnCnValence, VnCnPhase, VnCnEffect, VnCnLevel, VnCnAspect.
type SlotVIII interface {
	slotVIII()
}

// VnCnValence: Pattern-1, Vn = Series 1 (Valence).
type VnCnValence struct {
	Valence Valence
	MS      MoodOrScope
}

func (VnCnValence) slotVIII() {}

// VnCnPhase: Pattern-1, Vn = Series 2 (Phase).
type VnCnPhase struct {
	Phase Phase
	MS    MoodOrScope
}

func (VnCnPhase) slotVIII() {}

// VnCnEffect: Pattern-1, Vn = Series 3 (Effect).
type VnCnEffect struct {
	Effect Effect
	MS     MoodOrScope
}

func (VnCnEffect) slotVIII() {}

// VnCnLevel: Pattern-1, Vn = Series 4 (Level). Absolute marks the rare
// alternate "absolute level" reading; the default (relative) is false.
type VnCnLevel struct {
	Level    Level
	Absolute bool
	MS       MoodOrScope
}

func (VnCnLevel) slotVIII() {}

// VnCnAspect: Pattern-2, Vn = any aspect column.
type VnCnAspect struct {
	Aspect Aspect
	MS     MoodOrScope
}

func (VnCnAspect) slotVIII() {}

// DisambiguateSlotVIII applies DisambiguateMoodScope to the MS field of
// whichever SlotVIII variant is held.
func DisambiguateSlotVIII(stress Stress, s SlotVIII) SlotVIII {
	switch v := s.(type) {
	case VnCnValence:
		return VnCnValence{Valence: v.Valence, MS: DisambiguateMoodScope(stress, v.MS)}
	case VnCnPhase:
		return VnCnPhase{Phase: v.Phase, MS: DisambiguateMoodScope(stress, v.MS)}
	case VnCnEffect:
		return VnCnEffect{Effect: v.Effect, MS: DisambiguateMoodScope(stress, v.MS)}
	case VnCnLevel:
		return VnCnLevel{Level: v.Level, Absolute: v.Absolute, MS: DisambiguateMoodScope(stress, v.MS)}
	case VnCnAspect:
		return VnCnAspect{Aspect: v.Aspect, MS: DisambiguateMoodScope(stress, v.MS)}
	}
	return s
}
