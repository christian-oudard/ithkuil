package grammar

// SlotIX is the sealed sum type for the final slot of a formative.
// Nominal / framed-verbal formatives use CaseSlot. Verbal formatives
// (ultimate stress) use one of the nine illocution variants below.
// Only the ASR illocution (Assertive) carries a Validation; the other
// eight illocutions are leaf variants per §3.9.3.2.
type SlotIX interface {
	slotIX()
	// Tag returns the gloss/CLI label for this variant (the illocution
	// abbreviation, or "ASR" for the Assertive variant — Validation is
	// appended by callers when non-default).
	Tag() string
}

// CaseSlot wraps a Case as a SlotIX variant.
type CaseSlot struct{ Case Case }

func (CaseSlot) slotIX()     {}
func (c CaseSlot) Tag() string { return c.Case.String() }

// Assertive — ASR illocution, paired with one of nine Validations.
type Assertive struct{ Validation Validation }

func (Assertive) slotIX()    {}
func (Assertive) Tag() string { return "ASR" }

// The remaining eight illocutions are leaf variants — Validation does
// not apply (§3.9.3.2).

type Directive struct{}

func (Directive) slotIX()    {}
func (Directive) Tag() string { return "DIR" }

type Declarative struct{}

func (Declarative) slotIX()    {}
func (Declarative) Tag() string { return "DEC" }

type Interrogative struct{}

func (Interrogative) slotIX()    {}
func (Interrogative) Tag() string { return "IRG" }

type Verificative struct{}

func (Verificative) slotIX()    {}
func (Verificative) Tag() string { return "VER" }

type Admonitive struct{}

func (Admonitive) slotIX()    {}
func (Admonitive) Tag() string { return "ADM" }

type Potentiative struct{}

func (Potentiative) slotIX()    {}
func (Potentiative) Tag() string { return "POT" }

type Hortative struct{}

func (Hortative) slotIX()    {}
func (Hortative) Tag() string { return "HOR" }

type Conjectural struct{}

func (Conjectural) slotIX()    {}
func (Conjectural) Tag() string { return "CNJ" }

// AllIllocutionVariants lists one canonical SlotIX value per
// illocution. Assertive carries OBS Validation; the other eight are
// leaf values. Useful for exhaustive iteration in tests and CLI lookup.
var AllIllocutionVariants = []SlotIX{
	Assertive{Validation: OBS},
	Directive{}, Declarative{}, Interrogative{}, Verificative{},
	Admonitive{}, Potentiative{}, Hortative{}, Conjectural{},
}
