package grammar

// Register marks the discourse mode of a stretch of text. Narrative
// (NRR) is the unmarked default and produces no written marker.
// Other registers are opened by an h-vowel adjunct and closed by an
// "h…i"/"h…u" finalizer.
type Register int

const (
	NRR Register = iota // Narrative (default)
	DSV                 // Discursive (direct speech)
	PNT                 // Parenthetical (aside)
	SPF                 // Specificative (proper name)
	EXM                 // Exemplificative (giving example)
	CGT                 // Cogitant (internal thought)
	END                 // End of register/carrier
)

func (r Register) String() string {
	return [...]string{"NRR", "DSV", "PNT", "SPF", "EXM", "CGT", "END"}[r]
}

// AllRegisters enumerates every Register in declaration order.
var AllRegisters = []Register{NRR, DSV, PNT, SPF, EXM, CGT, END}

// registerInitialForms hold the opening h+vowel form for each register.
// NRR and END have no opening form.
var registerInitialForms = [...]string{
	NRR: "",
	DSV: "ha",
	PNT: "he",
	SPF: "hi",
	EXM: "ho",
	CGT: "hu",
	END: "",
}

// registerFinalForms hold the closing form for each register. NRR has
// no closing form; END is itself a finalizer (hüi).
var registerFinalForms = [...]string{
	NRR: "",
	DSV: "hai",
	PNT: "hei",
	SPF: "hiu",
	EXM: "hoi",
	CGT: "hui",
	END: "hüi",
}

// RegisterInitialForm returns the opening adjunct romanization for r.
// Empty if r has no opening form (NRR, END).
func RegisterInitialForm(r Register) string { return registerInitialForms[r] }

// RegisterFinalForm returns the closing adjunct romanization for r.
// Empty if r has no closing form (NRR).
func RegisterFinalForm(r Register) string { return registerFinalForms[r] }
