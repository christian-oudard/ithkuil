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
