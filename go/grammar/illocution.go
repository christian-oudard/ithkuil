package grammar

// Validation is evidentiality — how the speaker knows what they assert.
// Only meaningful with ASSERTIVE illocution (§3.9.3.2), so it appears
// only as a field on Assertive (one of the SlotIX variants).
type Validation int

const (
	OBS Validation = iota // Observational
	REC                   // Recollective
	PUP                   // Purportive
	RPR                   // Reportive
	USP                   // Unspecified
	IMA                   // Imaginary
	CVN                   // Conventional
	ITU                   // Intuitive
	INF                   // Inferential
)

func (v Validation) String() string {
	return [...]string{"OBS", "REC", "PUP", "RPR", "USP", "IMA", "CVN", "ITU", "INF"}[v]
}

var AllValidations = []Validation{OBS, REC, PUP, RPR, USP, IMA, CVN, ITU, INF}
