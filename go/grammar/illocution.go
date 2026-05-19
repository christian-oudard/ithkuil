package grammar

// Illocution is the speech-act type. Encoded together with Validation
// in the Vk vowel when the formative is verbal (ultimate stress).
type Illocution int

const (
	ASR Illocution = iota // Assertive
	DIR                   // Directive
	DEC                   // Declarative
	IRG                   // Interrogative
	VER                   // Verificative
	ADM                   // Admonitive
	POT                   // Potentiative
	HOR                   // Hortative
	CNJ                   // Conjectural
)

func (i Illocution) String() string {
	return [...]string{"ASR", "DIR", "DEC", "IRG", "VER", "ADM", "POT", "HOR", "CNJ"}[i]
}

var AllIllocutions = []Illocution{ASR, DIR, DEC, IRG, VER, ADM, POT, HOR, CNJ}

// Validation is evidentiality — how the speaker knows what they assert.
// Pairs with Illocution in the Vk vowel; when Illocution is non-ASR,
// Validation defaults to OBS.
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
