package grammar

// Bias is one of the ~70 attitudinal/emotional markers that can stand
// alone as a single-word adjunct. Each Bias has a unique consonant
// cluster as its romanization (no vowels) and a representative
// English expression that captures its tone.
type Bias int

const (
	DOL Bias = iota // Dolorous: sadness, grief
	DIS             // Dismissive: disregard
	DRS             // Derisive: mockery
	PES             // Pessimistic
	DUB             // Dubitative
	SKP             // Skeptical
	TRP             // Trepidative
	APH             // Apprehensive
	IPT             // Impatient
	ANP             // Anticipative
	DPB             // Disapprobative
	CTP             // Contemptive
	IDG             // Indignative
	EXA             // Exasperative
	RPU             // Repulsive
	IVD             // Invidious
	VEX             // Vexative
	STU             // Stupefactive
	PPX             // Perplexive
	DCC             // Disconcertive
	RVL             // Revelative
	FSC             // Fascinative
	EUH             // Euphoric
	GRT             // Gratificative
	SAT             // Satiative
	DLC             // Delectative
	IFT             // Infatuative
	SOL             // Solicitative
	RAC             // Reactive
	MAN             // Mandatory
	EXG             // Exigent
	ATE             // Attentive
	APB             // Approbative
	OPT             // Optimal
	CNV             // Contensive
	ACC             // Accidental
	ACH             // Archetypal
	IRO             // Ironic
	PSM             // Presumptive
	CRR             // Corrective
	EUP             // Euphemistic
	PSC             // Prosaic
	CMD             // Comedic
	PPV             // Propositive
	SGS             // Suggestive
	DFD             // Diffident
	RFL             // Reflective
	DES             // Desperative
	COI             // Coincidental
	FOR             // Fortuitous
	ANN             // Annunciative
	RSG             // Resignative
	ISP             // Insipid
	IPL             // Implicative
	MNF             // Manifestive
	ARB             // Arbitrary
	PPT             // Propitious
	CTV             // Contemplative
	CRP             // Corruptive
	DEJ             // Dejective
	ADS             // Admissive
)

var biasNames = [...]string{
	"DOL", "DIS", "DRS", "PES", "DUB", "SKP",
	"TRP", "APH", "IPT", "ANP",
	"DPB", "CTP", "IDG", "EXA", "RPU", "IVD", "VEX",
	"STU", "PPX", "DCC", "RVL", "FSC",
	"EUH", "GRT", "SAT", "DLC", "IFT",
	"SOL", "RAC", "MAN", "EXG", "ATE",
	"APB", "OPT", "CNV", "ACC", "ACH",
	"IRO", "PSM", "CRR", "EUP", "PSC", "CMD",
	"PPV", "SGS", "DFD", "RFL", "DES", "COI", "FOR",
	"ANN", "RSG", "ISP", "IPL", "MNF",
	"ARB", "PPT", "CTV", "CRP", "DEJ",
	"ADS",
}

func (b Bias) String() string { return biasNames[b] }

// biasExpressions are the representative English glosses.
var biasExpressions = [...]string{
	DOL: "Ow! Ouch!",
	DIS: "So what!",
	DRS: "How foolish!",
	PES: "Pfft!",
	DUB: "I doubt it",
	SKP: "Yeah, right!",
	TRP: "Oh, no!",
	APH: "I'm worried...",
	IPT: "C'mon!",
	ANP: "I'm looking forward to this!",
	DPB: "I don't like that...",
	CTP: "What nonsense!",
	IDG: "How dare...!?",
	EXA: "Don't you get it?",
	RPU: "Ew! Gross!",
	IVD: "How unfair!",
	VEX: "How annoying!",
	STU: "What the...?",
	PPX: "Huh?",
	DCC: "I don't feel comfortable about this...",
	RVL: "A-ha!",
	FSC: "Cool! Wow!",
	EUH: "What bliss!",
	GRT: "Ahhhh!",
	SAT: "How satisfying!",
	DLC: "Whee!",
	IFT: "Praise be to...!",
	SOL: "Please",
	RAC: "My goodness!",
	MAN: "Take it or leave it",
	EXG: "It's now or never!",
	ATE: "Who would have thought?",
	APB: "OK",
	OPT: "So!/Totally!",
	CNV: "I told you so!",
	ACC: "As luck would have it...",
	ACH: "Such a...!",
	IRO: "Just great!",
	PSM: "It can only mean one thing...",
	CRR: "What I meant to say is...",
	EUP: "Let me put it this way...",
	PSC: "Meh.",
	CMD: "Funny!",
	PPV: "Consider:",
	SGS: "How about...",
	DFD: "It's nothing, just...",
	RFL: "Look at it this way...",
	DES: "I'm sorry to have to tell you...",
	COI: "What a coincidence!",
	FOR: "All is well that ends well",
	ANN: "Wait till you hear this!",
	RSG: "So much for...!",
	ISP: "How boring!",
	IPL: "Of course,...",
	MNF: "Ah! Well, now! So!",
	ARB: "Yeah, whatever...",
	PPT: "It's a wonder that...",
	CTV: "Hmmmm...",
	CRP: "What corruption!",
	DEJ: "[dejected sigh]",
	ADS: "Mm-hm",
}

// BiasExpression returns a representative English expression for a Bias.
// May be empty (ANP).
func BiasExpression(b Bias) string { return biasExpressions[b] }

// AllBiases enumerates every Bias in declaration order.
var AllBiases = []Bias{
	DOL, DIS, DRS, PES, DUB, SKP,
	TRP, APH, IPT, ANP,
	DPB, CTP, IDG, EXA, RPU, IVD, VEX,
	STU, PPX, DCC, RVL, FSC,
	EUH, GRT, SAT, DLC, IFT,
	SOL, RAC, MAN, EXG, ATE,
	APB, OPT, CNV, ACC, ACH,
	IRO, PSM, CRR, EUP, PSC, CMD,
	PPV, SGS, DFD, RFL, DES, COI, FOR,
	ANN, RSG, ISP, IPL, MNF,
	ARB, PPT, CTV, CRP, DEJ,
	ADS,
}
