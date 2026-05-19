package grammar

// Configuration is one of 20 values describing the physical/compositional
// relationship between members of a set.
type Configuration int

const (
	UNI Configuration = iota
	DPX
	DSS
	DSC
	DSF
	DDS
	DDC
	DDF
	DFS
	DFC
	DFF
	MSS
	MSC
	MSF
	MDS
	MDC
	MDF
	MFS
	MFC
	MFF
)

var configurationNames = [...]string{
	"UNI", "DPX",
	"DSS", "DSC", "DSF", "DDS", "DDC", "DDF", "DFS", "DFC", "DFF",
	"MSS", "MSC", "MSF", "MDS", "MDC", "MDF", "MFS", "MFC", "MFF",
}

func (c Configuration) String() string { return configurationNames[c] }

// AllConfigurations enumerates all 20 configurations in declaration order.
var AllConfigurations = []Configuration{
	UNI, DPX,
	DSS, DSC, DSF, DDS, DDC, DDF, DFS, DFC, DFF,
	MSS, MSC, MSF, MDS, MDC, MDF, MFS, MFC, MFF,
}

// Affiliation is the social/functional relationship between set members.
type Affiliation int

const (
	CSL Affiliation = iota
	ASO
	COA
	VAR
)

func (a Affiliation) String() string {
	return [...]string{"CSL", "ASO", "COA", "VAR"}[a]
}

var AllAffiliations = []Affiliation{CSL, ASO, COA, VAR}

// Perspective marks boundedness and quantification of the referent.
// Names use trailing underscores to avoid collision with Configuration's
// M-prefixed constants and to read as "M-perspective", "G-perspective".
type Perspective int

const (
	M_ Perspective = iota
	G_
	N_
	A_
)

func (p Perspective) String() string {
	return [...]string{"M", "G", "N", "A"}[p]
}

var AllPerspectives = []Perspective{M_, G_, N_, A_}

// Extension describes the temporal/spatial extent of a referent.
type Extension int

const (
	DEL Extension = iota
	PRX
	ICP
	ATV
	GRA
	DPL
)

func (e Extension) String() string {
	return [...]string{"DEL", "PRX", "ICP", "ATV", "GRA", "DPL"}[e]
}

var AllExtensions = []Extension{DEL, PRX, ICP, ATV, GRA, DPL}

// Essence distinguishes Normal (real) from Representative (hypothetical).
type Essence int

const (
	NRM Essence = iota
	RPV
)

func (e Essence) String() string {
	return [...]string{"NRM", "RPV"}[e]
}

var AllEssences = []Essence{NRM, RPV}

// SlotVI = (Configuration, Affiliation, Perspective, Extension, Essence),
// encoded as the Ca consonant cluster.
type SlotVI struct {
	Configuration Configuration
	Affiliation   Affiliation
	Perspective   Perspective
	Extension     Extension
	Essence       Essence
}

// DefaultSlotVI is the unmarked Ca: UNI/CSL/M/DEL/NRM. It encodes as "l".
var DefaultSlotVI = SlotVI{UNI, CSL, M_, DEL, NRM}
