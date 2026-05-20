package grammar

// Root is the sealed sum type for a formative's lexical identity.
// It consolidates the grammatical content of surface-form Slots II
// (Vv) and IV (Vr) into one unit. Three variants — one per kind of
// root the spec distinguishes:
//
//   - CrRoot: regular lexical formative whose Cr is a consonant
//     cluster looked up in the root lexicon. Stem and Version index
//     the lexical meaning; SlotIV (Function/Specification/Context)
//     adjusts it.
//   - CsRoot: affix-as-root formative (§4.2). The Cs identifier
//     comes from the affix lexicon; Degree (0-9) picks one of nine
//     meanings. Stem is implicitly S1 and Specification implicitly
//     BSC — both omitted here. Function comes from the special Vv
//     marker at render time but is stored as a grammatical field.
//   - RefRoot: referential formative (§5.3). C1 is a referential
//     consonant. Stem implicitly S1, otherwise structurally the same
//     as CrRoot.
//
// Surface markers that distinguish these (special Vv ëi/eë/ëu/oë for
// CsRoot, ae/ea for RefRoot) are a rendering concern and do not
// appear in the grammar.
type Root interface {
	root()
}

// CrRoot — regular lexical formative.
type CrRoot struct {
	Cluster string
	Stem    Stem
	Version Version
	SlotIV  SlotIV
}

func (CrRoot) root() {}

// CsRoot — affix-as-root formative.
type CsRoot struct {
	Cs       string
	Degree   int
	Version  Version
	Function Function
	Context  Context
}

func (CsRoot) root() {}

// RefRoot — referential formative.
type RefRoot struct {
	C1      string
	Version Version
	SlotIV  SlotIV
}

func (RefRoot) root() {}

// DefaultCrRoot returns a CrRoot with all fields at their grammatical
// defaults for the given consonant cluster (S1/PRC, STA/BSC/EXS).
func DefaultCrRoot(cluster string) CrRoot {
	return CrRoot{
		Cluster: cluster,
		Stem:    S1,
		Version: PRC,
		SlotIV:  DefaultSlotIV,
	}
}
