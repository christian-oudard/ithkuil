package grammar

// Root is the sealed sum type for a formative's lexical identity.
// It consolidates the grammatical content of written Slots II
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
//   - RefRoot: referential formative (§4.6.4). Refs is the chain of
//     personal references the root names, which is what §4.6.4 means
//     by "a combination Referential affix" — not the cluster that
//     spells it. Stem implicitly S1, otherwise structurally the same
//     as CrRoot.
//
// Both specialized constructions replace Slot II's eight-value Stem ×
// Version table and neither says what becomes of Stem. §4.2 uses four
// Slot II values encoding Version by Function, Function having moved up
// from Slot IV to make room for Affix-Degree, and it settles
// Specification explicitly while saying nothing about Stem. §4.6.4 uses
// two encoding Version alone and states that its Slot IV shows
// Function, Specification and Context as for a standard formative, so
// Stem has no home in either slot.
//
// S1 is the reading here because it is the only one either construction
// can spell. §4.6.4 does write as though stems were still available —
// "the meaning of each stem ... changes depending on its Perspective",
// over a table headed "Stem 1 Nominal meaning" — but supplies no form
// for a second or third, so nothing else is expressible.
//
// Written markers that distinguish these (special Vv ëi/eë/ëu/oë for
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
//
// Refs holds the personal references the root names, not the cluster
// that spells them. §4.6.4 calls the Slot III form "a combination
// Referential affix", so the content is a referent chain; keeping it
// as a string meant every reader decoded it again, and a RefRoot could
// be built on a cluster that decodes to nothing. §4.6.4 also bars the
// §4.6 category affixes here, the Slot VI Perspective carrying those
// distinctions instead, so there is no category to hold.
type RefRoot struct {
	Refs    []PersonalRef
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
