package grammar

// Word is the sealed sum over the word classes. §4.9 uses the term for
// exactly this range — "words such as single- or dual-Referentials,
// the carrier adjunct, and a short monosyllabic modular adjunct" —
// alongside the formatives of §3, and §2.3 builds the whole
// pitch-accent system around the word as the unit whose boundaries a
// listener recovers.
//
// A Word holds grammar and nothing else. How it was spelled, or
// whether it was ever spelled at all, belongs to the packages that
// convert; one built by hand carries no text and is no less a word.
//
// A stretch of romanization that cannot be read is not a member. It is
// not a kind of word, it is a failure to find one, and it comes back
// from the parser as an error instead. That keeps a Text well-formed
// by construction and lets the conversions out of it be total.
type Word interface {
	word()
}

func (Formative) word()              {}
func (Chain) word()                  {}
func (Referential) word()            {}
func (CombinationReferential) word() {}
func (Bias) word()                   {}
func (RegisterMarker) word()         {}
func (ModularAdjunct) word()         {}
func (CarrierAdjunct) word()         {}
func (SingleAffixAdjunct) word()     {}
func (MultipleAffixAdjunct) word()   {}
func (Foreign) word()                {}

// RegisterMarker opens or closes a non-narrative register (§4.4). The
// two ends are one word class over one inventory, so they are one type
// with a flag rather than two types that would have to be kept in
// step.
type RegisterMarker struct {
	Register Register
	End      bool
}

// Foreign is text carried through untouched: the name, quotation or
// phrase a §4.5 carrier adjunct scopes over. It is the one word whose
// meaning genuinely is its letters, which is why it is the one Word
// holding any, and nothing here reads them as Ithkuil.
type Foreign struct {
	Text string
}

// Text is a span of language: its words, in order, one or many.
//
// The name is ours rather than Quijada's, deliberately. His levels
// above the word are sentence, clause and phrase, and none of them
// describes what actually gets handed to a parser, which may be a
// single word, a fragment, or several sentences. Text claims only that
// it is language.
//
// It carries no method named after another format. Turning a Text into
// a romanization or a gloss belongs to the package that owns that
// format; giving Text those methods would make the centre depend on
// every periphery, which is the arrangement this package exists to
// avoid. Derivations that need no format at all — whether a modular
// adjunct's neighbour is verbal, say — live in semantics.
type Text []Word
