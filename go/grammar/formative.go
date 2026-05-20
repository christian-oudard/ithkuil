package grammar

// Formative is the full grammatical structure of a V4 Ithkuil word.
// Every field carries grammatical meaning — surface-form encoding
// decisions (shortcut form, default-value elision, stress diacritic
// placement, §3.5.1 / §3.6.1 signals for Slot V presence, special Vv
// markers for Cs/Ref roots) live in the render package.
//
// Concat is the optional concatenation status — nil for a standalone
// formative; Type1 or Type2 for the corresponding chain position.
//
// Root consolidates the lexical identity (Slot II + Slot III + Slot
// IV) into one sum-type value; see root.go for the three variants.
//
// SlotVIII is nil when the slot is absent.
//
// SentenceStarter is true when the surface form carried a leading ç
// marker. The prefix has no grammatical content beyond signalling
// sentence start.
type Formative struct {
	Concat          *ConcatenationStatus
	Root            Root
	SlotV           []Affix
	SlotVI          SlotVI
	SlotVII         []Affix
	SlotVIII        SlotVIII
	Final           Final
	SentenceStarter bool
}

// MinimalFormative builds a formative whose only meaningful content is
// the lexical cluster — every other field is at its grammatical
// default. Useful as a starting point for incremental construction in
// tests and tooling.
func MinimalFormative(cluster string) Formative {
	return Formative{
		Root:   DefaultCrRoot(cluster),
		SlotVI: DefaultSlotVI,
		Final:  UnframedNominal{Case: THM},
	}
}
