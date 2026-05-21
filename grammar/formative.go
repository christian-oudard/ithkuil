package grammar

// Formative is the full grammatical structure of a V4 Ithkuil word.
// Every field carries grammatical meaning — surface-form encoding
// decisions (shortcut form, default-value elision, stress diacritic
// placement, §3.5.1 / §3.6.1 signals for Slot V presence, special Vv
// markers for Cs/Ref roots) live in the render package.
//
// Invariants — Root and Final must be non-nil; every formative has a
// root and a final case/illocution marker. The zero value
// (Formative{}) is not a valid formative — use MinimalFormative as a
// starting point. render and gloss panic on nil Root or nil Final.
//
// Concat is the optional concatenation status — nil for a standalone
// formative; Type1 or Type2 for the corresponding chain position.
//
// Root consolidates the lexical identity (Slot II + Slot III + Slot
// IV) into one sum-type value; see root.go for the three variants.
//
// SlotVIII is nil when the slot is absent.
//
// The ç(ë)- "sentence-juncture" prefix from §1.3.2/§5.8.8 is purely
// prosodic — the spec describes it as "normally never written" outside
// of scripts for performance. The parser accepts and discards it; it
// has no representation on Formative.
//
// Render always emits the canonical surface for a given Formative —
// shortcut form when conditions match, default elisions applied. A
// formative parsed from a long-form input and re-rendered comes back
// as the canonical (often shorter) form, by design. The non-canonical
// input is not lost in any meaningful sense — the gloss and grammar
// are identical to those of the canonical equivalent.
type Formative struct {
	Concat   *ConcatenationStatus
	Root     Root
	SlotV    []Affix
	SlotVI   SlotVI
	SlotVII  []Affix
	SlotVIII SlotVIII
	Final    Final
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
