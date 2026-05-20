package grammar

// Formative is the full V4 morphological structure of an Ithkuil word.
//
// SlotI, SlotIShortcut, and SlotVIII are pointer-valued to express
// optionality: nil means "absent" (the slot did not appear in the
// surface form), while a non-nil pointer holds the parsed value.
//
// SentenceStarter is true when the surface form carried a leading ç
// marker (sentence-start prefix). The prefix is stripped before slot
// parsing — only the flag is preserved.
//
// CsRootDegree is set on Cs-root formatives (special Vv = ëi/eë/ëu/oë).
// Cs-root forms put an affix Cs in the Cr slot and encode the affix
// degree in the Vr slot; the int here records that degree (0-9). nil
// means the formative is a normal root or a reference-root.
//
// Final is the grammatical category — encodes Slot IX content together
// with the verbal/nominal/framed-verbal discrimination that surface
// stress reflects. Replaces the older independent Stress + SlotIX
// fields, eliminating the ability to construct inconsistent pairs.
type Formative struct {
	SlotI           *ConcatenationStatus // Cc: optional concatenation
	SlotIShortcut   *CcShortcut          // Cc: optional Ca shortcut
	SlotII          SlotII               // Vv: stem + version
	SlotIII         Root                 // Cr: root consonants
	SlotIV          SlotIV               // Vr: function + spec + context
	SlotV           []Affix              // CsVx: stem affixes
	SlotVI          SlotVI               // Ca: configuration complex
	SlotVII         []Affix              // VxCs: Ca-scoped affixes
	SlotVIII        SlotVIII             // VnCn (sum type or nil)
	Final           Final                // grammatical Slot IX + stress role
	SentenceStarter bool                 // ç prefix was present
	CsRootDegree    *int                 // Cs-root degree (nil for normal roots)
}

// MinimalFormative builds a formative whose only meaningful content is
// the root. Every other slot takes its grammatical default. Useful as
// a starting point for incremental construction in tests and tooling.
func MinimalFormative(root string) Formative {
	return Formative{
		SlotII:  DefaultSlotII,
		SlotIII: Root(root),
		SlotIV:  DefaultSlotIV,
		SlotVI:  DefaultSlotVI,
		Final:   UnframedNominal{Case: THM},
	}
}
