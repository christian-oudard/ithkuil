package grammar

// SlotIX is the sealed sum type for the final slot of a formative.
// Two variants:
//   - CaseSlot: nominal formatives (penultimate / antepenultimate /
//     monosyllabic stress) encode a Case as Vc.
//   - IllocValSlot: verbal formatives (ultimate stress) encode an
//     Illocution and Validation as Vk.
type SlotIX interface {
	slotIX()
}

// CaseSlot wraps a Case as a SlotIX variant.
type CaseSlot struct{ Case Case }

func (CaseSlot) slotIX() {}

// IllocValSlot wraps an Illocution + Validation pair as a SlotIX variant.
type IllocValSlot struct {
	Illocution Illocution
	Validation Validation
}

func (IllocValSlot) slotIX() {}
