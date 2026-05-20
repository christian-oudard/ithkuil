package grammar

// Final is the sealed sum type for the formative's grammatical
// category. It combines the SlotIX content with the verbal/nominal/
// framed-verbal discrimination that stress encodes in the surface
// form — keeping the grammatical truth in one place so that
// inconsistent (Stress, SlotIX) pairs are unrepresentable.
//
// Variants:
//
//   - UnframedNominal: nominal formative. SlotIX is a Vc Case.
//     Surface form has penultimate stress (the default, no diacritic).
//   - FramedVerbal: framed-verbal formative. SlotIX is a Vc Case.
//     Surface form has antepenultimate stress (acute on the third-
//     from-last vowel).
//   - UnframedVerbal: verbal formative. SlotIX is a Vk illocution.
//     Surface form has ultimate stress (acute on the last vowel) —
//     except for monosyllabic words, where ultimate is implicit
//     and no diacritic is written (§3.10).
type Final interface {
	final()
	// Tag returns the gloss label for the grammatical category.
	// "" for UnframedNominal (penultimate is the unmarked default).
	Tag() string
}

// UnframedNominal: penultimate stress, Slot IX is Vc Case.
type UnframedNominal struct{ Case Case }

func (UnframedNominal) final()    {}
func (UnframedNominal) Tag() string { return "" }

// FramedVerbal: antepenultimate stress, Slot IX is Vc Case.
type FramedVerbal struct{ Case Case }

func (FramedVerbal) final()    {}
func (FramedVerbal) Tag() string { return "ANT" }

// UnframedVerbal: ultimate stress, Slot IX is Vk.
type UnframedVerbal struct{ Vk Vk }

func (UnframedVerbal) final()    {}
func (UnframedVerbal) Tag() string { return "ULT" }
