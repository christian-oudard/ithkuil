package grammar

// Final is the sealed sum type for the formative's grammatical
// category. It combines the SlotIX content with the verbal/nominal/
// framed-verbal discrimination that stress encodes in the romanization
// form — keeping the grammatical truth in one place so that
// inconsistent (Stress, SlotIX) pairs are unrepresentable.
//
// Variants:
//
//   - UnframedNominal: nominal formative. SlotIX is a Vc Case.
//     Romanization has penultimate stress (the default, no diacritic).
//   - FramedVerbal: framed-verbal formative. SlotIX is a Vc Case.
//     Romanization has antepenultimate stress (acute on the third-
//     from-last vowel).
//   - UnframedVerbal: verbal formative. SlotIX is a Vk illocution.
//     Romanization has ultimate stress (acute on the last vowel) —
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

func (UnframedNominal) final()      {}
func (UnframedNominal) Tag() string { return "" }

// FramedVerbal: antepenultimate stress, Slot IX is Vc Case.
type FramedVerbal struct{ Case Case }

func (FramedVerbal) final()      {}
func (FramedVerbal) Tag() string { return "ANT" }

// UnframedVerbal: ultimate stress, Slot IX is Vk.
type UnframedVerbal struct{ Vk Vk }

func (UnframedVerbal) final()      {}
func (UnframedVerbal) Tag() string { return "ULT" }

// IsVerbal reports whether the formative's Final selects the verbal
// reading for the Slot VIII Cn (Mood vs. Case-Scope) and for any
// modular adjunct that scopes over it. Per §3.8.1, only UNFRAMED
// verbal formatives (ultimate stress) take Mood; nominal and
// FRAMED-verbal formatives take Case-Scope.
func IsVerbal(f Final) bool {
	_, ok := f.(UnframedVerbal)
	return ok
}
