package slots

// A builder needs to know which grammatical categories are edited in
// which slot, and that is knowledge about the language, not about a
// user interface. Ithkapp put it in its templates, as `:disabled`
// expressions on components, so "Similarity is inapplicable under a
// uniplex configuration" became a string in an HTML attribute. Anything
// building on this package gets the structure from here instead.
//
// The mapping is checked rather than asserted: positions_test.go walks
// every sample in the inventory package, one minimal word per
// grammatical value, and fails if changing a value of some category
// leaves the slot declared for it untouched.

// Position is one place in a formative that a builder offers controls
// for. Field names the Layout field it writes, which is also the slot
// label the segment breakdown prints.
type Position struct {
	// Slot is the §3 slot number, "I" through "X".
	Slot string
	// Field is the Layout field, "" for Slot X, which is carried by
	// stress rather than by any conjunct.
	Field string
	// Name is what to title the group of controls.
	Name string
	// Categories are the grammar categories edited here, named as
	// search.Table names them so the values can be looked up directly.
	// Empty where the slot holds something that is not a category: a
	// root, an affix list, or a concatenation status.
	Categories []string
	// Note records a slot whose reading depends on something outside
	// it, which is the part a builder cannot infer from the list above.
	Note string
}

// Positions returns the formative's slots in written order.
//
// Two of them are conditional, and both conditions are facts about the
// language that a front end would otherwise have to reimplement:
//
//   - Slot VIII's Vn is read as Valence, Phase, Effect, Level or Aspect
//     depending on the Cn beside it, and that same Cn also chooses
//     between Mood and Case-Scope. semantics.VnCategory decides it.
//   - Slot IX is a Case under penultimate or antepenultimate stress and
//     an illocution-plus-validation under ultimate or monosyllabic
//     stress. That is Slot X reaching back into Slot IX, which is why
//     the relation is listed as its own position rather than as a
//     property of the case.
func Positions() []Position {
	return []Position{{
		Slot:  "I",
		Field: "Cc",
		Name:  "Concatenation",
		Note:  "concatenation status and the Slot II/IV shortcuts, not a category of its own",
	}, {
		Slot:       "II",
		Field:      "Vv",
		Name:       "Stem and Version",
		Categories: []string{"Stem", "Version"},
	}, {
		Slot:  "III",
		Field: "Cr",
		Name:  "Root",
		Note:  "the lexical root, looked up rather than chosen from a table",
	}, {
		Slot:       "IV",
		Field:      "Vr",
		Name:       "Function, Specification, Context",
		Categories: []string{"Function", "Specification", "Context"},
	}, {
		Slot:  "V",
		Field: "SlotV",
		Name:  "Affixes before Ca",
		Note:  "affixes here apply to the stem alone, not to the whole formative",
	}, {
		Slot:  "VI",
		Field: "Ca",
		Name:  "Ca complex",
		Categories: []string{
			"Configuration", "Affiliation", "Extension", "Perspective", "Essence",
		},
		Note: "one cluster spells all five at once, so they are not independently writable",
	}, {
		Slot:  "VII",
		Field: "SlotVII",
		Name:  "Affixes after Ca",
		Note:  "affixes here apply to the formative as a whole",
	}, {
		Slot:  "VIII",
		Field: "Vn",
		Name:  "Valence, Phase, Effect, Level, Aspect",
		Categories: []string{
			"Valence", "Phase", "Effect", "Level", "Aspect",
		},
		Note: "which of the five the Vn writes is decided by the Cn beside it",
	}, {
		Slot:       "VIII",
		Field:      "Cn",
		Name:       "Mood or Case-Scope",
		Categories: []string{"Mood", "CaseScope"},
		Note:       "the same consonant chooses between Mood and Case-Scope and names the Vn's category",
	}, {
		Slot:       "IX",
		Field:      "Vc",
		Name:       "Case, or Illocution and Validation",
		Categories: []string{"Case", "Illocution", "Validation"},
		Note:       "read as a Case under penultimate or antepenultimate stress, as illocution plus validation under ultimate or monosyllabic",
	}, {
		Slot:  "X",
		Field: "",
		Name:  "Relation",
		Note:  "carried by stress, not by any conjunct; it decides how Slot IX reads",
	}}
}
