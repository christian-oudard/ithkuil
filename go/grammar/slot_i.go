package grammar

// ConcatenationStatus marks a formative as part of a Type 1 or Type 2
// compound. Stored in Slot I; absent on stand-alone formatives.
type ConcatenationStatus int

const (
	Type1 ConcatenationStatus = iota
	Type2
)

func (c ConcatenationStatus) String() string {
	return [...]string{"Type1", "Type2"}[c]
}

// CcShortcut is the Slot I "shortcut" marker that lets a formative
// elide its Ca complex (Slot VI) by substituting a fixed Ca encoded
// jointly with the Vv series.
type CcShortcut int

const (
	ShortcutW CcShortcut = iota
	ShortcutY
)

func (s CcShortcut) String() string {
	return [...]string{"ShortcutW", "ShortcutY"}[s]
}

// Root is the consonant cluster that identifies a formative's lexical
// root (Slot III, Cr). The string is the surface consonant cluster
// as it appears between Vv and Vr.
type Root string

func (r Root) String() string { return string(r) }
