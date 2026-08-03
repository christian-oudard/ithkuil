package grammar

// ConcatenationStatus marks a formative's role in a concatenation
// chain (§3.1.7). Standalone formatives and the parent of a chain use
// ConcatNone; dependents use Type1 or Type2.
type ConcatenationStatus int

const (
	ConcatNone ConcatenationStatus = iota
	Type1
	Type2
)

func (c ConcatenationStatus) String() string {
	return enumName(c, "ConcatenationStatus", "None", "Type1", "Type2")
}
