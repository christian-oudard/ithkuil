package grammar

// ConcatenationStatus marks a formative as part of a Type 1 or Type 2
// compound. Absent on stand-alone formatives.
type ConcatenationStatus int

const (
	Type1 ConcatenationStatus = iota
	Type2
)

func (c ConcatenationStatus) String() string {
	return [...]string{"Type1", "Type2"}[c]
}
