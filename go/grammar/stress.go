package grammar

// Stress is the syllable on which a word is stressed. The four values
// distinguish formative roles: penultimate is the default for nouns;
// ultimate marks verbs; antepenultimate marks framed verbs;
// monosyllabic stands in for unmarked single-syllable words.
type Stress int

const (
	Monosyllabic Stress = iota
	Penultimate
	Ultimate
	Antepenultimate
)

func (s Stress) String() string {
	return [...]string{"Monosyllabic", "Penultimate", "Ultimate", "Antepenultimate"}[s]
}
