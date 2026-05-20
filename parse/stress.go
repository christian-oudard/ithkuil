package parse

// Stress is an orthographic observation about a surface word: where
// the acute/circumflex diacritic falls (or its absence).
//
// This type and its constants mirror surface.Stress (Layer A of the
// parse stack). They live here because fullparse and the slot-level
// parsers still pass Stress through their signatures; new code should
// prefer surface.Strip / surface.Stress directly.
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

// stressedVowels are the acute (á é í ó ú) and circumflex (â ê ô û)
// forms — these mark the stressed syllable. The set is asymmetric
// because circumflex doubles as the umlaut form and "î" isn't used:
// "i" pairs with the diaeresis hiatus marker "ï" instead.
var stressedVowels = map[rune]bool{
	'á': true, 'é': true, 'í': true, 'ó': true, 'ú': true,
	'â': true, 'ê': true, 'ô': true, 'û': true,
}

// IsStressedVowel reports whether a rune carries a stress mark.
func IsStressedVowel(r rune) bool { return stressedVowels[r] }
