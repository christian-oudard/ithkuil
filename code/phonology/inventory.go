// Package phonology covers §1 of the grammar: what the sounds of
// Ithkuil are, and what the letters that write them do. The two are one
// subject here because the romanization is one-to-one with the
// phonemes.
//
// It holds the phoneme inventory (9 vowels, 31 consonants, and the 4x9
// vowel-form table that encodes grammatical categories) in
// inventory.go, and the rune-level work on written words everywhere
// else: normalization, splitting a word into vowel and consonant
// conjuncts, reading and writing the stress diacritic, and the ASCII
// digraph notation.
//
// Nothing here knows any grammar. It is letters, accents, and syllable
// counts only, which is what lets every other package rest on it.
package phonology

type Voicing int

const (
	Voiced Voicing = iota
	Voiceless
)

type Place int

const (
	Labial Place = iota
	LabioDental
	Dental
	Alveolar
	PostAlveolar
	Retroflex
	Palatal
	Velar
	Uvular
	Glottal
)

type Manner int

const (
	Stop Manner = iota
	Fricative
	Affricate
	Nasal
	Tap
	Trill
	Approximant
	LateralApprox
	LateralFric
)

type Height int

const (
	High Height = iota
	Mid
	Low
)

type Backness int

const (
	Front Backness = iota
	Central
	Back
)

type Rounding int

const (
	Rounded Rounding = iota
	Unrounded
)

// Phoneme is the sum type {Consonant | Vowel}. Implementations are Consonant
// and Vowel; the unexported phoneme() method seals the interface.
type Phoneme interface {
	phoneme()
}

type Consonant struct {
	Voicing Voicing
	Place   Place
	Manner  Manner
}

func (Consonant) phoneme() {}

type Vowel struct {
	Height   Height
	Backness Backness
	Rounding Rounding
}

func (Vowel) phoneme() {}

// PhonemeEntry pairs a phoneme with its ASCII shorthand and its Unicode
// orthographic form.
type PhonemeEntry struct {
	Phoneme Phoneme
	ASCII   rune
	Text    string
}

// Consonants lists the 31 consonants of Ithkuil V4.
var Consonants = []PhonemeEntry{
	{Consonant{Voiceless, Labial, Stop}, 'p', "p"},
	{Consonant{Voiced, Labial, Stop}, 'b', "b"},
	{Consonant{Voiceless, Alveolar, Stop}, 't', "t"},
	{Consonant{Voiced, Alveolar, Stop}, 'd', "d"},
	{Consonant{Voiceless, Velar, Stop}, 'k', "k"},
	{Consonant{Voiced, Velar, Stop}, 'g', "g"},
	{Consonant{Voiceless, Glottal, Stop}, '\'', "'"},
	{Consonant{Voiceless, LabioDental, Fricative}, 'f', "f"},
	{Consonant{Voiced, LabioDental, Fricative}, 'v', "v"},
	{Consonant{Voiceless, Dental, Fricative}, 'T', "ţ"},
	{Consonant{Voiced, Dental, Fricative}, 'D', "ḑ"},
	{Consonant{Voiceless, Alveolar, Fricative}, 's', "s"},
	{Consonant{Voiced, Alveolar, Fricative}, 'z', "z"},
	{Consonant{Voiceless, PostAlveolar, Fricative}, 'S', "š"},
	{Consonant{Voiced, PostAlveolar, Fricative}, 'Z', "ž"},
	{Consonant{Voiceless, Palatal, Fricative}, 'c', "ç"},
	{Consonant{Voiceless, Uvular, Fricative}, 'x', "x"},
	{Consonant{Voiceless, Glottal, Fricative}, 'h', "h"},
	{Consonant{Voiceless, Alveolar, Affricate}, 'C', "c"},
	{Consonant{Voiced, Alveolar, Affricate}, 'J', "ẓ"},
	{Consonant{Voiceless, PostAlveolar, Affricate}, 'X', "č"},
	{Consonant{Voiced, PostAlveolar, Affricate}, 'j', "j"},
	{Consonant{Voiced, Labial, Nasal}, 'm', "m"},
	{Consonant{Voiced, Alveolar, Nasal}, 'n', "n"},
	{Consonant{Voiced, Velar, Nasal}, 'N', "ň"},
	{Consonant{Voiced, Alveolar, Tap}, 'r', "r"},
	{Consonant{Voiced, Uvular, Trill}, 'R', "ř"},
	{Consonant{Voiced, Alveolar, LateralApprox}, 'l', "l"},
	{Consonant{Voiced, Palatal, Approximant}, 'y', "y"},
	{Consonant{Voiced, Labial, Approximant}, 'w', "w"},
	{Consonant{Voiceless, Alveolar, LateralFric}, 'L', "ļ"},
}

// Vowels lists the 9 vowels of Ithkuil V4.
var Vowels = []PhonemeEntry{
	{Vowel{High, Front, Unrounded}, 'i', "i"},
	{Vowel{High, Central, Unrounded}, 'I', "ü"},
	{Vowel{High, Back, Rounded}, 'u', "u"},
	{Vowel{Mid, Front, Unrounded}, 'e', "e"},
	{Vowel{Mid, Central, Unrounded}, 'E', "ë"},
	{Vowel{Mid, Back, Rounded}, 'o', "o"},
	{Vowel{Mid, Front, Rounded}, 'O', "ö"},
	{Vowel{Low, Central, Unrounded}, 'a', "a"},
	{Vowel{Low, Back, Unrounded}, 'A', "ä"},
}

// VowelFormTable holds §1.6's 4 series x 9 forms, the pattern that
// populates Slots II, IV, V/VII, VIII and IX (rows 0..3 are series 1..4,
// columns 0..8 are forms 1..9).
//
// Series 3 is printed with two spellings in a cell — the source writes
// "ia / uä" — and they are one form written two ways, not two values.
// Which one is written is fixed, not free: see VowelFormAfterGlide.
// Cells with only one spelling leave the second entry empty, which is
// every cell outside Series 3 and Series 3's own form 5.
var VowelFormTable = [4][9][2]string{
	{{"a"}, {"ä"}, {"e"}, {"i"}, {"ëi"}, {"ö"}, {"o"}, {"ü"}, {"u"}},
	{{"ai"}, {"au"}, {"ei"}, {"eu"}, {"ëu"}, {"ou"}, {"oi"}, {"iu"}, {"ui"}},
	{
		{"ia", "uä"}, {"ie", "uë"}, {"io", "üä"}, {"iö", "üë"}, {"eë"},
		{"uö", "öë"}, {"uo", "öä"}, {"ue", "ië"}, {"ua", "iä"},
	},
	{{"ao"}, {"aö"}, {"eo"}, {"eö"}, {"oë"}, {"öe"}, {"oe"}, {"öa"}, {"oa"}},
}

// VowelForm returns the primary spelling of a given series (1..4) and
// form (1..9). Out-of-range arguments panic — invalid grammatical
// indices are programmer errors, not parse-time failures.
//
// For Series 3 this is the spelling printed on the left of the cell. It
// is the right one everywhere except after a glide, where
// VowelFormAfterGlide applies.
func VowelForm(series, form int) string {
	return VowelFormTable[series-1][form-1][0]
}

// VowelFormAlternate returns the second spelling of a vowel-form, or ""
// where the form has only one.
func VowelFormAlternate(series, form int) string {
	return VowelFormTable[series-1][form-1][1]
}

// VowelFormAfterGlide returns the spelling of a Series-3 vowel-form to
// write after the consonant prev, applying §1.6's footnote:
//
//	When preceded by y-, Series 3 forms beginning with -i use their
//	alternate forms instead (e.g., yuä, not yia), while Series 3 forms
//	beginning with -u use their alternate forms if preceded by w-
//	(e.g., wiä, not wua).
//
// It is dissimilation: a glide is not written before the vowel that
// matches it. Anything with no alternate, or already dissimilated, or
// not after a glide, comes back unchanged.
func VowelFormAfterGlide(prev rune, v string) string {
	series, form, ok := VowelFormLookup(v)
	if !ok || series != 3 {
		return v
	}
	alt := VowelFormAlternate(series, form)
	if alt == "" {
		return v
	}
	first := []rune(v)[0]
	if (prev == 'y' && first == 'i') || (prev == 'w' && first == 'u') {
		return alt
	}
	return v
}

// VowelFormLookup returns the (series, form) coordinates that produce v,
// accepting either spelling of a two-spelling cell. ok is false if v is
// not a recognized vowel form.
func VowelFormLookup(v string) (series, form int, ok bool) {
	for s, row := range VowelFormTable {
		for f, cell := range row {
			if cell[0] == v || (cell[1] != "" && cell[1] == v) {
				return s + 1, f + 1, true
			}
		}
	}
	return 0, 0, false
}
