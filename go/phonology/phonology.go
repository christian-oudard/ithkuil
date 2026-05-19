// Package phonology implements the Ithkuil V4 phoneme inventory:
// 9 vowels, 31 consonants, and the 4x9 vowel form table used to encode
// grammatical categories.
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

// VowelFormTable holds the 4 series x 9 forms used to encode grammatical
// categories (rows 0..3 are series 1..4; columns 0..8 are forms 1..9).
var VowelFormTable = [4][9]string{
	{"a", "ä", "e", "i", "ëi", "ö", "o", "ü", "u"},
	{"ai", "au", "ei", "eu", "ëu", "ou", "oi", "iu", "ui"},
	{"ia", "ie", "io", "iö", "eë", "uö", "uo", "ue", "ua"},
	{"ao", "aö", "eo", "eö", "oë", "öe", "oe", "öa", "oa"},
}

// series3Alternates lists the alternate Series-3 forms used after y-/w-
// glides. Keyed by form number (1-indexed).
var series3Alternates = map[int]string{
	1: "uä", 2: "uë", 3: "üä", 4: "üë",
	6: "öë", 7: "öä", 8: "ië", 9: "iä",
}

// VowelForm returns the vowel for a given series (1..4) and form (1..9).
// Out-of-range arguments panic — invalid grammatical indices are programmer
// errors, not parse-time failures.
func VowelForm(series, form int) string {
	return VowelFormTable[series-1][form-1]
}

// VowelFormLookup returns the (series, form) coordinates that produce v.
// Series 3 alternates resolve to series=3 with their canonical form number.
// ok is false if v is not a recognized vowel form.
func VowelFormLookup(v string) (series, form int, ok bool) {
	for s, row := range VowelFormTable {
		for f, vf := range row {
			if vf == v {
				return s + 1, f + 1, true
			}
		}
	}
	for f, alt := range series3Alternates {
		if alt == v {
			return 3, f, true
		}
	}
	return 0, 0, false
}
