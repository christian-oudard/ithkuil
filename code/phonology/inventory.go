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

// Place is where the primary constriction is made, ordered front to
// back so that the gap between two values approximates how far the
// articulators travel between them. Anything measuring articulatory
// effort reads it that way, and TestPlaceIsOrderedFrontToBack holds
// the ordering.
//
// The names and the order are §1.1's column headings, read off the
// PDF rather than off our transcription of it: the markdown table had
// several rows shifted a column left. Lateral is a column there too,
// but it is a manner rather than a place, so l and ļ take their
// position here (apico-alveolar) and their laterality from Manner.
type Place int

const (
	Labial            Place = iota // p b m
	LabioDental                    // f v
	ApicoDental                    // t d n
	InterDental                    // ţ ḑ
	ApicoAlveolar                  // s z c ẓ, and l ļ
	AlveolarRetroflex              // r
	AlveoloPalatal                 // š ž č j
	Palatal                        // ç y
	Velar                          // k g ň
	Uvular                         // ř, and x
	Glottal                        // ' h
)

// Secondary is a second, simultaneous constriction, made with a
// different organ from the primary one. §1.1 lists a Labio-velar
// column holding only w, which is a tongue-body approximant said with
// rounded lips rather than a labial consonant.
//
// Place records where the tongue is, so w is Velar and its lip
// rounding is recorded here. Filing it under Labial instead would put
// it as far from u as it is from i, when in articulation it is
// adjacent to u.
type Secondary int

const (
	// Plain is a single constriction, which is every consonant but w.
	Plain Secondary = iota
	Labialized
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

// Consonant is a phoneme described by where and how it is made.
//
// Place is ordered front to back, Labial through Glottal, so the gap
// between two values approximates how far the articulators travel
// between them. Anything measuring articulatory effort reads it that
// way, and TestPlaceIsOrderedFrontToBack holds the ordering.
type Consonant struct {
	Voicing   Voicing
	Place     Place
	Manner    Manner
	Secondary Secondary
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
	{Consonant{Voiceless, Labial, Stop, Plain}, 'p', "p"},
	{Consonant{Voiced, Labial, Stop, Plain}, 'b', "b"},
	{Consonant{Voiceless, ApicoDental, Stop, Plain}, 't', "t"},
	{Consonant{Voiced, ApicoDental, Stop, Plain}, 'd', "d"},
	{Consonant{Voiceless, Velar, Stop, Plain}, 'k', "k"},
	{Consonant{Voiced, Velar, Stop, Plain}, 'g', "g"},
	{Consonant{Voiceless, Glottal, Stop, Plain}, '\'', "'"},
	{Consonant{Voiceless, LabioDental, Fricative, Plain}, 'f', "f"},
	{Consonant{Voiced, LabioDental, Fricative, Plain}, 'v', "v"},
	{Consonant{Voiceless, InterDental, Fricative, Plain}, 'T', "ţ"},
	{Consonant{Voiced, InterDental, Fricative, Plain}, 'D', "ḑ"},
	{Consonant{Voiceless, ApicoAlveolar, Fricative, Plain}, 's', "s"},
	{Consonant{Voiced, ApicoAlveolar, Fricative, Plain}, 'z', "z"},
	{Consonant{Voiceless, AlveoloPalatal, Fricative, Plain}, 'S', "š"},
	{Consonant{Voiced, AlveoloPalatal, Fricative, Plain}, 'Z', "ž"},
	{Consonant{Voiceless, Palatal, Fricative, Plain}, 'c', "ç"},
	{Consonant{Voiceless, Uvular, Fricative, Plain}, 'x', "x"},
	{Consonant{Voiceless, Glottal, Fricative, Plain}, 'h', "h"},
	{Consonant{Voiceless, ApicoAlveolar, Affricate, Plain}, 'C', "c"},
	{Consonant{Voiced, ApicoAlveolar, Affricate, Plain}, 'J', "ẓ"},
	{Consonant{Voiceless, AlveoloPalatal, Affricate, Plain}, 'X', "č"},
	{Consonant{Voiced, AlveoloPalatal, Affricate, Plain}, 'j', "j"},
	{Consonant{Voiced, Labial, Nasal, Plain}, 'm', "m"},
	{Consonant{Voiced, ApicoDental, Nasal, Plain}, 'n', "n"},
	{Consonant{Voiced, Velar, Nasal, Plain}, 'N', "ň"},
	{Consonant{Voiced, AlveolarRetroflex, Tap, Plain}, 'r', "r"},
	// §1.2.2: "The uvular -ř- is an approximant [ʁ] as in colloquial
	// French or German; when geminated it is either [ʁː] or can be
	// strengthened to a uvular trill [ʀ]." §1.1 lists it on the
	// Approximant row. The trill is the geminate allophone, not the
	// phoneme.
	{Consonant{Voiced, Uvular, Approximant, Plain}, 'R', "ř"},
	{Consonant{Voiced, ApicoAlveolar, LateralApprox, Plain}, 'l', "l"},
	{Consonant{Voiced, Palatal, Approximant, Plain}, 'y', "y"},
	// §1.1 files w under Labio-velar, not Labial. See Secondary.
	{Consonant{Voiced, Velar, Approximant, Labialized}, 'w', "w"},
	{Consonant{Voiceless, ApicoAlveolar, LateralFric, Plain}, 'L', "ļ"},
}

// Vowels lists the 9 vowels of Ithkuil V4, in §1.1's own rows and
// columns, read off the PDF. §1.2.1 corroborates the two that had been
// recorded backwards: "-ä- is pronounced [æ]", which is front, and
// "-a- is pronounced [a] or [ɑ]", which is back.
var Vowels = []PhonemeEntry{
	{Vowel{High, Front, Unrounded}, 'i', "i"},
	{Vowel{High, Central, Rounded}, 'I', "ü"},
	{Vowel{High, Back, Rounded}, 'u', "u"},
	{Vowel{Mid, Front, Unrounded}, 'e', "e"},
	{Vowel{Mid, Front, Rounded}, 'O', "ö"},
	{Vowel{Mid, Back, Unrounded}, 'E', "ë"},
	{Vowel{Mid, Back, Rounded}, 'o', "o"},
	{Vowel{Low, Front, Unrounded}, 'A', "ä"},
	{Vowel{Low, Back, Unrounded}, 'a', "a"},
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
