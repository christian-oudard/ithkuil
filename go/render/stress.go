package render

import (
	"strings"
	"unicode/utf8"

	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/parse"
)

// stressMap maps a plain vowel to its stressed counterpart. Acute is
// used on plain vowels; circumflex is used on umlauted vowels because
// the diaeresis slot is already occupied.
var stressMap = map[rune]rune{
	'a': 'á', 'e': 'é', 'i': 'í', 'o': 'ó', 'u': 'ú',
	'ä': 'â', 'ë': 'ê', 'ö': 'ô', 'ü': 'û',
}

// applyFinalStress places a stress diacritic on the appropriate vowel
// of word, driven by the Formative's Final variant (§1.3.1):
//   - UnframedNominal: no diacritic (penultimate is the orthographic
//     default).
//   - UnframedVerbal: acute on the last vowel-conjunct. A monosyllabic
//     word is left unmarked — monosyllabic is implicit ultimate per
//     §3.10.
//   - FramedVerbal: acute on the third-from-last vowel-conjunct.
//
// Inside a multi-vowel conjunct (diphthong, hiatus pair), the diacritic
// goes on the first vowel — the prominent member of a falling diphthong.
//
// If the word has fewer vowel-conjuncts than the requested stress
// position needs, the original string is returned unchanged.
func applyFinalStress(word string, f g.Final) string {
	conjs := parse.SplitConjuncts(word)
	var vowelIdx []int
	for i, c := range conjs {
		if c == "" {
			continue
		}
		r, _ := utf8.DecodeRuneInString(c)
		if parse.IsVowelChar(r) {
			vowelIdx = append(vowelIdx, i)
		}
	}
	n := len(vowelIdx)
	if n == 0 {
		return word
	}
	var target int
	switch f.(type) {
	case g.UnframedVerbal:
		if n < 2 {
			return word
		}
		target = vowelIdx[n-1]
	case g.FramedVerbal:
		if n < 3 {
			return word
		}
		target = vowelIdx[n-3]
	default:
		return word
	}
	conjs[target] = markFirstVowel(conjs[target])
	return strings.Join(conjs, "")
}

// markFirstVowel applies the stress diacritic to the first markable
// vowel in s. Non-vowel runes pass through unchanged.
func markFirstVowel(s string) string {
	var b strings.Builder
	marked := false
	for _, r := range s {
		if !marked {
			if rep, ok := stressMap[r]; ok {
				b.WriteRune(rep)
				marked = true
				continue
			}
		}
		b.WriteRune(r)
	}
	return b.String()
}
