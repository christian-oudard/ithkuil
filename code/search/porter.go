package search

import "strings"

// Stem reduces an English word to its Porter stem, so that a search for
// "cats" finds a cat and one for "speaks" finds speech.
//
// This is the original Porter algorithm (1980), not Snowball English,
// and the choice is not stylistic: SQLite's FTS5 `porter` tokenizer
// implements original Porter, the store's index uses it, and the two
// halves of a search have to agree on what a word reduces to. A browser
// cannot have SQLite, so this is what stands in for it there, and
// store/stem_parity_test.go holds the two against each other over every
// word in the lexicon rather than trusting that they match.
//
// Words with no ASCII letters are returned unchanged. The lexicon is
// full of Latin binomials and Ithkuil clusters, and neither is English
// to be stemmed; passing them through leaves them findable by their
// exact form, which is how anyone looks them up.
func Stem(word string) string {
	w := strings.ToLower(word)
	if len(w) <= 2 || !isASCIIAlpha(w) {
		return w
	}
	b := []byte(w)
	b = step1a(b)
	b = step1b(b)
	b = step1c(b)
	b = step2(b)
	b = step3(b)
	b = step4(b)
	b = step5(b)
	return string(b)
}

func isASCIIAlpha(s string) bool {
	for i := 0; i < len(s); i++ {
		if s[i] < 'a' || s[i] > 'z' {
			return false
		}
	}
	return true
}

// isConsonant reports whether b[i] is a consonant. A y is a consonant
// when it follows a vowel and a vowel otherwise, which is why this has
// to look backwards rather than consult a set.
func isConsonant(b []byte, i int) bool {
	switch b[i] {
	case 'a', 'e', 'i', 'o', 'u':
		return false
	case 'y':
		return i == 0 || !isConsonant(b, i-1)
	}
	return true
}

// measure counts the vowel-consonant sequences in b, Porter's m: the
// number of VC pairs in [C](VC){m}[V].
func measure(b []byte) int {
	var m, i int
	for i < len(b) && isConsonant(b, i) {
		i++
	}
	for i < len(b) {
		for i < len(b) && !isConsonant(b, i) {
			i++
		}
		if i >= len(b) {
			break
		}
		m++
		for i < len(b) && isConsonant(b, i) {
			i++
		}
	}
	return m
}

// hasVowel is Porter's *v*.
func hasVowel(b []byte) bool {
	for i := range b {
		if !isConsonant(b, i) {
			return true
		}
	}
	return false
}

// doubleConsonant is Porter's *d: the word ends in two of the same
// consonant.
func doubleConsonant(b []byte) bool {
	return len(b) >= 2 && b[len(b)-1] == b[len(b)-2] && isConsonant(b, len(b)-1)
}

// cvc is Porter's *o: the word ends consonant-vowel-consonant where the
// last is not w, x or y. The condition exists to keep a silent e from
// being restored on words like "row".
func cvc(b []byte) bool {
	n := len(b)
	if n < 3 || !isConsonant(b, n-3) || isConsonant(b, n-2) || !isConsonant(b, n-1) {
		return false
	}
	switch b[n-1] {
	case 'w', 'x', 'y':
		return false
	}
	return true
}

// rule is one suffix and what replaces it, with the condition the stem
// left behind has to satisfy.
type rule struct {
	suffix, repl string
	ok           func([]byte) bool
}

// apply obeys at most one rule: the one with the longest matching
// suffix. If that rule's condition fails, nothing happens, and a
// shorter rule is *not* tried instead.
//
// That last part is the whole of Porter's "only one rule is obeyed, and
// this will be the one with the longest matching S1", and getting it
// wrong is quiet. Falling through from the (m>0) EED rule to the (*v*)
// ED rule turns "feed" into "fe": "eed" matches but "f" has measure 0,
// and "fe" does contain a vowel.
func apply(b []byte, rules []rule) ([]byte, bool) {
	best := -1
	for i, r := range rules {
		if !strings.HasSuffix(string(b), r.suffix) {
			continue
		}
		if best < 0 || len(r.suffix) > len(rules[best].suffix) {
			best = i
		}
	}
	if best < 0 {
		return b, false
	}
	r := rules[best]
	stem := b[:len(b)-len(r.suffix)]
	if r.ok != nil && !r.ok(stem) {
		return b, false
	}
	return append(append([]byte{}, stem...), r.repl...), true
}

func mGreater(n int) func([]byte) bool {
	return func(stem []byte) bool { return measure(stem) > n }
}

func step1a(b []byte) []byte {
	out, _ := apply(b, []rule{
		{"sses", "ss", nil}, {"ies", "i", nil}, {"ss", "ss", nil}, {"s", "", nil},
	})
	return out
}

func step1b(b []byte) []byte {
	// "eed" is the longest suffix in this step, so a word ending in it
	// can obey no other rule here: if its condition fails the step is
	// over, rather than falling through to "ed". That fall-through is
	// what turned "feed" into "fe", since "f" has measure 0 but "fe"
	// does contain a vowel. Its result needs no tidying either, so it
	// returns straight out.
	if strings.HasSuffix(string(b), "eed") {
		out, _ := apply(b, []rule{{"eed", "ee", mGreater(0)}})
		return out
	}
	out, fired := apply(b, []rule{
		{"ed", "", hasVowel},
		{"ing", "", hasVowel},
	})
	if !fired {
		return b
	}
	b = out
	// The second half of step 1b tidies the stem the removal exposed.
	if out, ok := apply(b, []rule{
		{"at", "ate", nil}, {"bl", "ble", nil}, {"iz", "ize", nil},
	}); ok {
		return out
	}
	if doubleConsonant(b) {
		switch b[len(b)-1] {
		case 'l', 's', 'z':
		default:
			return b[:len(b)-1]
		}
		return b
	}
	if measure(b) == 1 && cvc(b) {
		return append(b, 'e')
	}
	return b
}

func step1c(b []byte) []byte {
	out, _ := apply(b, []rule{{"y", "i", hasVowel}})
	return out
}

// step2Rules and the tables below are Porter's suffix lists, in the
// order the algorithm applies them. Order matters: "ational" has to be
// tried before "tional", or the longer suffix is never seen.
var step2Rules = [][2]string{
	{"ational", "ate"}, {"tional", "tion"}, {"enci", "ence"}, {"anci", "ance"},
	{"izer", "ize"}, {"bli", "ble"}, {"alli", "al"}, {"entli", "ent"},
	{"eli", "e"}, {"ousli", "ous"}, {"ization", "ize"}, {"ation", "ate"},
	{"ator", "ate"}, {"alism", "al"}, {"iveness", "ive"}, {"fulness", "ful"},
	{"ousness", "ous"}, {"aliti", "al"}, {"iviti", "ive"}, {"biliti", "ble"},
	{"logi", "log"},
}

var step3Rules = [][2]string{
	{"icate", "ic"}, {"ative", ""}, {"alize", "al"}, {"iciti", "ic"},
	{"ical", "ic"}, {"ful", ""}, {"ness", ""},
}

var step4Suffixes = []string{
	"al", "ance", "ence", "er", "ic", "able", "ible", "ant", "ement",
	"ment", "ent", "ou", "ism", "ate", "iti", "ous", "ive", "ize",
}

func withCondition(pairs [][2]string, ok func([]byte) bool) []rule {
	out := make([]rule, len(pairs))
	for i, p := range pairs {
		out[i] = rule{p[0], p[1], ok}
	}
	return out
}

func step2(b []byte) []byte {
	out, _ := apply(b, withCondition(step2Rules, mGreater(0)))
	return out
}

func step3(b []byte) []byte {
	out, _ := apply(b, withCondition(step3Rules, mGreater(0)))
	return out
}

func step4(b []byte) []byte {
	rules := make([]rule, 0, len(step4Suffixes)+1)
	for _, suffix := range step4Suffixes {
		rules = append(rules, rule{suffix, "", mGreater(1)})
	}
	// "ion" only goes when what precedes it is s or t.
	rules = append(rules, rule{"ion", "", func(stem []byte) bool {
		return measure(stem) > 1 && len(stem) > 0 &&
			(stem[len(stem)-1] == 's' || stem[len(stem)-1] == 't')
	}})
	out, _ := apply(b, rules)
	return out
}

func step5(b []byte) []byte {
	if strings.HasSuffix(string(b), "e") {
		stem := b[:len(b)-1]
		if m := measure(stem); m > 1 || (m == 1 && !cvc(stem)) {
			b = stem
		}
	}
	if measure(b) > 1 && doubleConsonant(b) && b[len(b)-1] == 'l' {
		b = b[:len(b)-1]
	}
	return b
}
