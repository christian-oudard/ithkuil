package phonology

import (
	"fmt"
	"strings"
)

// The ASCII input method maps two-character ASCII digraphs to the
// Unicode diacritics used by the Ithkuil orthography, plus a postfix
// `/` that stresses the preceding vowel. FromASCII and ToASCII are
// inverses on full orthographic text including stress.
//
//	FromASCII(ToASCII(w)) == w
//	ToASCII(FromASCII(a)) == a   (on the digraph-free output of FromASCII)
//
// The glottal stop `'` is not part of any digraph; it passes through
// both directions verbatim.
//
// For an interactive TUI, InputState exposes the same state machine
// in streaming form with separate views of committed and pending
// (the dimmed, still-extendable tail).

// asciiDigraphs maps each two-character ASCII trigger to its Unicode
// grapheme. The first character of every key is also listed in
// asciiStarter or asciiVowel.
var asciiDigraphs = map[string]string{
	"aa": "ä", "ee": "ë", "oo": "ö", "uu": "ü",
	"t,": "ţ", "d,": "ḑ", "l,": "ļ", "c,": "ç",
	"sq": "š", "zq": "ž", "cq": "č", "nq": "ň", "rq": "ř",
	"dz": "ẓ",
}

// asciiInverse is the unicode → digraph reverse table. Plain umlaut
// and consonant digraphs come from asciiDigraphs; stressed forms
// append `/` to the plain/umlaut spelling.
var asciiInverse = buildAsciiInverse()

func buildAsciiInverse() map[rune]string {
	out := make(map[rune]string, len(asciiDigraphs)+9)
	for k, v := range asciiDigraphs {
		r := []rune(v)[0]
		out[r] = k
	}
	out['á'] = "a/"
	out['é'] = "e/"
	out['í'] = "i/"
	out['ó'] = "o/"
	out['ú'] = "u/"
	out['â'] = "aa/"
	out['ê'] = "ee/"
	out['ô'] = "oo/"
	out['û'] = "uu/"
	return out
}

// asciiVowel reports whether r is a vowel that has an umlaut form
// (a, e, o, u). The fifth ASCII vowel `i` has no umlaut and so no
// doubling rule.
func asciiVowel(r rune) bool {
	switch r {
	case 'a', 'e', 'o', 'u':
		return true
	}
	return false
}

// asciiStarter reports whether r is the leading character of any
// non-vowel digraph. After typing a starter the input method waits
// for one more character before deciding whether a digraph fired.
func asciiStarter(r rune) bool {
	switch r {
	case 't', 'd', 'l', 'c', 's', 'z', 'n', 'r':
		return true
	}
	return false
}

// FromASCII applies the input method to a complete ASCII string,
// committing any trailing pending vowel run or unresolved starter.
//
// Vowel runs collapse with right-grouping:
//
//	"e"    → "e"
//	"ee"   → "ë"
//	"eee"  → "eë"
//	"eeee" → "ëë"
//
// Non-vowel digraphs commit as soon as the second character arrives:
//
//	"t,"   → "ţ"
//	"cq"   → "č"
//	"dz"   → "ẓ"
//
// Any input character that isn't a vowel or starter is appended
// verbatim.
func FromASCII(ascii string) string {
	var s InputState
	for _, r := range ascii {
		s.Feed(r)
	}
	return s.Commit()
}

// ToASCII produces the keystroke sequence that, fed through
// FromASCII, reproduces s. ASCII runes pass through unchanged.
// Any non-ASCII rune that has no digraph mapping is a programmer
// error — ToASCII panics rather than silently emit non-ASCII output.
func ToASCII(s string) string {
	var b strings.Builder
	b.Grow(len(s))
	for _, r := range s {
		if rep, ok := asciiInverse[r]; ok {
			b.WriteString(rep)
		} else if r < 128 {
			b.WriteRune(r)
		} else {
			panic(fmt.Sprintf("ToASCII: no mapping for %q in %q", r, s))
		}
	}
	return b.String()
}

// InputState is the streaming state machine for the ASCII input
// method. The TUI feeds keystrokes one at a time and renders
// Committed text solid plus Pending text dimmed; the pending tail
// is what could still change with one more keystroke (vowel runs
// extend, single starters resolve into a digraph).
type InputState struct {
	committed []rune
	pending   []rune
}

// Feed processes one keystroke.
func (s *InputState) Feed(r rune) {
	if r == '/' {
		s.applyStress()
		return
	}
	if len(s.pending) == 0 {
		s.start(r)
		return
	}
	if asciiVowel(s.pending[0]) {
		if r == s.pending[0] {
			s.pending = append(s.pending, r)
			return
		}
		s.commitPending()
		s.start(r)
		return
	}
	combo := string(s.pending[0]) + string(r)
	if unicode, ok := asciiDigraphs[combo]; ok {
		s.committed = append(s.committed, []rune(unicode)...)
		s.pending = s.pending[:0]
		return
	}
	s.commitPending()
	s.start(r)
}

// applyStress closes the pending buffer and promotes the last committed
// vowel to its stressed form. If no vowel precedes, the `/` is emitted
// verbatim.
func (s *InputState) applyStress() {
	s.commitPending()
	if len(s.committed) == 0 {
		s.committed = append(s.committed, '/')
		return
	}
	last := s.committed[len(s.committed)-1]
	stressed, ok := applyMap[last]
	if !ok {
		s.committed = append(s.committed, '/')
		return
	}
	s.committed[len(s.committed)-1] = stressed
}

func (s *InputState) start(r rune) {
	if asciiVowel(r) || asciiStarter(r) {
		s.pending = append(s.pending, r)
		return
	}
	s.committed = append(s.committed, r)
}

// Backspace removes one displayed character: a pending keystroke if
// one is buffered, otherwise the last committed rune.
func (s *InputState) Backspace() {
	if len(s.pending) > 0 {
		s.pending = s.pending[:len(s.pending)-1]
		return
	}
	if len(s.committed) > 0 {
		s.committed = s.committed[:len(s.committed)-1]
	}
}

// Reset clears all state.
func (s *InputState) Reset() {
	s.committed = s.committed[:0]
	s.pending = s.pending[:0]
}

// Committed returns the part of the input that has fully resolved.
func (s *InputState) Committed() string {
	return string(s.committed)
}

// Pending returns the rendered form of the pending tail. Vowel runs
// apply right-grouping; single starters render as themselves.
func (s *InputState) Pending() string {
	return renderAsciiPending(s.pending)
}

// Display returns Committed + Pending — the full visible text.
func (s *InputState) Display() string {
	return s.Committed() + s.Pending()
}

// Commit force-commits any pending text and returns the full
// committed string. The state is left ready to accept more input.
func (s *InputState) Commit() string {
	s.commitPending()
	return s.Committed()
}

func (s *InputState) commitPending() {
	s.committed = append(s.committed, []rune(renderAsciiPending(s.pending))...)
	s.pending = s.pending[:0]
}

// renderAsciiPending produces the visible form of the pending
// buffer. Vowel runs apply right-grouping; single starters render
// as themselves.
func renderAsciiPending(pending []rune) string {
	if len(pending) == 0 {
		return ""
	}
	if asciiVowel(pending[0]) {
		v := pending[0]
		umlaut := asciiDigraphs[string(v)+string(v)]
		n := len(pending)
		return strings.Repeat(string(v), n%2) + strings.Repeat(umlaut, n/2)
	}
	return string(pending)
}
