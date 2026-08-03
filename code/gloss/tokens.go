package gloss

import "strings"

// A gloss line is written as one string because that is how it is read
// aloud and how it is typed back in. An interface wants it as pieces:
// every code in it names a grammatical value that has something to say
// about itself, and a reader should be able to ask.
//
// Splitting it is not a job for the front end. Joining is trivial and
// the syntax is not: which mark separates two slots, which binds a
// degree to its affix, which encloses a referential, and which
// characters are part of a cluster rather than between two of them.
// That knowledge lives here, beside the code that writes the line, and
// Tokens is how it leaves.
//
// The guarantee is exact: concatenating every token's Text reproduces
// the string it came from, byte for byte. tokens_test.go checks that
// over every sample in the inventory and every word of the corpus, so
// a caller can render tokens instead of the string with no risk of
// showing something the glosser did not write.

// Kind classifies a token for display. It is a judgment about how the
// piece is written, not about what it resolves to: Code means "written
// as a grammatical code", and whether one exists is the caller's
// question to ask. A code with no entry is ordinary, since only the
// values with something surprising about them carry a note.
type Kind string

const (
	// KindCode is an uppercase-initial atom: THM, S2, PRC, SYS, ULT.
	KindCode Kind = "code"
	// KindRoot is a lowercase atom: a root or affix consonant cluster,
	// which may contain a comma from the ASCII digraph notation, or a
	// lowercase structural marker such as the "parent" in {parent}.
	KindRoot Kind = "root"
	// KindDegree is an all-digit atom: an affix degree, or the Type
	// after an underscore.
	KindDegree Kind = "degree"
	// KindPunct is everything between the atoms, one rune at a time:
	// the slot separator "-", the intra-slot ".", the "/" binding a
	// degree or case to a head, "_" before an affix Type, "+" joining
	// referents, ":" tagging a stacked Ca, the brackets and braces, and
	// the space between the members of a concatenation chain.
	KindPunct Kind = "punct"
)

// Token is one piece of a gloss line.
type Token struct {
	Text string `json:"text"`
	Kind Kind   `json:"kind"`
}

// punct is the closed set of characters that are never part of an atom.
// Everything else belongs to the atom it sits in, the comma of the
// ASCII digraph notation included: nt,l is one cluster, not two
// separated by a comma.
const punct = "-./+_: ()[]{}"

// Tokens splits a gloss line into its pieces. Concatenating their Text
// in order reproduces the input exactly.
func Tokens(gloss string) []Token {
	var out []Token
	var atom strings.Builder
	flush := func() {
		if atom.Len() == 0 {
			return
		}
		out = append(out, Token{Text: atom.String(), Kind: classify(atom.String())})
		atom.Reset()
	}
	for _, r := range gloss {
		if strings.ContainsRune(punct, r) {
			flush()
			out = append(out, Token{Text: string(r), Kind: KindPunct})
			continue
		}
		atom.WriteRune(r)
	}
	flush()
	return out
}

// Join writes tokens back out. It is the inverse of Tokens, and exists
// so the one-line gloss and the pieces cannot disagree: the string a
// caller shows is the string these tokens make.
func Join(tokens []Token) string {
	var b strings.Builder
	for _, t := range tokens {
		b.WriteString(t.Text)
	}
	return b.String()
}

func classify(atom string) Kind {
	digits := true
	for _, r := range atom {
		if r < '0' || r > '9' {
			digits = false
			break
		}
	}
	if digits {
		return KindDegree
	}
	if r := []rune(atom)[0]; r >= 'A' && r <= 'Z' {
		return KindCode
	}
	return KindRoot
}
