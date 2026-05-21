package parse

import "github.com/christian-oudard/ithkuil/grammar"

// ShortcutVariant is a parse-time observation of which Ca-shortcut
// indicator the surface Cc cluster carried. The grammar itself does
// not store this — it's a rendering choice — but the parser needs
// to thread the info through so it can resolve Slot VI from the Vv
// series.
type ShortcutVariant int

const (
	ShortcutNone ShortcutVariant = iota
	ShortcutW
	ShortcutY
)

// CcResult bundles the two pieces of Slot I that Cc can carry.
//   - Concat marks the formative's role in a concatenation chain.
//   - Shortcut tells the caller which Ca-shortcut indicator (if any)
//     the surface Cc had; it does not appear in the grammar output.
type CcResult struct {
	Concat   grammar.ConcatenationStatus
	Shortcut ShortcutVariant
}

// ParseCc decodes a Slot I Cc consonant cluster. Some Cc forms carry
// both a concatenation marker and a shortcut (e.g. "hl" = Type1 + W).
func ParseCc(cc string) CcResult {
	var r CcResult
	switch cc {
	case "h":
		r.Concat = grammar.Type1
	case "hl":
		r.Concat = grammar.Type1
		r.Shortcut = ShortcutW
	case "hm":
		r.Concat = grammar.Type1
		r.Shortcut = ShortcutY
	case "hw":
		r.Concat = grammar.Type2
	case "hr":
		r.Concat = grammar.Type2
		r.Shortcut = ShortcutW
	case "hn":
		r.Concat = grammar.Type2
		r.Shortcut = ShortcutY
	case "w":
		r.Shortcut = ShortcutW
	case "y":
		r.Shortcut = ShortcutY
	}
	return r
}
