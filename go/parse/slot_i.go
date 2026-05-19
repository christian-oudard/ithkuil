package parse

import "github.com/coudard/ithkuil/go/grammar"

// CcResult bundles the two pieces of Slot I that Cc can carry. Either,
// both, or neither may be present:
//   - Concat marks the formative as part of a Type 1 or Type 2 compound.
//   - Shortcut lets the formative elide Slot VI Ca by carrying it
//     compositionally in the Cc/Vv pair.
//
// Both pointers are nil if the corresponding feature is absent.
type CcResult struct {
	Concat   *grammar.ConcatenationStatus
	Shortcut *grammar.CcShortcut
}

// ParseCc decodes a Slot I Cc consonant cluster. Some Cc forms carry
// both a concatenation marker and a shortcut (e.g. "hl" = Type1 + W).
func ParseCc(cc string) CcResult {
	var r CcResult
	switch cc {
	case "h":
		t := grammar.Type1
		r.Concat = &t
	case "hl":
		t := grammar.Type1
		s := grammar.ShortcutW
		r.Concat = &t
		r.Shortcut = &s
	case "hm":
		t := grammar.Type1
		s := grammar.ShortcutY
		r.Concat = &t
		r.Shortcut = &s
	case "hw":
		t := grammar.Type2
		r.Concat = &t
	case "hr":
		t := grammar.Type2
		s := grammar.ShortcutW
		r.Concat = &t
		r.Shortcut = &s
	case "hn":
		t := grammar.Type2
		s := grammar.ShortcutY
		r.Concat = &t
		r.Shortcut = &s
	case "w":
		s := grammar.ShortcutW
		r.Shortcut = &s
	case "y":
		s := grammar.ShortcutY
		r.Shortcut = &s
	}
	return r
}
