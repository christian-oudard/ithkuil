package surface

import (
	"strings"

	"golang.org/x/text/unicode/norm"
)

// Normalize puts surface text in the form the rest of the stack expects:
// composed (Unicode NFC) and lowercase.
//
// Both are canonicalizations, not corrections. Ithkuil orthography is
// case-insensitive, so a capital is a sentence-position artifact with no
// grammatical content; and "š" has two equally valid Unicode spellings
// (U+0161, or "s" plus U+030C), of which the tables here index only the
// composed one. Parse entry points call this so neither distinction ever
// reaches the lookup tables.
func Normalize(word string) string {
	return norm.NFC.String(strings.ToLower(word))
}
