package surface

import (
	"strings"

	"golang.org/x/text/unicode/norm"
)

// variants folds spellings of a letter the alphabet already has.
//
// The glottal stop is an apostrophe, and almost nothing types a plain
// one. Phone keyboards, Discord, and word processors turn it into a
// typographic ’; ʼ is the same substitution made deliberately, being
// the character linguistics prefers; ‘ shows up when the autocorrect
// guesses the wrong side of a quotation. ț is t-with-comma, which most
// fonts draw identically to the t-with-cedilla ţ that v4 uses.
//
// Only letters v4 already has are folded. The pre-v4 alphabet had
// others — dotless ı, grave ì and ù, đ — and those stay unrecognised,
// because a word spelled with one is not v4 text and should fail
// rather than be quietly rewritten into something that parses.
//
// tools/discord_archive/words.py keeps its own copy of this table. It
// has to tokenize the raw archive before any of this code sees it, and
// an unrecognised character there splits a word in two rather than
// failing it.
var variants = strings.NewReplacer(
	"’", "'", // ’ right single quotation mark
	"‘", "'", // ‘ left single quotation mark
	"ʼ", "'", // ʼ modifier letter apostrophe
	"ț", "ţ", // ț t-comma
	"Ț", "Ţ",
)

// Normalize puts surface text in the form the rest of the stack expects:
// composed (Unicode NFC), lowercase, and with look-alike spellings of
// the alphabet's own letters folded onto it.
//
// All three are canonicalizations, not corrections. Ithkuil orthography
// is case-insensitive, so a capital is a sentence-position artifact with
// no grammatical content; "š" has two equally valid Unicode spellings
// (U+0161, or "s" plus U+030C), of which the tables here index only the
// composed one; and a typographic ’ is an apostrophe that a keyboard
// rewrote on the way in. Parse entry points call this so none of those
// distinctions ever reaches the lookup tables.
func Normalize(word string) string {
	return variants.Replace(norm.NFC.String(strings.ToLower(word)))
}
