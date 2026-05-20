// Package data embeds the V4 lexicon JSON so binaries built from this
// module ship with the lexicon by default. lexicon.LoadDefault reads
// these bytes; tooling that wants a different lexicon can still load
// from disk via lexicon.Load.
package data

import _ "embed"

//go:embed roots.json
var Roots []byte

//go:embed affixes.json
var Affixes []byte
