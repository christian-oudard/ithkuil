// Package data embeds the V4 lexicon JSON so binaries built from this
// module ship with the lexicon by default. lexicon.LoadDefault reads
// these bytes; tooling that wants a different lexicon can still load
// from disk via lexicon.Load.
//
// The lexicon is a single file with shape:
//
//	{
//	    "version": "<short content hash>",
//	    "roots":   [...],
//	    "affixes": [...]
//	}
//
// The version is a content-derived hash so any change to the roots
// or affixes (via sync_lexicon.py) bumps the version automatically.
// The binary serialization format includes this version in its
// sentence header so the decoder can detect mismatches.
package data

import _ "embed"

//go:embed lexicon.json
var Lexicon []byte
