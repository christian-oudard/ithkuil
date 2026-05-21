// Package serialize provides a compact binary codec for Ithkuil
// grammatical words — the same data the gloss canonical format
// represents in text, encoded as bytes.
//
// Layout principles:
//
//   - One byte per phoneme. Cr roots, Cs roots, affix Cs, and
//     referential C1 clusters all encode as a 1-byte length prefix
//     followed by N phoneme bytes (max 6 from phonotactics).
//   - One byte per enum value. All grammatical enums (Case, Aspect,
//     Configuration, etc.) fit in 1 byte with substantial headroom.
//   - Sum types use a leading variant-tag byte; the payload depends
//     on the variant.
//   - Tokens are prefixed with a one-byte type tag (TokenFormative,
//     TokenBias, etc.) so the decoder can dispatch.
//
// No bit-packing inside bytes — gzip/zstd handles whatever bytes
// remain compressible.
package serialize

import (
	"fmt"

	"github.com/christian-oudard/ithkuil/phonology"
)

// phonemeTable indexes every legal Ithkuil phoneme to a byte value:
// consonants[0..30] occupy bytes 0..30; vowels[0..8] occupy 31..39.
// The order matches phonology.Consonants / phonology.Vowels and is
// part of the on-wire contract — do not reorder without bumping a
// format-version byte.
var (
	phonemeToByte map[string]byte
	byteToPhoneme [40]string
)

func init() {
	phonemeToByte = make(map[string]byte, 40)
	for i, p := range phonology.Consonants {
		phonemeToByte[p.Text] = byte(i)
		byteToPhoneme[i] = p.Text
	}
	off := len(phonology.Consonants)
	for i, p := range phonology.Vowels {
		phonemeToByte[p.Text] = byte(off + i)
		byteToPhoneme[off+i] = p.Text
	}
}

// EncodePhoneme returns the byte value for a single-phoneme string
// (one consonant or one vowel as written in canonical orthography).
func EncodePhoneme(p string) (byte, error) {
	b, ok := phonemeToByte[p]
	if !ok {
		return 0, fmt.Errorf("not a known phoneme: %q", p)
	}
	return b, nil
}

// DecodePhoneme returns the orthographic text for a phoneme byte.
func DecodePhoneme(b byte) (string, error) {
	if int(b) >= len(byteToPhoneme) || byteToPhoneme[b] == "" {
		return "", fmt.Errorf("phoneme byte out of range: %d", b)
	}
	return byteToPhoneme[b], nil
}

// EncodeCluster encodes a phoneme cluster (e.g. "ml", "ţř", "kpt") as
// a length-prefixed byte run. Each rune in the cluster must be a
// recognized phoneme. Max length 6 per the phonotactics; this codec
// allows up to 255 since the length is a full byte.
func EncodeCluster(cluster string) ([]byte, error) {
	runes := []rune(cluster)
	if len(runes) > 255 {
		return nil, fmt.Errorf("cluster too long for byte length: %d", len(runes))
	}
	out := make([]byte, 1, 1+len(runes))
	out[0] = byte(len(runes))
	for _, r := range runes {
		b, err := EncodePhoneme(string(r))
		if err != nil {
			return nil, fmt.Errorf("cluster %q: %w", cluster, err)
		}
		out = append(out, b)
	}
	return out, nil
}

// DecodeCluster reads a length-prefixed cluster from buf and returns
// (cluster, bytes-consumed, error).
func DecodeCluster(buf []byte) (string, int, error) {
	if len(buf) == 0 {
		return "", 0, fmt.Errorf("empty buffer reading cluster length")
	}
	n := int(buf[0])
	if len(buf) < 1+n {
		return "", 0, fmt.Errorf("cluster wants %d bytes, have %d", n, len(buf)-1)
	}
	out := make([]rune, 0, n)
	for i := 0; i < n; i++ {
		s, err := DecodePhoneme(buf[1+i])
		if err != nil {
			return "", 0, fmt.Errorf("cluster byte %d: %w", i, err)
		}
		out = append(out, []rune(s)...)
	}
	return string(out), 1 + n, nil
}
