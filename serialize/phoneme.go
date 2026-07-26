// Package serialize provides a binary codec for Ithkuil words: the
// grammatical structure a formative parses into, written as bytes.
//
// The point is to store meaning rather than pronunciation. A
// grammar.Formative is the parsed word, and the romanized surface is
// one rendering of it; this codec writes the structure directly, so
// nothing depends on orthographic or phonotactic detail.
//
// The encoding is version-independent: roots and affixes are written
// as phoneme clusters, never as lexicon indices, so a file stays
// readable when the lexicon is updated. See formative.go for the
// layout rules and what they buy.
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

// clusterEnd marks the last phoneme of a cluster. Phoneme values run
// 0..39, so bit 6 is free to carry the terminator and no length prefix
// is needed: a two-consonant cluster costs two bytes.
const clusterEnd = 0x40

// EncodeCluster encodes a phoneme cluster (e.g. "ml", "ţř", "kpt").
// Each rune must be a recognized phoneme. Clusters are never empty.
func EncodeCluster(cluster string) ([]byte, error) {
	return putCluster(nil, cluster)
}

// DecodeCluster reads a cluster from the head of buf and returns
// (cluster, bytes-consumed, error).
func DecodeCluster(buf []byte) (string, int, error) {
	return getCluster(buf)
}

func putCluster(out []byte, cluster string) ([]byte, error) {
	runes := []rune(cluster)
	if len(runes) == 0 {
		return nil, fmt.Errorf("empty cluster")
	}
	for i, r := range runes {
		b, err := EncodePhoneme(string(r))
		if err != nil {
			return nil, fmt.Errorf("cluster %q: %w", cluster, err)
		}
		if i == len(runes)-1 {
			b |= clusterEnd
		}
		out = append(out, b)
	}
	return out, nil
}

func getCluster(buf []byte) (string, int, error) {
	var out []rune
	for i, b := range buf {
		s, err := DecodePhoneme(b &^ clusterEnd)
		if err != nil {
			return "", 0, fmt.Errorf("cluster byte %d: %w", i, err)
		}
		out = append(out, []rune(s)...)
		if b&clusterEnd != 0 {
			return string(out), i + 1, nil
		}
	}
	return "", 0, fmt.Errorf("unterminated cluster")
}
