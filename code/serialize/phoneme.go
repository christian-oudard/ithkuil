// Package serialize provides a binary codec for Ithkuil words: the
// grammatical structure a formative parses into, written as bytes.
//
// The point is to store meaning rather than pronunciation. A
// grammar.Formative is the parsed word, and the romanization is
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

// numConsonants is where the vowels start in the phoneme table. Every
// cluster the format stores is consonants only, so it is also the size
// of the alphabet a cluster draws on.
const numConsonants = 31

// Cluster encoding.
//
// Every cluster in the format is consonants only — a root Cr, an affix
// Cs, a referential C1 — and Ithkuil has 31 consonants. So a consonant
// is five bits, not the eight a byte apiece would spend, and a cluster
// is those five-bit fields packed end to end behind a length code.
//
// The packing stays inside the cluster: a cluster begins and ends on a
// byte boundary, which is for the decoder's sake. Raw size is what the
// codec optimises, and compressing the result is a separate concern
// with its own tools.
//
// The length codes are chosen against the corpus distribution
// (1:1405, 2:3380, 3:1049, 4:71, 5:5, 6:5):
//
//	len 3    "1"                    1 + 15 = 16 bits → 2 bytes
//	len 1    "000"                  3 +  5 =  8      → 1
//	len 2    "001"                  3 + 10 = 13      → 2
//	len 4    "010"                  3 + 20 = 23      → 3
//	len 5+   "011" + 3 bits of n-5  6 + 5n
//
// Length 3 gets the one-bit code even though length 2 is three times
// commoner. Fifteen bits of payload leaves room for exactly one bit of
// framing inside two bytes, so it is the only length where a shorter
// code buys a whole byte; length 2 needs a second byte either way.
const (
	lenCode1   = 0 // "000"
	lenCode2   = 1 // "001"
	lenCode4   = 2 // "010"
	lenCodeBig = 3 // "011", followed by three bits of n-5
	maxCluster = 12
)

// bitWriter appends big-endian bit fields, padding the last byte with
// zeros. Only ever used within one cluster.
type bitWriter struct {
	out []byte
	acc uint32
	n   uint
}

func (w *bitWriter) put(v uint32, bits uint) {
	w.acc = w.acc<<bits | v
	w.n += bits
	for w.n >= 8 {
		w.n -= 8
		w.out = append(w.out, byte(w.acc>>w.n))
	}
}

func (w *bitWriter) flush() {
	if w.n > 0 {
		w.out = append(w.out, byte(w.acc<<(8-w.n)))
		w.n = 0
	}
}

// bitReader is the inverse, tracking a bit offset into buf.
type bitReader struct {
	buf []byte
	pos uint
}

func (r *bitReader) get(bits uint) (uint32, error) {
	if r.pos+bits > uint(len(r.buf))*8 {
		return 0, fmt.Errorf("cluster: short read")
	}
	var v uint32
	for i := uint(0); i < bits; i++ {
		p := r.pos + i
		v = v<<1 | uint32(r.buf[p/8]>>(7-p%8)&1)
	}
	r.pos += bits
	return v, nil
}

// align advances to the next byte boundary, matching bitWriter.flush.
func (r *bitReader) align() { r.pos = (r.pos + 7) / 8 * 8 }

// bytesRead rounds the bit offset up to the byte the cluster ends on.
func (r *bitReader) bytesRead() int { return int((r.pos + 7) / 8) }

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
	n := len(runes)
	if n == 0 {
		return nil, fmt.Errorf("empty cluster")
	}
	if n > maxCluster {
		return nil, fmt.Errorf("cluster %q: %d consonants, more than the %d the length code can express",
			cluster, n, maxCluster)
	}
	codes := make([]uint32, n)
	for i, r := range runes {
		b, err := EncodePhoneme(string(r))
		if err != nil {
			return nil, fmt.Errorf("cluster %q: %w", cluster, err)
		}
		if int(b) >= numConsonants {
			return nil, fmt.Errorf("cluster %q: %q is a vowel, and a cluster is consonants only",
				cluster, string(r))
		}
		codes[i] = uint32(b)
	}

	w := bitWriter{out: out}
	switch n {
	case 3:
		w.put(1, 1)
	case 1:
		w.put(lenCode1, 3)
	case 2:
		w.put(lenCode2, 3)
	case 4:
		w.put(lenCode4, 3)
	default:
		w.put(lenCodeBig, 3)
		w.put(uint32(n-5), 3)
	}
	for i, c := range codes {
		// Only the first consonant shares a byte with the length code.
		// The rest are packed when packing buys a byte and given a byte
		// each when it does not; see the table above for which lengths
		// are which.
		if i > 0 && !packs(n) {
			// A whole byte, so the byte's value is the consonant's
			// own index rather than a shifted slice of it. Costs
			// nothing at these lengths and keeps a hex dump legible.
			w.flush()
			w.put(c, 8)
			continue
		}
		w.put(c, 5)
	}
	w.flush()
	return w.out, nil
}

// packs reports whether a cluster of n consonants is bit-packed. It is
// worth doing only where five-bit fields fit in fewer bytes than a byte
// apiece would take, which is every length from three up. Below that,
// packing would smear a consonant across a byte boundary and buy
// nothing, so each consonant keeps its own byte and its own value.
func packs(n int) bool { return n >= 3 }

func getCluster(buf []byte) (string, int, error) {
	r := bitReader{buf: buf}
	lead, err := r.get(1)
	if err != nil {
		return "", 0, err
	}
	n := 3
	if lead == 0 {
		code, err := r.get(2)
		if err != nil {
			return "", 0, err
		}
		switch code {
		case lenCode1:
			n = 1
		case lenCode2:
			n = 2
		case lenCode4:
			n = 4
		default:
			extra, err := r.get(3)
			if err != nil {
				return "", 0, err
			}
			n = int(extra) + 5
		}
	}

	var out []rune
	for i := 0; i < n; i++ {
		bits := uint(5)
		if i > 0 && !packs(n) {
			r.align()
			bits = 8
		}
		v, err := r.get(bits)
		if err != nil {
			return "", 0, fmt.Errorf("cluster consonant %d: %w", i, err)
		}
		if v >= numConsonants {
			return "", 0, fmt.Errorf("cluster consonant %d: value %d is not a consonant", i, v)
		}
		s, err := DecodePhoneme(byte(v))
		if err != nil {
			return "", 0, err
		}
		out = append(out, []rune(s)...)
	}
	return string(out), r.bytesRead(), nil
}
