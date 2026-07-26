package serialize

import (
	"fmt"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// nonFormative is the two-byte prefix that introduces every token that
// is not a formative. A formative writes no tag at all: it is most of
// the tokens in running text, so it gets the unprefixed encoding and
// everything else pays two bytes to escape out of it.
//
// The escape is free because those two bytes cannot begin a formative.
// The first says "Slot I is present and the root is a plain Cr", so the
// second must be the concatenation status — and that byte is only
// written when the status is Type1 or Type2, never 0.
var nonFormative = [2]byte{fConcat, 0}

// Token-type tag bytes, following the nonFormative prefix. Tags 0 and 1
// are reserved and never emitted; a formative carries no tag at all,
// being recognised by the absence of a prefix.
//
// Register start and end share a tag: they carry the same enum, so the
// direction rides in the high bit of the register byte.
const (
	TokenBias           byte = 2
	TokenRegister       byte = 3
	TokenForeign        byte = 4
	TokenParsingAdjunct byte = 5
	TokenCarrier        byte = 6
	TokenModular        byte = 7
	TokenSingleAffix    byte = 8
	TokenMultipleAffix  byte = 9
	TokenReferential    byte = 10
	TokenCombinationRef byte = 11
)

// registerEnd marks a RegisterEndWord in the register byte.
const registerEnd = 1 << 7

// MarshalWord encodes a single tokenize.WordToken to bytes. A formative
// is written directly; every other token is introduced by the
// nonFormative prefix and a type tag.
func MarshalWord(t tokenize.WordToken) ([]byte, error) {
	switch v := t.(type) {
	case tokenize.FormativeWord:
		// A Cc marker means "another formative follows", so it belongs
		// to a chain and never to a lone formative. Writing one here
		// would make the decoder swallow the next token.
		if v.Formative.Concat != g.ConcatNone {
			return nil, fmt.Errorf("lone formative carries concatenation status %v; use a ConcatenatedFormativeWord", v.Formative.Concat)
		}
		return putFormative(nil, v.Formative)
	case tokenize.ConcatenatedFormativeWord:
		return putChain(nil, v.Chain)
	}
	out := append([]byte{}, nonFormative[:]...)
	switch v := t.(type) {
	case tokenize.BiasWord:
		return append(out, TokenBias, byte(v.Bias)), nil
	case tokenize.RegisterStartWord:
		return append(out, TokenRegister, byte(v.Register)), nil
	case tokenize.RegisterEndWord:
		return append(out, TokenRegister, byte(v.Register)|registerEnd), nil
	case tokenize.ParsingAdjunctWord:
		return append(out, TokenParsingAdjunct, byte(v.Adjunct.Stress)), nil
	case tokenize.ForeignWord:
		return putForeign(append(out, TokenForeign), v.Text), nil
	case tokenize.CarrierWord:
		return putCarrier(append(out, TokenCarrier), v.Carrier), nil
	case tokenize.ModularWord:
		return putModular(append(out, TokenModular), v.Modular)
	case tokenize.SingleAffixWord:
		return putSingleAffix(append(out, TokenSingleAffix), v.Affix)
	case tokenize.MultipleAffixWord:
		return putMultipleAffix(append(out, TokenMultipleAffix), v.Affixes)
	case tokenize.ReferentialWord:
		return putReferential(append(out, TokenReferential), v)
	case tokenize.CombinationRefWord:
		return putCombinationRef(append(out, TokenCombinationRef), v)
	}
	return nil, fmt.Errorf("MarshalWord: unsupported token %T", t)
}

// UnmarshalWord decodes a single token from bytes. Returns the token
// plus the number of bytes consumed, so a sentence-level decoder can
// stream multiple tokens from one buffer.
func UnmarshalWord(buf []byte) (tokenize.WordToken, int, error) {
	if len(buf) == 0 {
		return nil, 0, fmt.Errorf("empty input")
	}
	if len(buf) < 2 || buf[0] != nonFormative[0] || buf[1] != nonFormative[1] {
		f, n, err := getFormative(buf)
		if err != nil {
			return nil, 0, err
		}
		// A Cc marker on the first formative opens a §3.1.7 chain; the
		// rest of it follows inline, ending at the parent.
		if f.Concat != g.ConcatNone {
			c, rest, err := getChain(f, buf[n:])
			if err != nil {
				return nil, 0, err
			}
			return tokenize.ConcatenatedFormativeWord{Chain: c}, n + rest, nil
		}
		return tokenize.FormativeWord{Formative: f}, n, nil
	}
	if len(buf) < 3 {
		return nil, 0, fmt.Errorf("token: prefix with no type tag")
	}
	tag, rest := buf[2], buf[3:]
	const hdr = 3

	// The one-byte-payload tags, which is most of them.
	switch tag {
	case TokenBias, TokenRegister, TokenParsingAdjunct:
		if len(rest) < 1 {
			return nil, 0, fmt.Errorf("tag %d: short read", tag)
		}
		b := rest[0]
		switch tag {
		case TokenBias:
			return tokenize.BiasWord{Bias: g.Bias(b)}, hdr + 1, nil
		case TokenRegister:
			reg := g.Register(b &^ registerEnd)
			if b&registerEnd != 0 {
				return tokenize.RegisterEndWord{Register: reg}, hdr + 1, nil
			}
			return tokenize.RegisterStartWord{Register: reg}, hdr + 1, nil
		default:
			return tokenize.ParsingAdjunctWord{
				Adjunct: g.ParsingAdjunct{Stress: stressFromByte(b)},
			}, hdr + 1, nil
		}
	}

	// The variable-length tags.
	switch tag {
	case TokenForeign:
		s, n, err := getForeign(rest)
		if err != nil {
			return nil, 0, err
		}
		return tokenize.ForeignWord{Text: s}, hdr + n, nil
	case TokenCarrier:
		c, n, err := getCarrier(rest)
		if err != nil {
			return nil, 0, err
		}
		return tokenize.CarrierWord{Carrier: c}, hdr + n, nil
	case TokenModular:
		m, n, err := getModular(rest)
		if err != nil {
			return nil, 0, err
		}
		return tokenize.ModularWord{Modular: m}, hdr + n, nil
	case TokenSingleAffix:
		a, n, err := getSingleAffix(rest)
		if err != nil {
			return nil, 0, err
		}
		return tokenize.SingleAffixWord{Affix: a}, hdr + n, nil
	case TokenMultipleAffix:
		m, n, err := getMultipleAffix(rest)
		if err != nil {
			return nil, 0, err
		}
		return tokenize.MultipleAffixWord{Affixes: m}, hdr + n, nil
	case TokenReferential:
		r, n, err := getReferential(rest)
		if err != nil {
			return nil, 0, err
		}
		return r, hdr + n, nil
	case TokenCombinationRef:
		c, n, err := getCombinationRef(rest)
		if err != nil {
			return nil, 0, err
		}
		return c, hdr + n, nil
	}
	return nil, 0, fmt.Errorf("UnmarshalWord: unknown tag %d", tag)
}

// FormatVersion is the current binary format version. A stream begins
// with this byte; decoders reject unknown versions so any future
// incompatible layout change is detected at the boundary.
const FormatVersion byte = 2

// MarshalTokens encodes a stream of tokens. Layout:
//
//	[format version byte]    FormatVersion
//	[uvarint token count]
//	[token bytes...]
//
// Each token self-delimits. Nothing here pins the lexicon: roots and
// affixes are written as phoneme clusters, so a file outlives the
// lexicon revision it was written against.
//
// Ithkuil's grammar has no sentence or paragraph structure — sentence
// boundaries are prosodic (§5.8 ¶8) and not encoded in the romanized
// text. The format mirrors that: one stream of tokens, no higher
// framing.
func MarshalTokens(tokens []tokenize.WordToken) ([]byte, error) {
	out := appendUvarint([]byte{FormatVersion}, uint64(len(tokens)))
	for _, t := range tokens {
		b, err := MarshalWord(t)
		if err != nil {
			return nil, err
		}
		out = append(out, b...)
	}
	return out, nil
}

// UnmarshalTokens is the inverse of MarshalTokens.
func UnmarshalTokens(buf []byte) ([]tokenize.WordToken, error) {
	if len(buf) < 2 {
		return nil, fmt.Errorf("tokens: short input (need at least 2 header bytes)")
	}
	if buf[0] != FormatVersion {
		return nil, fmt.Errorf("tokens: unknown format version %d (this decoder supports %d)", buf[0], FormatVersion)
	}
	count, n, err := getUvarint(buf[1:])
	if err != nil {
		return nil, fmt.Errorf("tokens: bad token count: %w", err)
	}
	cur := 1 + n
	out := make([]tokenize.WordToken, 0, count)
	for i := uint64(0); i < count; i++ {
		t, consumed, err := UnmarshalWord(buf[cur:])
		if err != nil {
			return nil, fmt.Errorf("tokens: token %d: %w", i, err)
		}
		cur += consumed
		out = append(out, t)
	}
	// A modular adjunct's MarksMood is read off its neighbours rather
	// than off the adjunct, so it is not stored. Restore it now that
	// the whole stream is back.
	tokenize.ResolveModularMood(out)
	return out, nil
}
