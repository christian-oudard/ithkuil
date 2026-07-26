package serialize

import (
	"bytes"
	"encoding/binary"
	"fmt"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/referentials"
	"github.com/christian-oudard/ithkuil/surface"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// nonFormative is the two-byte prefix that introduces every token that
// is not a formative. A formative writes no tag at all: it is four
// words in five in running text, so it gets the unprefixed encoding
// and everything else pays two bytes to escape out of it.
//
// The escape is free because those two bytes cannot begin a formative.
// The first says "Slot I is present and the root is a plain Cr", so
// the second must be the concatenation status — and that byte is only
// written when the status is Type1 or Type2, never 0.
var nonFormative = [2]byte{fConcat, 0}

// Token-type tag bytes, following the nonFormative prefix. Tag 0 and
// tag 1 are reserved and never emitted; a formative carries no tag at
// all, being recognised by the absence of a prefix.
const (
	TokenBias           byte = 2
	TokenRegisterStart  byte = 3
	TokenRegisterEnd    byte = 4
	TokenParsingAdjunct byte = 5
	TokenCarrier        byte = 6
	TokenModular        byte = 7
	TokenSingleAffix    byte = 8
	TokenMultipleAffix  byte = 9
	TokenReferential    byte = 10
	TokenCombinationRef byte = 11
)

// MarshalWord encodes a single tokenize.WordToken to bytes. A
// formative is written directly; every other token is introduced by
// the nonFormative prefix and a type tag.
func MarshalWord(t tokenize.WordToken) ([]byte, error) {
	var buf bytes.Buffer
	if fw, ok := t.(tokenize.FormativeWord); ok {
		return putFormative(nil, fw.Formative)
	}
	buf.Write(nonFormative[:])
	switch v := t.(type) {
	case tokenize.BiasWord:
		buf.WriteByte(TokenBias)
		buf.WriteByte(byte(v.Bias))
	case tokenize.RegisterStartWord:
		buf.WriteByte(TokenRegisterStart)
		buf.WriteByte(byte(v.Register))
	case tokenize.RegisterEndWord:
		buf.WriteByte(TokenRegisterEnd)
		buf.WriteByte(byte(v.Register))
	case tokenize.ParsingAdjunctWord:
		buf.WriteByte(TokenParsingAdjunct)
		buf.WriteByte(byte(v.Adjunct.Stress))
	case tokenize.CarrierWord:
		buf.WriteByte(TokenCarrier)
		buf.WriteByte(byte(v.Carrier.Type))
		buf.WriteByte(byte(v.Carrier.Case))
	case tokenize.ModularWord:
		buf.WriteByte(TokenModular)
		if err := writeModular(&buf, v.Modular); err != nil {
			return nil, err
		}
	case tokenize.SingleAffixWord:
		buf.WriteByte(TokenSingleAffix)
		buf.WriteByte(byte(v.Affix.Scope))
		if err := writeAffix(&buf, v.Affix.Affix); err != nil {
			return nil, err
		}
	case tokenize.MultipleAffixWord:
		buf.WriteByte(TokenMultipleAffix)
		buf.WriteByte(byte(v.Affixes.FirstScope))
		buf.WriteByte(byte(v.Affixes.RestScope))
		if err := writeAffix(&buf, v.Affixes.First); err != nil {
			return nil, err
		}
		buf.WriteByte(byte(len(v.Affixes.Rest)))
		for _, a := range v.Affixes.Rest {
			if err := writeAffix(&buf, a); err != nil {
				return nil, err
			}
		}
	case tokenize.ReferentialWord:
		buf.WriteByte(TokenReferential)
		if err := writeReferential(&buf, v); err != nil {
			return nil, err
		}
	case tokenize.CombinationRefWord:
		buf.WriteByte(TokenCombinationRef)
		if err := writeCombinationRef(&buf, v); err != nil {
			return nil, err
		}
	default:
		return nil, fmt.Errorf("MarshalWord: unsupported token %T", t)
	}
	return buf.Bytes(), nil
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
		return tokenize.FormativeWord{Formative: f}, n, nil
	}
	if len(buf) < 3 {
		return nil, 0, fmt.Errorf("token: prefix with no type tag")
	}
	tag := buf[2]
	rest := buf[3:]
	consumed := 3
	switch tag {
	case TokenBias:
		if len(rest) < 1 {
			return nil, 0, fmt.Errorf("bias: short read")
		}
		return tokenize.BiasWord{Bias: g.Bias(rest[0])}, consumed + 1, nil
	case TokenRegisterStart:
		if len(rest) < 1 {
			return nil, 0, fmt.Errorf("register start: short read")
		}
		return tokenize.RegisterStartWord{Register: g.Register(rest[0])}, consumed + 1, nil
	case TokenRegisterEnd:
		if len(rest) < 1 {
			return nil, 0, fmt.Errorf("register end: short read")
		}
		return tokenize.RegisterEndWord{Register: g.Register(rest[0])}, consumed + 1, nil
	case TokenParsingAdjunct:
		if len(rest) < 1 {
			return nil, 0, fmt.Errorf("parsing adjunct: short read")
		}
		return tokenize.ParsingAdjunctWord{
			Adjunct: g.ParsingAdjunct{Stress: stressFromByte(rest[0])},
		}, consumed + 1, nil
	case TokenCarrier:
		if len(rest) < 2 {
			return nil, 0, fmt.Errorf("carrier: short read")
		}
		return tokenize.CarrierWord{
			Carrier: g.CarrierAdjunct{
				Type: g.CarrierType(rest[0]),
				Case: g.Case(rest[1]),
			},
		}, consumed + 2, nil
	case TokenModular:
		ma, n, err := readModular(rest)
		if err != nil {
			return nil, 0, err
		}
		return tokenize.ModularWord{Modular: ma}, consumed + n, nil
	case TokenSingleAffix:
		if len(rest) < 1 {
			return nil, 0, fmt.Errorf("single affix: short read")
		}
		scope := g.AffixScope(rest[0])
		a, n, err := readAffix(rest[1:])
		if err != nil {
			return nil, 0, err
		}
		return tokenize.SingleAffixWord{
			Affix: g.SingleAffixAdjunct{Affix: a, Scope: scope},
		}, consumed + 1 + n, nil
	case TokenMultipleAffix:
		if len(rest) < 2 {
			return nil, 0, fmt.Errorf("multi affix: short header")
		}
		first := g.AffixScope(rest[0])
		restScope := g.AffixScope(rest[1])
		cur := 2
		fa, n, err := readAffix(rest[cur:])
		if err != nil {
			return nil, 0, err
		}
		cur += n
		if len(rest) < cur+1 {
			return nil, 0, fmt.Errorf("multi affix: missing rest count")
		}
		restCount := int(rest[cur])
		cur++
		var rests []g.Affix
		for i := 0; i < restCount; i++ {
			a, n, err := readAffix(rest[cur:])
			if err != nil {
				return nil, 0, err
			}
			cur += n
			rests = append(rests, a)
		}
		return tokenize.MultipleAffixWord{
			Affixes: g.MultipleAffixAdjunct{
				First:      fa,
				Rest:       rests,
				FirstScope: first,
				RestScope:  restScope,
			},
		}, consumed + cur, nil
	case TokenReferential:
		w, n, err := readReferential(rest)
		if err != nil {
			return nil, 0, err
		}
		return w, consumed + n, nil
	case TokenCombinationRef:
		w, n, err := readCombinationRef(rest)
		if err != nil {
			return nil, 0, err
		}
		return w, consumed + n, nil
	}
	return nil, 0, fmt.Errorf("UnmarshalWord: unknown tag %d", tag)
}

// FormatVersion is the current binary format version. A sentence
// begins with this byte; decoders reject unknown versions so any
// future incompatible layout change is detected at the boundary.
const FormatVersion byte = 2

// MarshalTokens encodes a stream of tokens. Layout:
//
//	[format version byte]    FormatVersion
//	[uvarint token count]
//	[token bytes...]
//
// Each token self-delimits via its leading type tag. Nothing here
// pins the lexicon: roots and affixes are written as phoneme clusters,
// so a file outlives the lexicon revision it was written against.
//
// Ithkuil's grammar has no sentence or paragraph structure — sentence
// boundaries are prosodic (§5.8 ¶8) and not encoded in the romanized
// text. The wire format mirrors that: one stream of tokens, no higher
// framing.
func MarshalTokens(tokens []tokenize.WordToken) ([]byte, error) {
	var buf bytes.Buffer
	buf.WriteByte(FormatVersion)
	var hdr [binary.MaxVarintLen64]byte
	n := binary.PutUvarint(hdr[:], uint64(len(tokens)))
	buf.Write(hdr[:n])
	for _, t := range tokens {
		b, err := MarshalWord(t)
		if err != nil {
			return nil, err
		}
		buf.Write(b)
	}
	return buf.Bytes(), nil
}

// UnmarshalTokens is the inverse of MarshalTokens.
func UnmarshalTokens(buf []byte) ([]tokenize.WordToken, error) {
	if len(buf) < 2 {
		return nil, fmt.Errorf("tokens: short input (need >=2 bytes of header)")
	}
	if buf[0] != FormatVersion {
		return nil, fmt.Errorf("tokens: unknown format version %d (this decoder supports %d)", buf[0], FormatVersion)
	}
	cur := 1
	count, n := binary.Uvarint(buf[cur:])
	if n <= 0 {
		return nil, fmt.Errorf("tokens: bad token-count varint")
	}
	cur += n
	out := make([]tokenize.WordToken, 0, count)
	for i := uint64(0); i < count; i++ {
		t, consumed, err := UnmarshalWord(buf[cur:])
		if err != nil {
			return nil, fmt.Errorf("tokens token %d: %w", i, err)
		}
		cur += consumed
		out = append(out, t)
	}
	return out, nil
}

// ----- affix -----

// writeAffix encodes one affix using the same (Type, Degree, Cs) byte
// layout the formative slots use, so an adjunct's affix and a Slot VII
// affix are byte-identical.
func writeAffix(buf *bytes.Buffer, a g.Affix) error {
	b, err := putAffixes(nil, []g.Affix{a})
	if err != nil {
		return err
	}
	buf.Write(b)
	return nil
}

func readAffix(buf []byte) (g.Affix, int, error) {
	as, n, err := getAffixes(buf)
	if err != nil {
		return g.Affix{}, 0, err
	}
	if len(as) != 1 {
		return g.Affix{}, 0, fmt.Errorf("affix: expected 1, got %d", len(as))
	}
	return as[0], n, nil
}

// ----- modular -----

func writeModular(buf *bytes.Buffer, m g.ModularAdjunct) error {
	buf.WriteByte(byte(m.Scope))
	buf.WriteByte(byte(m.Reach))
	buf.WriteByte(byte(len(m.Content)))
	for _, s := range m.Content {
		b, err := putSlotVIII(nil, s)
		if err != nil {
			return fmt.Errorf("modular content: %w", err)
		}
		buf.Write(b)
	}
	return nil
}

func readModular(buf []byte) (g.ModularAdjunct, int, error) {
	if len(buf) < 3 {
		return g.ModularAdjunct{}, 0, fmt.Errorf("modular: short header")
	}
	scope := g.ModularScope(buf[0])
	reach := g.ModularReach(buf[1])
	count := int(buf[2])
	cur := 3
	var content []g.SlotVIII
	for i := 0; i < count; i++ {
		s, n, err := getSlotVIII(buf[cur:])
		if err != nil {
			return g.ModularAdjunct{}, 0, fmt.Errorf("modular content[%d]: %w", i, err)
		}
		cur += n
		content = append(content, s)
	}
	return g.ModularAdjunct{Scope: scope, Reach: reach, Content: content}, cur, nil
}

// ----- referential -----

const (
	refFlagCarrier  = 1 << 0
	refFlagCategory = 1 << 1
	refFlagCase     = 1 << 2
	refFlagCase2    = 1 << 3
	refFlagRefB     = 1 << 4
	refFlagRPV      = 1 << 5
)

func writeReferential(buf *bytes.Buffer, r tokenize.ReferentialWord) error {
	flags := byte(0)
	if r.Carrier != nil {
		flags |= refFlagCarrier
	}
	if r.Category != nil {
		flags |= refFlagCategory
	}
	if r.Case != nil {
		flags |= refFlagCase
	}
	if r.Case2 != nil {
		flags |= refFlagCase2
	}
	if len(r.RefB) > 0 {
		flags |= refFlagRefB
	}
	if r.RpvEssence {
		flags |= refFlagRPV
	}
	buf.WriteByte(flags)
	if r.Carrier != nil {
		buf.WriteByte(byte(*r.Carrier))
	}
	if r.Category != nil {
		buf.WriteByte(byte(*r.Category))
	}
	writeRefList(buf, r.Refs)
	if r.Case != nil {
		buf.WriteByte(byte(*r.Case))
	}
	if r.Case2 != nil {
		buf.WriteByte(byte(*r.Case2))
	}
	if len(r.RefB) > 0 {
		writeRefList(buf, r.RefB)
	}
	return nil
}

func readReferential(buf []byte) (tokenize.ReferentialWord, int, error) {
	if len(buf) < 1 {
		return tokenize.ReferentialWord{}, 0, fmt.Errorf("ref: short flags")
	}
	flags := buf[0]
	cur := 1
	var carrier *g.CarrierType
	var category *referentials.Category
	var caseV *g.Case
	var case2 *g.Case
	var refB []referentials.PersonalRef
	if flags&refFlagCarrier != 0 {
		ct := g.CarrierType(buf[cur])
		carrier = &ct
		cur++
	}
	if flags&refFlagCategory != 0 {
		cat := referentials.Category(buf[cur])
		category = &cat
		cur++
	}
	refs, n, err := readRefList(buf[cur:])
	if err != nil {
		return tokenize.ReferentialWord{}, 0, fmt.Errorf("ref refs: %w", err)
	}
	cur += n
	if flags&refFlagCase != 0 {
		cv := g.Case(buf[cur])
		caseV = &cv
		cur++
	}
	if flags&refFlagCase2 != 0 {
		cv := g.Case(buf[cur])
		case2 = &cv
		cur++
	}
	if flags&refFlagRefB != 0 {
		rb, n, err := readRefList(buf[cur:])
		if err != nil {
			return tokenize.ReferentialWord{}, 0, fmt.Errorf("ref refB: %w", err)
		}
		cur += n
		refB = rb
	}
	return tokenize.ReferentialWord{
		Carrier:    carrier,
		Refs:       refs,
		Category:   category,
		Case:       caseV,
		Case2:      case2,
		RefB:       refB,
		RpvEssence: flags&refFlagRPV != 0,
	}, cur, nil
}

// ----- combination ref -----

const (
	combFlagCarrier = 1 << 0
	combFlagCase2   = 1 << 1
)

func writeCombinationRef(buf *bytes.Buffer, c tokenize.CombinationRefWord) error {
	flags := byte(0)
	if c.Carrier != nil {
		flags |= combFlagCarrier
	}
	if c.Case2 != nil {
		flags |= combFlagCase2
	}
	buf.WriteByte(flags)
	if c.Carrier != nil {
		buf.WriteByte(byte(*c.Carrier))
	}
	writeRefList(buf, c.Refs)
	buf.WriteByte(byte(c.Case))
	buf.WriteByte(byte(c.Spec))
	buf.WriteByte(byte(len(c.Affixes)))
	for _, a := range c.Affixes {
		if err := writeAffix(buf, a); err != nil {
			return err
		}
	}
	if c.Case2 != nil {
		buf.WriteByte(byte(*c.Case2))
	}
	return nil
}

func readCombinationRef(buf []byte) (tokenize.CombinationRefWord, int, error) {
	if len(buf) < 1 {
		return tokenize.CombinationRefWord{}, 0, fmt.Errorf("combo: short flags")
	}
	flags := buf[0]
	cur := 1
	var carrier *g.CarrierType
	if flags&combFlagCarrier != 0 {
		ct := g.CarrierType(buf[cur])
		carrier = &ct
		cur++
	}
	refs, n, err := readRefList(buf[cur:])
	if err != nil {
		return tokenize.CombinationRefWord{}, 0, fmt.Errorf("combo refs: %w", err)
	}
	cur += n
	if len(buf) < cur+3 {
		return tokenize.CombinationRefWord{}, 0, fmt.Errorf("combo: short body")
	}
	caseV := g.Case(buf[cur])
	cur++
	spec := g.Specification(buf[cur])
	cur++
	affCount := int(buf[cur])
	cur++
	var affixes []g.Affix
	for i := 0; i < affCount; i++ {
		a, n, err := readAffix(buf[cur:])
		if err != nil {
			return tokenize.CombinationRefWord{}, 0, fmt.Errorf("combo affix[%d]: %w", i, err)
		}
		cur += n
		affixes = append(affixes, a)
	}
	var case2 *g.Case
	if flags&combFlagCase2 != 0 {
		cv := g.Case(buf[cur])
		case2 = &cv
		cur++
	}
	return tokenize.CombinationRefWord{
		Carrier: carrier,
		Refs:    refs,
		Case:    caseV,
		Spec:    spec,
		Affixes: affixes,
		Case2:   case2,
	}, cur, nil
}

// ----- helpers -----

func writeRefList(buf *bytes.Buffer, refs []referentials.PersonalRef) {
	buf.WriteByte(byte(len(refs)))
	for _, r := range refs {
		buf.WriteByte(byte(r.Referent))
		buf.WriteByte(byte(r.Effect))
	}
}

func readRefList(buf []byte) ([]referentials.PersonalRef, int, error) {
	if len(buf) < 1 {
		return nil, 0, fmt.Errorf("ref list: missing count")
	}
	n := int(buf[0])
	if len(buf) < 1+2*n {
		return nil, 0, fmt.Errorf("ref list: %d refs need %d bytes, have %d", n, 2*n, len(buf)-1)
	}
	var out []referentials.PersonalRef
	for i := 0; i < n; i++ {
		out = append(out, referentials.PersonalRef{
			Referent: referentials.Referent(buf[1+2*i]),
			Effect:   referentials.Effect(buf[1+2*i+1]),
		})
	}
	return out, 1 + 2*n, nil
}

func stressFromByte(b byte) surface.Stress {
	return surface.Stress(b)
}
