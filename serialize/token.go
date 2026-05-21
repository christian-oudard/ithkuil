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

// Token-type tag bytes. Sit at the head of every marshalled token so
// the decoder can dispatch without parsing the body. Tag 0 is
// reserved as a sentinel — never emit it.
const (
	TokenFormative      byte = 1
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

// MarshalWord encodes a single tokenize.WordToken to bytes. The first
// byte is the type tag; the remainder is the type-specific payload.
func MarshalWord(t tokenize.WordToken) ([]byte, error) {
	var buf bytes.Buffer
	switch v := t.(type) {
	case tokenize.FormativeWord:
		buf.WriteByte(TokenFormative)
		if err := writeFormative(&buf, v.Formative); err != nil {
			return nil, err
		}
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
	tag := buf[0]
	rest := buf[1:]
	consumed := 1
	switch tag {
	case TokenFormative:
		f, n, err := readFormative(rest)
		if err != nil {
			return nil, 0, err
		}
		return tokenize.FormativeWord{Formative: f}, consumed + n, nil
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
const FormatVersion byte = 1

// MarshalSentence encodes a sequence of tokens. Layout:
//
//	[version byte] [uvarint token count] [token bytes...]
//
// Each token self-delimits via its leading type tag, so the stream
// decodes without needing per-token length prefixes.
func MarshalSentence(tokens []tokenize.WordToken) ([]byte, error) {
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

// UnmarshalSentence is the inverse of MarshalSentence.
func UnmarshalSentence(buf []byte) ([]tokenize.WordToken, error) {
	if len(buf) < 1 {
		return nil, fmt.Errorf("sentence: empty input")
	}
	if buf[0] != FormatVersion {
		return nil, fmt.Errorf("sentence: unknown format version %d (this decoder supports %d)", buf[0], FormatVersion)
	}
	count, n := binary.Uvarint(buf[1:])
	if n <= 0 {
		return nil, fmt.Errorf("sentence: bad token-count varint")
	}
	cur := 1 + n
	out := make([]tokenize.WordToken, 0, count)
	for i := uint64(0); i < count; i++ {
		t, consumed, err := UnmarshalWord(buf[cur:])
		if err != nil {
			return nil, fmt.Errorf("sentence token %d: %w", i, err)
		}
		cur += consumed
		out = append(out, t)
	}
	return out, nil
}

// ----- formative -----

func writeFormative(buf *bytes.Buffer, f g.Formative) error {
	// Concat: 0=standalone, 1=Type1, 2=Type2.
	concatByte := byte(0)
	if f.Concat != nil {
		concatByte = byte(*f.Concat) + 1
	}
	buf.WriteByte(concatByte)
	// Root variant tag + payload.
	switch r := f.Root.(type) {
	case g.CrRoot:
		buf.WriteByte(0)
		buf.WriteByte(byte(r.Stem))
		buf.WriteByte(byte(r.Version))
		buf.WriteByte(byte(r.SlotIV.Function))
		buf.WriteByte(byte(r.SlotIV.Specification))
		buf.WriteByte(byte(r.SlotIV.Context))
		c, err := EncodeCluster(r.Cluster)
		if err != nil {
			return fmt.Errorf("Cr cluster: %w", err)
		}
		buf.Write(c)
	case g.CsRoot:
		buf.WriteByte(1)
		buf.WriteByte(byte(r.Degree))
		buf.WriteByte(byte(r.Version))
		buf.WriteByte(byte(r.Function))
		buf.WriteByte(byte(r.Context))
		c, err := EncodeCluster(r.Cs)
		if err != nil {
			return fmt.Errorf("Cs cluster: %w", err)
		}
		buf.Write(c)
	case g.RefRoot:
		buf.WriteByte(2)
		buf.WriteByte(byte(r.Version))
		buf.WriteByte(byte(r.SlotIV.Function))
		buf.WriteByte(byte(r.SlotIV.Specification))
		buf.WriteByte(byte(r.SlotIV.Context))
		c, err := EncodeCluster(r.C1)
		if err != nil {
			return fmt.Errorf("Ref C1 cluster: %w", err)
		}
		buf.Write(c)
	default:
		return fmt.Errorf("unknown Root type %T", r)
	}
	// SlotV affixes (count + each).
	buf.WriteByte(byte(len(f.SlotV)))
	for _, a := range f.SlotV {
		if err := writeAffix(buf, a); err != nil {
			return err
		}
	}
	// SlotVI: 5 enum bytes.
	buf.WriteByte(byte(f.SlotVI.Configuration))
	buf.WriteByte(byte(f.SlotVI.Affiliation))
	buf.WriteByte(byte(f.SlotVI.Perspective))
	buf.WriteByte(byte(f.SlotVI.Extension))
	buf.WriteByte(byte(f.SlotVI.Essence))
	// SlotVII affixes.
	buf.WriteByte(byte(len(f.SlotVII)))
	for _, a := range f.SlotVII {
		if err := writeAffix(buf, a); err != nil {
			return err
		}
	}
	// SlotVIII: 0=absent, 1=Valence, 2=Phase, 3=Effect, 4=Level, 5=Aspect.
	if f.SlotVIII == nil {
		buf.WriteByte(0)
	} else {
		if err := writeSlotVIII(buf, f.SlotVIII); err != nil {
			return err
		}
	}
	// Final: 0=UnframedNominal, 1=FramedVerbal, 2=UnframedVerbal.
	switch fin := f.Final.(type) {
	case g.UnframedNominal:
		buf.WriteByte(0)
		buf.WriteByte(byte(fin.Case))
	case g.FramedVerbal:
		buf.WriteByte(1)
		buf.WriteByte(byte(fin.Case))
	case g.UnframedVerbal:
		buf.WriteByte(2)
		if err := writeVk(buf, fin.Vk); err != nil {
			return err
		}
	default:
		return fmt.Errorf("unknown Final type %T", fin)
	}
	return nil
}

func readFormative(buf []byte) (g.Formative, int, error) {
	if len(buf) < 7 {
		return g.Formative{}, 0, fmt.Errorf("formative: short read at header")
	}
	cur := 0
	concatByte := buf[cur]
	cur++
	var concat *g.ConcatenationStatus
	if concatByte > 0 {
		c := g.ConcatenationStatus(concatByte - 1)
		concat = &c
	}
	rootTag := buf[cur]
	cur++
	var root g.Root
	switch rootTag {
	case 0:
		if len(buf) < cur+5 {
			return g.Formative{}, 0, fmt.Errorf("Cr: short read")
		}
		stem := g.Stem(buf[cur])
		version := g.Version(buf[cur+1])
		fn := g.Function(buf[cur+2])
		spec := g.Specification(buf[cur+3])
		ctx := g.Context(buf[cur+4])
		cur += 5
		cluster, n, err := DecodeCluster(buf[cur:])
		if err != nil {
			return g.Formative{}, 0, fmt.Errorf("Cr cluster: %w", err)
		}
		cur += n
		root = g.CrRoot{
			Cluster: cluster, Stem: stem, Version: version,
			SlotIV: g.SlotIV{Function: fn, Specification: spec, Context: ctx},
		}
	case 1:
		if len(buf) < cur+4 {
			return g.Formative{}, 0, fmt.Errorf("Cs: short read")
		}
		degree := int(buf[cur])
		version := g.Version(buf[cur+1])
		fn := g.Function(buf[cur+2])
		ctx := g.Context(buf[cur+3])
		cur += 4
		cs, n, err := DecodeCluster(buf[cur:])
		if err != nil {
			return g.Formative{}, 0, fmt.Errorf("Cs cluster: %w", err)
		}
		cur += n
		root = g.CsRoot{Cs: cs, Degree: degree, Version: version, Function: fn, Context: ctx}
	case 2:
		if len(buf) < cur+4 {
			return g.Formative{}, 0, fmt.Errorf("Ref: short read")
		}
		version := g.Version(buf[cur])
		fn := g.Function(buf[cur+1])
		spec := g.Specification(buf[cur+2])
		ctx := g.Context(buf[cur+3])
		cur += 4
		c1, n, err := DecodeCluster(buf[cur:])
		if err != nil {
			return g.Formative{}, 0, fmt.Errorf("Ref C1: %w", err)
		}
		cur += n
		root = g.RefRoot{C1: c1, Version: version,
			SlotIV: g.SlotIV{Function: fn, Specification: spec, Context: ctx}}
	default:
		return g.Formative{}, 0, fmt.Errorf("unknown root tag %d", rootTag)
	}
	// SlotV.
	if len(buf) < cur+1 {
		return g.Formative{}, 0, fmt.Errorf("SlotV count: short read")
	}
	v5n := int(buf[cur])
	cur++
	var v5 []g.Affix
	for i := 0; i < v5n; i++ {
		a, n, err := readAffix(buf[cur:])
		if err != nil {
			return g.Formative{}, 0, fmt.Errorf("SlotV[%d]: %w", i, err)
		}
		cur += n
		v5 = append(v5, a)
	}
	// SlotVI.
	if len(buf) < cur+5 {
		return g.Formative{}, 0, fmt.Errorf("SlotVI: short read")
	}
	v6 := g.SlotVI{
		Configuration: g.Configuration(buf[cur]),
		Affiliation:   g.Affiliation(buf[cur+1]),
		Perspective:   g.Perspective(buf[cur+2]),
		Extension:     g.Extension(buf[cur+3]),
		Essence:       g.Essence(buf[cur+4]),
	}
	cur += 5
	// SlotVII.
	if len(buf) < cur+1 {
		return g.Formative{}, 0, fmt.Errorf("SlotVII count: short read")
	}
	v7n := int(buf[cur])
	cur++
	var v7 []g.Affix
	for i := 0; i < v7n; i++ {
		a, n, err := readAffix(buf[cur:])
		if err != nil {
			return g.Formative{}, 0, fmt.Errorf("SlotVII[%d]: %w", i, err)
		}
		cur += n
		v7 = append(v7, a)
	}
	// SlotVIII.
	if len(buf) < cur+1 {
		return g.Formative{}, 0, fmt.Errorf("SlotVIII tag: short read")
	}
	var v8 g.SlotVIII
	if buf[cur] == 0 {
		cur++
	} else {
		s, n, err := readSlotVIII(buf[cur:])
		if err != nil {
			return g.Formative{}, 0, err
		}
		cur += n
		v8 = s
	}
	// Final.
	if len(buf) < cur+1 {
		return g.Formative{}, 0, fmt.Errorf("Final tag: short read")
	}
	finTag := buf[cur]
	cur++
	var fin g.Final
	switch finTag {
	case 0:
		if len(buf) < cur+1 {
			return g.Formative{}, 0, fmt.Errorf("UnframedNominal: short read")
		}
		fin = g.UnframedNominal{Case: g.Case(buf[cur])}
		cur++
	case 1:
		if len(buf) < cur+1 {
			return g.Formative{}, 0, fmt.Errorf("FramedVerbal: short read")
		}
		fin = g.FramedVerbal{Case: g.Case(buf[cur])}
		cur++
	case 2:
		vk, n, err := readVk(buf[cur:])
		if err != nil {
			return g.Formative{}, 0, fmt.Errorf("UnframedVerbal: %w", err)
		}
		cur += n
		fin = g.UnframedVerbal{Vk: vk}
	default:
		return g.Formative{}, 0, fmt.Errorf("unknown Final tag %d", finTag)
	}
	return g.Formative{
		Concat:   concat,
		Root:     root,
		SlotV:    v5,
		SlotVI:   v6,
		SlotVII:  v7,
		SlotVIII: v8,
		Final:    fin,
	}, cur, nil
}

// ----- SlotVIII (variant tag + payload, no nil case) -----

func writeSlotVIII(buf *bytes.Buffer, s g.SlotVIII) error {
	switch v := s.(type) {
	case g.VnCnValence:
		buf.WriteByte(1)
		buf.WriteByte(byte(v.Valence))
		buf.WriteByte(byte(v.MoodScope))
	case g.VnCnPhase:
		buf.WriteByte(2)
		buf.WriteByte(byte(v.Phase))
		buf.WriteByte(byte(v.MoodScope))
	case g.VnCnEffect:
		buf.WriteByte(3)
		buf.WriteByte(byte(v.Effect))
		buf.WriteByte(byte(v.MoodScope))
	case g.VnCnLevel:
		buf.WriteByte(4)
		buf.WriteByte(byte(v.Level))
		buf.WriteByte(byte(v.MoodScope))
		if v.Absolute {
			buf.WriteByte(1)
		} else {
			buf.WriteByte(0)
		}
	case g.VnCnAspect:
		buf.WriteByte(5)
		buf.WriteByte(byte(v.Aspect))
		buf.WriteByte(byte(v.MoodScope))
	default:
		return fmt.Errorf("unknown SlotVIII type %T", s)
	}
	return nil
}

func readSlotVIII(buf []byte) (g.SlotVIII, int, error) {
	if len(buf) < 1 {
		return nil, 0, fmt.Errorf("SlotVIII: short tag")
	}
	switch buf[0] {
	case 1:
		if len(buf) < 3 {
			return nil, 0, fmt.Errorf("VnCnValence: short read")
		}
		return g.VnCnValence{Valence: g.Valence(buf[1]), MoodScope: g.Mood(buf[2])}, 3, nil
	case 2:
		if len(buf) < 3 {
			return nil, 0, fmt.Errorf("VnCnPhase: short read")
		}
		return g.VnCnPhase{Phase: g.Phase(buf[1]), MoodScope: g.Mood(buf[2])}, 3, nil
	case 3:
		if len(buf) < 3 {
			return nil, 0, fmt.Errorf("VnCnEffect: short read")
		}
		return g.VnCnEffect{Effect: g.Effect(buf[1]), MoodScope: g.Mood(buf[2])}, 3, nil
	case 4:
		if len(buf) < 4 {
			return nil, 0, fmt.Errorf("VnCnLevel: short read")
		}
		return g.VnCnLevel{
			Level: g.Level(buf[1]), MoodScope: g.Mood(buf[2]),
			Absolute: buf[3] != 0,
		}, 4, nil
	case 5:
		if len(buf) < 3 {
			return nil, 0, fmt.Errorf("VnCnAspect: short read")
		}
		return g.VnCnAspect{Aspect: g.Aspect(buf[1]), MoodScope: g.Mood(buf[2])}, 3, nil
	}
	return nil, 0, fmt.Errorf("unknown SlotVIII tag %d", buf[0])
}

// ----- affix -----

// writeAffix encodes an affix as [type byte][degree byte][2-byte index]
// when the Cs is in the default lexicon, or [type][degree][0xFFFF][cluster]
// (1-byte length + N phoneme bytes) when the Cs is not in the lexicon.
func writeAffix(buf *bytes.Buffer, a g.Affix) error {
	buf.WriteByte(byte(a.Type))
	buf.WriteByte(byte(a.Degree))
	if idx, ok := EncodeAffixIndex(a.Consonant); ok {
		buf.WriteByte(byte(idx >> 8))
		buf.WriteByte(byte(idx))
		return nil
	}
	buf.WriteByte(byte(AffixIndexUnknown >> 8))
	buf.WriteByte(byte(AffixIndexUnknown & 0xFF))
	c, err := EncodeCluster(a.Consonant)
	if err != nil {
		return fmt.Errorf("affix cluster fallback: %w", err)
	}
	buf.Write(c)
	return nil
}

func readAffix(buf []byte) (g.Affix, int, error) {
	if len(buf) < 4 {
		return g.Affix{}, 0, fmt.Errorf("affix: short header")
	}
	atype := g.AffixType(buf[0])
	degree := int(buf[1])
	idx := uint16(buf[2])<<8 | uint16(buf[3])
	if idx == AffixIndexUnknown {
		cs, n, err := DecodeCluster(buf[4:])
		if err != nil {
			return g.Affix{}, 0, fmt.Errorf("affix cluster fallback: %w", err)
		}
		return g.Affix{Type: atype, Degree: degree, Consonant: cs}, 4 + n, nil
	}
	cs, err := DecodeAffixIndex(idx)
	if err != nil {
		return g.Affix{}, 0, fmt.Errorf("affix index: %w", err)
	}
	return g.Affix{Type: atype, Degree: degree, Consonant: cs}, 4, nil
}

// ----- Vk -----

func writeVk(buf *bytes.Buffer, v g.Vk) error {
	switch x := v.(type) {
	case g.Assertive:
		buf.WriteByte(0)
		buf.WriteByte(byte(x.Validation))
	case g.Directive:
		buf.WriteByte(1)
	case g.Declarative:
		buf.WriteByte(2)
	case g.Interrogative:
		buf.WriteByte(3)
	case g.Verificative:
		buf.WriteByte(4)
	case g.Admonitive:
		buf.WriteByte(5)
	case g.Potentiative:
		buf.WriteByte(6)
	case g.Hortative:
		buf.WriteByte(7)
	case g.Conjectural:
		buf.WriteByte(8)
	default:
		return fmt.Errorf("unknown Vk type %T", v)
	}
	return nil
}

func readVk(buf []byte) (g.Vk, int, error) {
	if len(buf) < 1 {
		return nil, 0, fmt.Errorf("Vk: short read")
	}
	tag := buf[0]
	switch tag {
	case 0:
		if len(buf) < 2 {
			return nil, 0, fmt.Errorf("Assertive: short read")
		}
		return g.Assertive{Validation: g.Validation(buf[1])}, 2, nil
	case 1:
		return g.Directive{}, 1, nil
	case 2:
		return g.Declarative{}, 1, nil
	case 3:
		return g.Interrogative{}, 1, nil
	case 4:
		return g.Verificative{}, 1, nil
	case 5:
		return g.Admonitive{}, 1, nil
	case 6:
		return g.Potentiative{}, 1, nil
	case 7:
		return g.Hortative{}, 1, nil
	case 8:
		return g.Conjectural{}, 1, nil
	}
	return nil, 0, fmt.Errorf("unknown Vk tag %d", tag)
}

// ----- modular -----

func writeModular(buf *bytes.Buffer, m g.ModularAdjunct) error {
	buf.WriteByte(byte(m.Scope))
	buf.WriteByte(byte(m.Reach))
	buf.WriteByte(byte(len(m.Content)))
	for _, s := range m.Content {
		if err := writeSlotVIII(buf, s); err != nil {
			return fmt.Errorf("modular content: %w", err)
		}
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
		s, n, err := readSlotVIII(buf[cur:])
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
