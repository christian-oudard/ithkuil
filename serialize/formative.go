package serialize

import (
	"fmt"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// The formative encoding. Three rules produce the whole layout:
//
//  1. Elide defaults. Every grammatical enum in package grammar has its
//     default as the zero value, so a leading bitmap can say which
//     fields deviate and the rest cost nothing. This is the same trick
//     the romanization plays when it drops an unmarked slot, and it is
//     where nearly all of the compaction comes from: the median
//     formative sets two or three bits and writes a root cluster.
//
//  2. Pack a slot into one byte when the slot's total information fits
//     in one byte, rather than giving each field its own byte or its
//     own presence bit. Slot II + Slot IV is exactly 8 bits (Stem 2,
//     Version 1, Function 1, Specification 2, Context 2), so a presence
//     bitmap over those fields would cost more than it saves.
//
//  3. Stay byte-aligned. Packing fields across byte boundaries would
//     make the same formative encode to different bytes depending on
//     what preceded it, which destroys the repeated-substring matches a
//     general-purpose compressor runs on. Measured on a 61k-word
//     corpus, a byte-aligned encoding compresses to the same size as
//     the romanized text while being 35% smaller uncompressed;
//     regrouping the same bytes into per-field columns compresses 13%
//     worse.
//
// No lexicon indices appear anywhere. Roots and affixes encode as their
// phoneme clusters, so a file stays readable across lexicon updates.

// Header bits. A set bit means the field is present or non-default.
// Bits 1 and 7 together form a 2-bit root-kind field.
const (
	fConcat  = 1 << 0
	fRootLo  = 1 << 1
	fSlotV   = 1 << 2
	fCa      = 1 << 3
	fSlotVII = 1 << 4
	fSlotIX  = 1 << 5 // Slot VIII (Vn + Cn)
	fFinal   = 1 << 6 // Final is not UnframedNominal{THM}
	fRootHi  = 1 << 7
)

// Root kinds. rootPlain is a CrRoot with every Slot II and Slot IV
// field at its default; it writes no root byte at all.
const (
	rootPlain = iota
	rootCr
	rootCs
	rootRef
)

func rootKind(flags byte) int {
	k := 0
	if flags&fRootLo != 0 {
		k |= 1
	}
	if flags&fRootHi != 0 {
		k |= 2
	}
	return k
}

// ── Slot VI (Ca) ────────────────────────────────────────────────────
//
// Five sub-fields, each with a zero default, each fitting in 5 bits
// (Configuration is widest at 20 values). One non-default field is by
// far the common case, so the selector goes in the high 3 bits and the
// value in the low 5. Selector 0 escapes to a mixed-radix pack.

var caRadix = [5]int{20, 4, 4, 6, 2} // Config, Affil, Persp, Ext, Essence

func putCa(out []byte, c g.SlotVI) []byte {
	f := [5]int{int(c.Configuration), int(c.Affiliation),
		int(c.Perspective), int(c.Extension), int(c.Essence)}
	nz, idx := 0, 0
	for i, v := range f {
		if v != 0 {
			nz++
			idx = i
		}
	}
	if nz == 1 {
		return append(out, byte(idx+1)<<5|byte(f[idx]))
	}
	packed := 0
	for i := 4; i >= 0; i-- {
		packed = packed*caRadix[i] + f[i]
	}
	return appendUvarint(append(out, 0), uint64(packed))
}

func getCa(buf []byte) (g.SlotVI, int, error) {
	if len(buf) == 0 {
		return g.SlotVI{}, 0, fmt.Errorf("Ca: short read")
	}
	var f [5]int
	n := 1
	if sel := buf[0] >> 5; sel != 0 {
		f[sel-1] = int(buf[0] & 0x1F)
	} else {
		packed, m, err := getUvarint(buf[1:])
		if err != nil {
			return g.SlotVI{}, 0, fmt.Errorf("Ca: %w", err)
		}
		n += m
		for i := 0; i < 5; i++ {
			f[i] = int(packed) % caRadix[i]
			packed /= uint64(caRadix[i])
		}
	}
	for i, v := range f {
		if v >= caRadix[i] {
			return g.SlotVI{}, 0, fmt.Errorf("Ca: field %d value %d out of range", i, v)
		}
	}
	return g.SlotVI{
		Configuration: g.Configuration(f[0]), Affiliation: g.Affiliation(f[1]),
		Perspective: g.Perspective(f[2]), Extension: g.Extension(f[3]),
		Essence: g.Essence(f[4]),
	}, n, nil
}

// ── Slot VIII (Vn + Cn) ─────────────────────────────────────────────
//
// Variant tag in the high 3 bits, payload in the low 5. Aspect is the
// only payload wider than 5 bits (36 values), so tag 7 carries its
// upper half. Tag 6 escapes to the long form, needed only for a
// non-default Mood/Case-Scope or the Level Absolute flag.

const (
	slotVIIIEscape   = 6
	slotVIIIAspectHi = 7
)

func putSlotVIII(out []byte, s g.SlotVIII) ([]byte, error) {
	var tag, val, mood int
	abs := false
	switch v := s.(type) {
	case g.VnCnValence:
		tag, val, mood = 1, int(v.Valence), int(v.MoodScope)
	case g.VnCnPhase:
		tag, val, mood = 2, int(v.Phase), int(v.MoodScope)
	case g.VnCnEffect:
		tag, val, mood = 3, int(v.Effect), int(v.MoodScope)
	case g.VnCnLevel:
		tag, val, mood, abs = 4, int(v.Level), int(v.MoodScope), v.Absolute
	case g.VnCnAspect:
		tag, val, mood = 5, int(v.Aspect), int(v.MoodScope)
	default:
		return nil, fmt.Errorf("unknown SlotVIII type %T", s)
	}
	if mood == 0 && !abs {
		if val < 32 {
			return append(out, byte(tag)<<5|byte(val)), nil
		}
		if tag == 5 && val < 64 {
			return append(out, byte(slotVIIIAspectHi)<<5|byte(val-32)), nil
		}
	}
	absByte := byte(0)
	if abs {
		absByte = 1
	}
	return append(out, byte(slotVIIIEscape)<<5, byte(tag), byte(val), byte(mood), absByte), nil
}

func getSlotVIII(buf []byte) (g.SlotVIII, int, error) {
	if len(buf) == 0 {
		return nil, 0, fmt.Errorf("SlotVIII: short read")
	}
	tag, val := int(buf[0]>>5), int(buf[0]&0x1F)
	mood, abs, n := 0, false, 1
	switch tag {
	case slotVIIIAspectHi:
		tag, val = 5, val+32
	case slotVIIIEscape:
		if len(buf) < 5 {
			return nil, 0, fmt.Errorf("SlotVIII escape: short read")
		}
		tag, val, mood, abs, n = int(buf[1]), int(buf[2]), int(buf[3]), buf[4] != 0, 5
	}
	switch tag {
	case 1:
		return g.VnCnValence{Valence: g.Valence(val), MoodScope: g.Mood(mood)}, n, nil
	case 2:
		return g.VnCnPhase{Phase: g.Phase(val), MoodScope: g.Mood(mood)}, n, nil
	case 3:
		return g.VnCnEffect{Effect: g.Effect(val), MoodScope: g.Mood(mood)}, n, nil
	case 4:
		return g.VnCnLevel{Level: g.Level(val), MoodScope: g.Mood(mood), Absolute: abs}, n, nil
	case 5:
		return g.VnCnAspect{Aspect: g.Aspect(val), MoodScope: g.Mood(mood)}, n, nil
	}
	return nil, 0, fmt.Errorf("SlotVIII: bad variant tag %d", tag)
}

// ── Final ───────────────────────────────────────────────────────────
//
// One flat byte covering all three variants, with no separate variant
// tag: 0..67 nominal case, 68..135 framed-verbal case, 136..144
// assertive by validation, 145..152 the eight leaf illocutions.

const numCases = 68

// leafVk lists the eight illocutions that carry no Validation, in the
// order their byte values follow the nine assertive validations.
var leafVk = []g.Vk{
	g.Directive{}, g.Declarative{}, g.Interrogative{}, g.Verificative{},
	g.Admonitive{}, g.Potentiative{}, g.Hortative{}, g.Conjectural{},
}

func putFinal(out []byte, f g.Final) ([]byte, error) {
	switch v := f.(type) {
	case g.UnframedNominal:
		return append(out, byte(v.Case)), nil
	case g.FramedVerbal:
		return append(out, byte(numCases+int(v.Case))), nil
	case g.UnframedVerbal:
		if a, ok := v.Vk.(g.Assertive); ok {
			return append(out, byte(2*numCases+int(a.Validation))), nil
		}
		for i, k := range leafVk {
			if k == v.Vk {
				return append(out, byte(2*numCases+9+i)), nil
			}
		}
		return nil, fmt.Errorf("unknown Vk type %T", v.Vk)
	}
	return nil, fmt.Errorf("unknown Final type %T", f)
}

func getFinal(b byte) (g.Final, error) {
	n := int(b)
	switch {
	case n < numCases:
		return g.UnframedNominal{Case: g.Case(n)}, nil
	case n < 2*numCases:
		return g.FramedVerbal{Case: g.Case(n - numCases)}, nil
	case n < 2*numCases+9:
		return g.UnframedVerbal{Vk: g.Assertive{Validation: g.Validation(n - 2*numCases)}}, nil
	case n < 2*numCases+9+len(leafVk):
		return g.UnframedVerbal{Vk: leafVk[n-2*numCases-9]}, nil
	}
	return nil, fmt.Errorf("Final: byte %d out of range", b)
}

// ── affixes ─────────────────────────────────────────────────────────
//
// Type (2 bits) and Degree (4 bits) share one byte with a continuation
// bit in the high position, so a run of affixes needs no count prefix.
// The Cs cluster follows.

const affixMore = 1 << 7

func putAffixes(out []byte, as []g.Affix) ([]byte, error) {
	for i, a := range as {
		if a.Degree > 15 {
			return nil, fmt.Errorf("affix degree %d exceeds 4 bits", a.Degree)
		}
		if a.Type > 3 {
			return nil, fmt.Errorf("affix type %v exceeds 2 bits", a.Type)
		}
		b := byte(a.Type)<<4 | byte(a.Degree)
		if i < len(as)-1 {
			b |= affixMore
		}
		var err error
		if out, err = putCluster(append(out, b), a.Consonant); err != nil {
			return nil, fmt.Errorf("affix Cs: %w", err)
		}
	}
	return out, nil
}

func getAffixes(buf []byte) ([]g.Affix, int, error) {
	var out []g.Affix
	cur := 0
	for {
		if cur >= len(buf) {
			return nil, 0, fmt.Errorf("affixes: short read")
		}
		b := buf[cur]
		cur++
		cs, n, err := getCluster(buf[cur:])
		if err != nil {
			return nil, 0, fmt.Errorf("affix Cs: %w", err)
		}
		cur += n
		out = append(out, g.Affix{
			Type: g.AffixType(b >> 4 & 0x03), Degree: int(b & 0x0F), Consonant: cs,
		})
		if b&affixMore == 0 {
			return out, cur, nil
		}
	}
}

// ── formative ───────────────────────────────────────────────────────

func putFormative(out []byte, f g.Formative) ([]byte, error) {
	flags := byte(0)
	if f.Concat != g.ConcatNone {
		// A written Concat byte is always 1 or 2. Package token relies
		// on 0 being unreachable there to escape non-formative tokens.
		if f.Concat != g.Type1 && f.Concat != g.Type2 {
			return nil, fmt.Errorf("concatenation status out of range: %d", f.Concat)
		}
		flags |= fConcat
	}

	var cluster string
	var rootByte byte
	kind := rootPlain
	switch r := f.Root.(type) {
	case g.CrRoot:
		cluster = r.Cluster
		rootByte = byte(r.Stem) | byte(r.Version)<<2 |
			byte(r.SlotIV.Function)<<3 | byte(r.SlotIV.Specification)<<4 |
			byte(r.SlotIV.Context)<<6
		if rootByte != 0 {
			kind = rootCr
		}
	case g.CsRoot:
		cluster, kind = r.Cs, rootCs
		if r.Degree > 15 {
			return nil, fmt.Errorf("Cs-root degree %d exceeds 4 bits", r.Degree)
		}
		rootByte = byte(r.Degree) | byte(r.Version)<<4 |
			byte(r.Function)<<5 | byte(r.Context)<<6
	case g.RefRoot:
		cluster, kind = r.C1, rootRef
		rootByte = byte(r.Version) | byte(r.SlotIV.Function)<<1 |
			byte(r.SlotIV.Specification)<<2 | byte(r.SlotIV.Context)<<4
	default:
		return nil, fmt.Errorf("unknown Root type %T", f.Root)
	}
	if kind&1 != 0 {
		flags |= fRootLo
	}
	if kind&2 != 0 {
		flags |= fRootHi
	}

	if len(f.SlotV) > 0 {
		flags |= fSlotV
	}
	if f.SlotVI != g.DefaultSlotVI {
		flags |= fCa
	}
	if len(f.SlotVII) > 0 {
		flags |= fSlotVII
	}
	if f.SlotVIII != nil {
		flags |= fSlotIX
	}
	if fin, ok := f.Final.(g.UnframedNominal); !ok || fin.Case != g.THM {
		flags |= fFinal
	}

	out = append(out, flags)
	if flags&fConcat != 0 {
		out = append(out, byte(f.Concat))
	}
	if kind != rootPlain {
		out = append(out, rootByte)
	}
	var err error
	if out, err = putCluster(out, cluster); err != nil {
		return nil, fmt.Errorf("root cluster: %w", err)
	}
	if flags&fSlotV != 0 {
		if out, err = putAffixes(out, f.SlotV); err != nil {
			return nil, fmt.Errorf("Slot V: %w", err)
		}
	}
	if flags&fCa != 0 {
		out = putCa(out, f.SlotVI)
	}
	if flags&fSlotVII != 0 {
		if out, err = putAffixes(out, f.SlotVII); err != nil {
			return nil, fmt.Errorf("Slot VII: %w", err)
		}
	}
	if flags&fSlotIX != 0 {
		if out, err = putSlotVIII(out, f.SlotVIII); err != nil {
			return nil, fmt.Errorf("Slot VIII: %w", err)
		}
	}
	if flags&fFinal != 0 {
		if out, err = putFinal(out, f.Final); err != nil {
			return nil, fmt.Errorf("Final: %w", err)
		}
	}
	return out, nil
}

func getFormative(buf []byte) (g.Formative, int, error) {
	var f g.Formative
	if len(buf) == 0 {
		return f, 0, fmt.Errorf("formative: empty input")
	}
	flags := buf[0]
	cur := 1
	take := func(what string) (byte, error) {
		if cur >= len(buf) {
			return 0, fmt.Errorf("%s: short read", what)
		}
		b := buf[cur]
		cur++
		return b, nil
	}

	if flags&fConcat != 0 {
		b, err := take("Concat")
		if err != nil {
			return f, 0, err
		}
		if b != byte(g.Type1) && b != byte(g.Type2) {
			return f, 0, fmt.Errorf("concatenation status out of range: %d", b)
		}
		f.Concat = g.ConcatenationStatus(b)
	}

	kind := rootKind(flags)
	rootByte := byte(0)
	if kind != rootPlain {
		var err error
		if rootByte, err = take("root byte"); err != nil {
			return f, 0, err
		}
	}
	cluster, n, err := getCluster(buf[cur:])
	if err != nil {
		return f, 0, fmt.Errorf("root cluster: %w", err)
	}
	cur += n
	switch kind {
	case rootPlain, rootCr:
		f.Root = g.CrRoot{
			Cluster: cluster,
			Stem:    g.Stem(rootByte & 0x03),
			Version: g.Version(rootByte >> 2 & 0x01),
			SlotIV: g.SlotIV{
				Function:      g.Function(rootByte >> 3 & 0x01),
				Specification: g.Specification(rootByte >> 4 & 0x03),
				Context:       g.Context(rootByte >> 6 & 0x03),
			},
		}
	case rootCs:
		f.Root = g.CsRoot{
			Cs:       cluster,
			Degree:   int(rootByte & 0x0F),
			Version:  g.Version(rootByte >> 4 & 0x01),
			Function: g.Function(rootByte >> 5 & 0x01),
			Context:  g.Context(rootByte >> 6 & 0x03),
		}
	case rootRef:
		f.Root = g.RefRoot{
			C1:      cluster,
			Version: g.Version(rootByte & 0x01),
			SlotIV: g.SlotIV{
				Function:      g.Function(rootByte >> 1 & 0x01),
				Specification: g.Specification(rootByte >> 2 & 0x03),
				Context:       g.Context(rootByte >> 4 & 0x03),
			},
		}
	}

	if flags&fSlotV != 0 {
		as, n, err := getAffixes(buf[cur:])
		if err != nil {
			return f, 0, fmt.Errorf("Slot V: %w", err)
		}
		cur += n
		f.SlotV = as
	}
	f.SlotVI = g.DefaultSlotVI
	if flags&fCa != 0 {
		ca, n, err := getCa(buf[cur:])
		if err != nil {
			return f, 0, err
		}
		cur += n
		f.SlotVI = ca
	}
	if flags&fSlotVII != 0 {
		as, n, err := getAffixes(buf[cur:])
		if err != nil {
			return f, 0, fmt.Errorf("Slot VII: %w", err)
		}
		cur += n
		f.SlotVII = as
	}
	if flags&fSlotIX != 0 {
		s, n, err := getSlotVIII(buf[cur:])
		if err != nil {
			return f, 0, err
		}
		cur += n
		f.SlotVIII = s
	}
	f.Final = g.UnframedNominal{Case: g.THM}
	if flags&fFinal != 0 {
		b, err := take("Final")
		if err != nil {
			return f, 0, err
		}
		if f.Final, err = getFinal(b); err != nil {
			return f, 0, err
		}
	}
	return f, cur, nil
}

// ── varint ──────────────────────────────────────────────────────────

func appendUvarint(out []byte, v uint64) []byte {
	for v >= 0x80 {
		out = append(out, byte(v)|0x80)
		v >>= 7
	}
	return append(out, byte(v))
}

func getUvarint(buf []byte) (uint64, int, error) {
	var v uint64
	var shift uint
	for i := 0; i < len(buf); i++ {
		v |= uint64(buf[i]&0x7F) << shift
		if buf[i]&0x80 == 0 {
			return v, i + 1, nil
		}
		shift += 7
	}
	return 0, 0, fmt.Errorf("truncated uvarint")
}
