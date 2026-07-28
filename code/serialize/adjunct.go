package serialize

import (
	"fmt"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/surface"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// Adjuncts, referentials, and the rest of the non-formative tokens,
// under the same three rules as formative.go:
//
//  1. Elide defaults. Every adjunct enum has its default at zero
//     (ModularScopeDefault, ModularReachNone, ScopeVDom, Carrier, THM),
//     so a flag bit can stand in for a byte.
//  2. Pack a slot into one byte when it fits. A modular adjunct's Scope,
//     Reach and content count are 2 + 3 + 2 bits, so they share a byte
//     instead of occupying three.
//  3. Spend bits where a field is narrower than a byte and the
//     saving is a whole byte, as the cluster encoding does.
//
// Variable-length runs carry a continuation bit in the high position of
// each element rather than a count prefix, so a run costs nothing when
// it holds one element. Affixes already worked this way; referent lists
// now do too, which also drops each referent from two bytes to one.

// ── referent runs ───────────────────────────────────────────────────
//
// Referent is 11 values and Effect is 3, so a (Referent, Effect) pair
// is 6 bits and leaves room for the continuation bit.

const refMore = 1 << 7

func putRefs(out []byte, refs []g.PersonalRef) ([]byte, error) {
	for i, r := range refs {
		if r.Referent > 15 {
			return nil, fmt.Errorf("referent %d exceeds 4 bits", r.Referent)
		}
		if r.Effect > 3 {
			return nil, fmt.Errorf("effect %d exceeds 2 bits", r.Effect)
		}
		b := byte(r.Referent) | byte(r.Effect)<<4
		if i < len(refs)-1 {
			b |= refMore
		}
		out = append(out, b)
	}
	return out, nil
}

func getRefs(buf []byte) ([]g.PersonalRef, int, error) {
	var out []g.PersonalRef
	for i := 0; i < len(buf); i++ {
		b := buf[i]
		out = append(out, g.PersonalRef{
			Referent: g.Referent(b & 0x0F),
			Effect:   g.RefEffect(b >> 4 & 0x03),
		})
		if b&refMore == 0 {
			return out, i + 1, nil
		}
	}
	return nil, 0, fmt.Errorf("referents: unterminated run")
}

// ── carrier ─────────────────────────────────────────────────────────
//
// CarrierType is 2 bits; Case needs 7 and is usually THM, so it gets a
// presence bit and is omitted at its default.

const carrierHasCase = 1 << 7

func putCarrier(out []byte, c g.CarrierAdjunct) []byte {
	b := byte(c.Type)
	if c.Case == g.THM {
		return append(out, b)
	}
	return append(out, b|carrierHasCase, byte(c.Case))
}

func getCarrier(buf []byte) (g.CarrierAdjunct, int, error) {
	if len(buf) == 0 {
		return g.CarrierAdjunct{}, 0, fmt.Errorf("carrier: short read")
	}
	c := g.CarrierAdjunct{Type: g.CarrierType(buf[0] & 0x03)}
	if buf[0]&carrierHasCase == 0 {
		return c, 1, nil
	}
	if len(buf) < 2 {
		return g.CarrierAdjunct{}, 0, fmt.Errorf("carrier: missing case")
	}
	c.Case = g.Case(buf[1])
	return c, 2, nil
}

// ── modular ─────────────────────────────────────────────────────────
//
// Scope (3 values), Reach (5 values) and the content count (0-3) are
// 2 + 3 + 2 bits, so the whole header is one byte.

func putModular(out []byte, m g.ModularAdjunct) ([]byte, error) {
	if len(m.Content) > 3 {
		return nil, fmt.Errorf("modular adjunct holds %d Vn/Cn pairs, max 3", len(m.Content))
	}
	out = append(out, byte(m.Scope)|byte(m.Reach)<<2|byte(len(m.Content))<<5)
	for _, s := range m.Content {
		var err error
		if out, err = putSlotVIII(out, s); err != nil {
			return nil, fmt.Errorf("modular content: %w", err)
		}
	}
	return out, nil
}

func getModular(buf []byte) (g.ModularAdjunct, int, error) {
	if len(buf) == 0 {
		return g.ModularAdjunct{}, 0, fmt.Errorf("modular: short read")
	}
	m := g.ModularAdjunct{
		Scope: g.ModularScope(buf[0] & 0x03),
		Reach: g.ModularReach(buf[0] >> 2 & 0x07),
	}
	n := int(buf[0] >> 5 & 0x03)
	cur := 1
	for i := 0; i < n; i++ {
		s, used, err := getSlotVIII(buf[cur:])
		if err != nil {
			return g.ModularAdjunct{}, 0, fmt.Errorf("modular content[%d]: %w", i, err)
		}
		cur += used
		m.Content = append(m.Content, s)
	}
	return m, cur, nil
}

// ── affixual adjuncts ───────────────────────────────────────────────
//
// AffixScope is 6 values, so the two scopes of a multiple-affix adjunct
// share a byte. First and Rest go out as one affix run and the decoder
// splits the head back off, which removes the separate count.

func putSingleAffix(out []byte, a g.SingleAffixAdjunct) ([]byte, error) {
	return putAffixes(append(out, byte(a.Scope)), []g.Affix{a.Affix})
}

func getSingleAffix(buf []byte) (g.SingleAffixAdjunct, int, error) {
	if len(buf) == 0 {
		return g.SingleAffixAdjunct{}, 0, fmt.Errorf("single affix: short read")
	}
	as, n, err := getAffixes(buf[1:])
	if err != nil {
		return g.SingleAffixAdjunct{}, 0, err
	}
	if len(as) != 1 {
		return g.SingleAffixAdjunct{}, 0, fmt.Errorf("single affix: got %d affixes", len(as))
	}
	return g.SingleAffixAdjunct{Affix: as[0], Scope: g.AffixScope(buf[0])}, 1 + n, nil
}

func putMultipleAffix(out []byte, m g.MultipleAffixAdjunct) ([]byte, error) {
	if m.FirstScope > 7 || m.RestScope > 7 {
		return nil, fmt.Errorf("affix scope exceeds 3 bits: %v/%v", m.FirstScope, m.RestScope)
	}
	out = append(out, byte(m.FirstScope)|byte(m.RestScope)<<3)
	return putAffixes(out, append([]g.Affix{m.First}, m.Rest...))
}

func getMultipleAffix(buf []byte) (g.MultipleAffixAdjunct, int, error) {
	if len(buf) == 0 {
		return g.MultipleAffixAdjunct{}, 0, fmt.Errorf("multi affix: short read")
	}
	as, n, err := getAffixes(buf[1:])
	if err != nil {
		return g.MultipleAffixAdjunct{}, 0, err
	}
	m := g.MultipleAffixAdjunct{
		First:      as[0],
		FirstScope: g.AffixScope(buf[0] & 0x07),
		RestScope:  g.AffixScope(buf[0] >> 3 & 0x07),
	}
	// Leave Rest nil rather than empty when the run held only the
	// head, so a decoded adjunct compares equal to the original.
	if len(as) > 1 {
		m.Rest = as[1:]
	}
	return m, 1 + n, nil
}

// ── referential ─────────────────────────────────────────────────────

const (
	refFlagCarrier = 1 << iota
	refFlagCategory
	refFlagCase
	refFlagCase2
	refFlagRefB
	refFlagRPV
	refFlagRefs
)

func putReferential(out []byte, r tokenize.ReferentialWord) ([]byte, error) {
	flags := byte(0)
	for _, f := range []struct {
		set  bool
		mask byte
	}{
		{r.Carrier != nil, refFlagCarrier},
		{r.Category != nil, refFlagCategory},
		{r.Case != nil, refFlagCase},
		{r.Case2 != nil, refFlagCase2},
		{len(r.RefB) > 0, refFlagRefB},
		{r.RpvEssence, refFlagRPV},
		{len(r.Refs) > 0, refFlagRefs},
	} {
		if f.set {
			flags |= f.mask
		}
	}
	out = append(out, flags)
	if r.Carrier != nil {
		out = append(out, byte(*r.Carrier))
	}
	if r.Category != nil {
		out = append(out, byte(*r.Category))
	}
	var err error
	if len(r.Refs) > 0 {
		if out, err = putRefs(out, r.Refs); err != nil {
			return nil, err
		}
	}
	if r.Case != nil {
		out = append(out, byte(*r.Case))
	}
	if r.Case2 != nil {
		out = append(out, byte(*r.Case2))
	}
	if len(r.RefB) > 0 {
		if out, err = putRefs(out, r.RefB); err != nil {
			return nil, err
		}
	}
	return out, nil
}

func getReferential(buf []byte) (tokenize.ReferentialWord, int, error) {
	var r tokenize.ReferentialWord
	if len(buf) == 0 {
		return r, 0, fmt.Errorf("referential: short read")
	}
	flags := buf[0]
	cur := 1
	take := func(what string) (byte, error) {
		if cur >= len(buf) {
			return 0, fmt.Errorf("referential %s: short read", what)
		}
		b := buf[cur]
		cur++
		return b, nil
	}
	if flags&refFlagCarrier != 0 {
		b, err := take("carrier")
		if err != nil {
			return r, 0, err
		}
		ct := g.CarrierType(b)
		r.Carrier = &ct
	}
	if flags&refFlagCategory != 0 {
		b, err := take("category")
		if err != nil {
			return r, 0, err
		}
		cat := g.RefCategory(b)
		r.Category = &cat
	}
	if flags&refFlagRefs != 0 {
		refs, n, err := getRefs(buf[cur:])
		if err != nil {
			return r, 0, err
		}
		cur += n
		r.Refs = refs
	}
	if flags&refFlagCase != 0 {
		b, err := take("case")
		if err != nil {
			return r, 0, err
		}
		c := g.Case(b)
		r.Case = &c
	}
	if flags&refFlagCase2 != 0 {
		b, err := take("case2")
		if err != nil {
			return r, 0, err
		}
		c := g.Case(b)
		r.Case2 = &c
	}
	if flags&refFlagRefB != 0 {
		refs, n, err := getRefs(buf[cur:])
		if err != nil {
			return r, 0, err
		}
		cur += n
		r.RefB = refs
	}
	r.RpvEssence = flags&refFlagRPV != 0
	return r, cur, nil
}

// ── combination referential ─────────────────────────────────────────

const (
	combFlagCarrier = 1 << iota
	combFlagCase
	combFlagCase2
	combFlagSpec
	combFlagAffixes
	combFlagRefs
)

func putCombinationRef(out []byte, c tokenize.CombinationRefWord) ([]byte, error) {
	flags := byte(0)
	for _, f := range []struct {
		set  bool
		mask byte
	}{
		{c.Carrier != nil, combFlagCarrier},
		{c.Case != g.THM, combFlagCase},
		{c.Case2 != nil, combFlagCase2},
		{c.Spec != g.BSC, combFlagSpec},
		{len(c.Affixes) > 0, combFlagAffixes},
		{len(c.Refs) > 0, combFlagRefs},
	} {
		if f.set {
			flags |= f.mask
		}
	}
	out = append(out, flags)
	if c.Carrier != nil {
		out = append(out, byte(*c.Carrier))
	}
	var err error
	if len(c.Refs) > 0 {
		if out, err = putRefs(out, c.Refs); err != nil {
			return nil, err
		}
	}
	if c.Case != g.THM {
		out = append(out, byte(c.Case))
	}
	if c.Spec != g.BSC {
		out = append(out, byte(c.Spec))
	}
	if len(c.Affixes) > 0 {
		if out, err = putAffixes(out, c.Affixes); err != nil {
			return nil, err
		}
	}
	if c.Case2 != nil {
		out = append(out, byte(*c.Case2))
	}
	return out, nil
}

func getCombinationRef(buf []byte) (tokenize.CombinationRefWord, int, error) {
	var c tokenize.CombinationRefWord
	if len(buf) == 0 {
		return c, 0, fmt.Errorf("combination ref: short read")
	}
	flags := buf[0]
	cur := 1
	take := func(what string) (byte, error) {
		if cur >= len(buf) {
			return 0, fmt.Errorf("combination ref %s: short read", what)
		}
		b := buf[cur]
		cur++
		return b, nil
	}
	if flags&combFlagCarrier != 0 {
		b, err := take("carrier")
		if err != nil {
			return c, 0, err
		}
		ct := g.CarrierType(b)
		c.Carrier = &ct
	}
	if flags&combFlagRefs != 0 {
		refs, n, err := getRefs(buf[cur:])
		if err != nil {
			return c, 0, err
		}
		cur += n
		c.Refs = refs
	}
	if flags&combFlagCase != 0 {
		b, err := take("case")
		if err != nil {
			return c, 0, err
		}
		c.Case = g.Case(b)
	}
	if flags&combFlagSpec != 0 {
		b, err := take("specification")
		if err != nil {
			return c, 0, err
		}
		c.Spec = g.Specification(b)
	}
	if flags&combFlagAffixes != 0 {
		as, n, err := getAffixes(buf[cur:])
		if err != nil {
			return c, 0, err
		}
		cur += n
		c.Affixes = as
	}
	if flags&combFlagCase2 != 0 {
		b, err := take("case2")
		if err != nil {
			return c, 0, err
		}
		cs := g.Case(b)
		c.Case2 = &cs
	}
	return c, cur, nil
}

func stressFromByte(b byte) surface.Stress { return surface.Stress(b) }

// ── foreign text ────────────────────────────────────────────────────
//
// A foreign word is the one token whose meaning genuinely is its text.
// §4.4 and §4.5 provide the CAR register and the carrier adjuncts so a
// speaker can drop a name or a quotation into Ithkuil without it being
// read as Ithkuil, and what those constructions carry is the letters
// themselves. Writing them verbatim is not a retreat from storing
// meaning; the letters are the meaning, and a formative's phoneme
// clusters are stored for the same reason.
//
// This is why UnknownWord is not encodable and never will be. A word
// we could not classify is a parse failure, not a meaning, and giving
// it a byte string here would let a document encode without complaint
// while quietly recording that we did not understand it.

func putForeign(out []byte, text string) []byte {
	out = appendUvarint(out, uint64(len(text)))
	return append(out, text...)
}

func getForeign(buf []byte) (string, int, error) {
	n, hdr, err := getUvarint(buf)
	if err != nil {
		return "", 0, fmt.Errorf("foreign word: %w", err)
	}
	end := hdr + int(n)
	if end > len(buf) {
		return "", 0, fmt.Errorf("foreign word: want %d bytes, have %d", n, len(buf)-hdr)
	}
	return string(buf[hdr:end]), end, nil
}
