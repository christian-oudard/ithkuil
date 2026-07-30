package serialize

import (
	"fmt"

	g "github.com/christian-oudard/ithkuil/grammar"
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

// ── referential ─────────────────────────────────────────────────

// A head is either a referent chain or a suppletive cluster, so one
// flag bit discriminates them and the payload that follows is whichever
// the bit selected. Case needs no presence bit of its own: §4.6.1
// requires one, so only a non-default value costs a byte.
// refFlagSuppletive is written by putRefHead, which both referential
// shapes share, so it has to mean the same bit in both flag bytes.
// Their own flags start above it.
const refFlagSuppletive = 1 << 0

const (
	refFlagCategory = 1 << (iota + 1)
	refFlagCase
	refFlagSecond
	refFlagSecondRefs
	refFlagRPV
)

func putRefHead(out []byte, head g.RefHead, flags *byte) ([]byte, error) {
	switch h := head.(type) {
	case g.SuppletiveHead:
		*flags |= refFlagSuppletive
		return append(out, byte(h.Type)), nil
	case g.PersonalHead:
		return putRefs(out, h.Refs)
	}
	return nil, fmt.Errorf("referential: unknown head %T", head)
}

func getRefHead(buf []byte, flags byte, cat *g.RefCategory) (g.RefHead, int, error) {
	if flags&refFlagSuppletive != 0 {
		if len(buf) == 0 {
			return nil, 0, fmt.Errorf("referential head: short read")
		}
		return g.SuppletiveHead{Type: g.CarrierType(buf[0])}, 1, nil
	}
	refs, n, err := getRefs(buf)
	if err != nil {
		return nil, 0, err
	}
	return g.PersonalHead{Refs: refs, Category: cat}, n, nil
}

// headCategory returns the category a personal head carries, or nil.
func headCategory(head g.RefHead) *g.RefCategory {
	if p, ok := head.(g.PersonalHead); ok {
		return p.Category
	}
	return nil
}

func putReferential(out []byte, r g.Referential) ([]byte, error) {
	ref := r
	cat := headCategory(ref.Head)
	flags := byte(0)
	for _, f := range []struct {
		set  bool
		mask byte
	}{
		{cat != nil, refFlagCategory},
		{ref.Case != g.THM, refFlagCase},
		{ref.Second != nil, refFlagSecond},
		{ref.Second != nil && len(ref.Second.Refs) > 0, refFlagSecondRefs},
		{ref.RpvEssence, refFlagRPV},
	} {
		if f.set {
			flags |= f.mask
		}
	}
	// The head is written into a scratch buffer first because encoding
	// it is what decides the suppletive flag, and the flag byte leads.
	var body []byte
	body, err := putRefHead(body, ref.Head, &flags)
	if err != nil {
		return nil, err
	}
	out = append(out, flags)
	if cat != nil {
		out = append(out, byte(*cat))
	}
	out = append(out, body...)
	if ref.Case != g.THM {
		out = append(out, byte(ref.Case))
	}
	if ref.Second != nil {
		out = append(out, byte(ref.Second.Case))
		if len(ref.Second.Refs) > 0 {
			if out, err = putRefs(out, ref.Second.Refs); err != nil {
				return nil, err
			}
		}
	}
	return out, nil
}

func getReferential(buf []byte) (g.Referential, int, error) {
	var r g.Referential
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
	var cat *g.RefCategory
	if flags&refFlagCategory != 0 {
		b, err := take("category")
		if err != nil {
			return r, 0, err
		}
		c := g.RefCategory(b)
		cat = &c
	}
	head, n, err := getRefHead(buf[cur:], flags, cat)
	if err != nil {
		return r, 0, err
	}
	cur += n
	ref := g.Referential{Head: head, Case: g.THM, RpvEssence: flags&refFlagRPV != 0}
	if flags&refFlagCase != 0 {
		b, err := take("case")
		if err != nil {
			return r, 0, err
		}
		ref.Case = g.Case(b)
	}
	if flags&refFlagSecond != 0 {
		b, err := take("second case")
		if err != nil {
			return r, 0, err
		}
		second := g.SecondReferent{Case: g.Case(b)}
		if flags&refFlagSecondRefs != 0 {
			refs, n, err := getRefs(buf[cur:])
			if err != nil {
				return r, 0, err
			}
			cur += n
			second.Refs = refs
		}
		ref.Second = &second
	}
	return ref, cur, nil
}

// ── combination referential ───────────────────────────────────

const (
	combFlagCategory = 1 << (iota + 1) // bit 0 is refFlagSuppletive
	combFlagCase
	combFlagCase2
	combFlagSpec
	combFlagAffixes
	combFlagRPV
)

func putCombinationRef(out []byte, c g.CombinationReferential) ([]byte, error) {
	comb := c
	cat := headCategory(comb.Head)
	flags := byte(0)
	for _, f := range []struct {
		set  bool
		mask byte
	}{
		{cat != nil, combFlagCategory},
		{comb.Case != g.THM, combFlagCase},
		{comb.Case2 != nil, combFlagCase2},
		{comb.Spec != g.BSC, combFlagSpec},
		{len(comb.Affixes) > 0, combFlagAffixes},
		{comb.RpvEssence, combFlagRPV},
	} {
		if f.set {
			flags |= f.mask
		}
	}
	var body []byte
	body, err := putRefHead(body, comb.Head, &flags)
	if err != nil {
		return nil, err
	}
	out = append(out, flags)
	if cat != nil {
		out = append(out, byte(*cat))
	}
	out = append(out, body...)
	if comb.Case != g.THM {
		out = append(out, byte(comb.Case))
	}
	if comb.Spec != g.BSC {
		out = append(out, byte(comb.Spec))
	}
	if len(comb.Affixes) > 0 {
		if out, err = putAffixes(out, comb.Affixes); err != nil {
			return nil, err
		}
	}
	if comb.Case2 != nil {
		out = append(out, byte(*comb.Case2))
	}
	return out, nil
}

func getCombinationRef(buf []byte) (g.CombinationReferential, int, error) {
	var c g.CombinationReferential
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
	var cat *g.RefCategory
	if flags&combFlagCategory != 0 {
		b, err := take("category")
		if err != nil {
			return c, 0, err
		}
		v := g.RefCategory(b)
		cat = &v
	}
	head, n, err := getRefHead(buf[cur:], flags, cat)
	if err != nil {
		return c, 0, err
	}
	cur += n
	comb := g.CombinationReferential{
		Head:       head,
		Case:       g.THM,
		Spec:       g.BSC,
		RpvEssence: flags&combFlagRPV != 0,
	}
	if flags&combFlagCase != 0 {
		b, err := take("case")
		if err != nil {
			return c, 0, err
		}
		comb.Case = g.Case(b)
	}
	if flags&combFlagSpec != 0 {
		b, err := take("specification")
		if err != nil {
			return c, 0, err
		}
		comb.Spec = g.Specification(b)
	}
	if flags&combFlagAffixes != 0 {
		as, n, err := getAffixes(buf[cur:])
		if err != nil {
			return c, 0, err
		}
		cur += n
		comb.Affixes = as
	}
	if flags&combFlagCase2 != 0 {
		b, err := take("case2")
		if err != nil {
			return c, 0, err
		}
		cs := g.Case(b)
		comb.Case2 = &cs
	}
	return comb, cur, nil
}

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
