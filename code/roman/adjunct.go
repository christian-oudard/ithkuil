package roman

import (
	"fmt"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/slots"
)

// SingleAffixAdjunct writes a §4.1.1 single-affix adjunct, V_X C_S
// [V_S]. The scope vowel is omitted at its default, so the shortest
// form is two conjuncts ("ač"). Inverse of parse.ParseSingleAffix.
func SingleAffixAdjunct(a g.SingleAffixAdjunct) (string, error) {
	if a.Affix.Consonant == "" {
		return "", fmt.Errorf("single-affix adjunct: affix has no Cs cluster")
	}
	// §1.6's footnote depends on what precedes the vowel, so it
	// applies to the assembled word. See phonology.DissimilateGlides.
	return phonology.DissimilateGlides(
		parse.AffixVowel(a.Affix.Type, a.Affix.Degree) + a.Affix.Consonant + parse.VsForm(a.Scope)), nil
}

// MultipleAffixAdjunct writes a §4.1.2 multiple-affix adjunct,
// [ë] C_S V_X C_Z (V_X C_S)+ [V_Z]. The first affix is written in
// reverse order, consonant before vowel, which with C_Z is what tells
// the class apart. Inverse of parse.ParseMultipleAffix.
func MultipleAffixAdjunct(a g.MultipleAffixAdjunct) (string, error) {
	if a.First.Consonant == "" {
		return "", fmt.Errorf("multiple-affix adjunct: first affix has no Cs cluster")
	}
	if len(a.Rest) == 0 {
		return "", fmt.Errorf("multiple-affix adjunct: needs a second affix; use a single-affix adjunct for one")
	}
	var b strings.Builder
	b.WriteString(a.First.Consonant)
	b.WriteString(parse.AffixVowel(a.First.Type, a.First.Degree))
	b.WriteString(parse.CzForm(a.FirstScope))
	for _, af := range a.Rest {
		if af.Consonant == "" {
			return "", fmt.Errorf("multiple-affix adjunct: trailing affix has no Cs cluster")
		}
		b.WriteString(parse.AffixVowel(af.Type, af.Degree))
		b.WriteString(af.Consonant)
	}
	b.WriteString(parse.VzForm(a.RestScope, a.FirstScope))
	// §4.1.2 allows a leading ë- "if phonotactically necessary", and the
	// reversed first affix is what makes it necessary: a Cs that cannot
	// open a word opens this one. Ask the phonotactics rather than
	// guessing which clusters those are.
	word := b.String()
	if phonology.Legal(word) {
		return phonology.DissimilateGlides(word), nil
	}
	if prefixed := "ë" + word; phonology.Legal(prefixed) {
		return prefixed, nil
	}
	return "", fmt.Errorf("multiple-affix adjunct: %q is unpronounceable with or without the ë- prefix", word)
}

// ModularAdjunct writes a §4.3 modular adjunct:
//
//	1  ' or w- or y-   scope over parent or concatenated formative
//	2  (V_N C_N)       optional
//	3  (V_N C_M)       optional
//	4  V_N or V_H      mandatory
//
// Content fills slots 2, 3 and 4 in order. Slot 3's C_M and slot 4's
// bare vowel carry no Mood/Case-Scope of their own — §4.3 spends C_M
// only on "n if V_N represents an Aspect, otherwise ň" — so a value
// placed there whose mood is not the default cannot be written, and
// this says so instead of dropping it. Inverse of parse.ParseModular.
//
// ModularParts hands out the same split for a reader that needs to know
// which letters went where.
func ModularAdjunct(m g.ModularAdjunct) (string, error) {
	if len(m.Content) == 0 {
		return "", fmt.Errorf("modular adjunct: no content to write")
	}
	// Slot 4 holds a V_H reach or one Content value, never both. With a
	// reach, every Content value needs a pair, and there are two pair
	// slots; without one, the last value may take Slot 4 as a bare vowel
	// if it is the kind ParseModular reads there, which is what makes
	// three values fit and what writes "yu" as two letters.
	pairs, slot4 := m.Content, -1
	if m.Reach == g.ModularReachNone && fitsSlot4(m.Content[len(m.Content)-1]) {
		slot4 = len(m.Content) - 1
		pairs = m.Content[:slot4]
	}
	if len(pairs) > 2 {
		return "", fmt.Errorf("modular adjunct: %d values need %d pair slots, and §4.3 has 2",
			len(m.Content), len(pairs))
	}

	var b strings.Builder
	b.WriteString(ModularScopePrefix(m.Scope))
	for i, s := range pairs {
		vn, cn := slots.VnCnFromSlotVIII(s)
		b.WriteString(vn)
		if i == 0 {
			b.WriteString(cn)
			continue
		}
		// Slot 3's C_M is spent entirely on "n if V_N represents an
		// Aspect, otherwise ň", so it cannot also carry the C_N that
		// Slot 2 would have written.
		if g.SlotVIIIMoodScope(s) != g.FAC {
			return "", fmt.Errorf("modular adjunct: Slot 3 has no C_N, so %v Mood/Case-Scope cannot be written there",
				g.SlotVIIIMoodScope(s))
		}
		if _, isAspect := s.(g.VnCnAspect); isAspect {
			b.WriteString("n")
		} else {
			b.WriteString("ň")
		}
	}
	if slot4 >= 0 {
		vn, _ := slots.VnCnFromSlotVIII(m.Content[slot4])
		b.WriteString(vn)
		return phonology.DissimilateGlides(b.String()), nil
	}
	if m.Reach == g.ModularReachNone {
		return phonology.DissimilateGlides(b.String()), nil
	}
	// §4.3: the trailing vowel reads as V_H only under ultimate stress.
	b.WriteString(reachVH(m.Reach))
	return phonology.Apply(phonology.DissimilateGlides(b.String()), phonology.Ultimate), nil
}

// ModularScopePrefix returns the §4.3 Slot 1 consonant for a scope, or
// "" for the default, which writes nothing.
func ModularScopePrefix(s g.ModularScope) string {
	switch s {
	case g.ModularScopeParent:
		return "w"
	case g.ModularScopeConcat:
		return "y"
	}
	return ""
}

// ModularPart is one §4.3 slot of a modular adjunct as written.
//
// The slots are not alike, which is the whole reason this is handed out
// rather than re-derived. Slot 2 writes a full C_N. Slot 3 spends its
// consonant entirely on C_M, "n if V_N represents an Aspect, otherwise
// ň", so there is no C_N in it. Slot 4 is a bare vowel with no
// consonant at all. A reader that asks for a (V_N, C_N) pair per entry
// looks for a C_N that Slot 3 never wrote, reports it elided, and
// leaves the n that is there unaccounted for.
type ModularPart struct {
	Slot  int        // §4.3 slot number: 2, 3 or 4
	Vn    string     // as written, after glide dissimilation
	Cn    string     // C_N in Slot 2, C_M in Slot 3, empty in Slot 4
	Value g.SlotVIII // the entry this slot writes; nil for a V_H Slot 4
}

// ModularParts returns the §4.3 split ModularAdjunct writes, each piece
// spelled as it appears in the word. The scope prefix is not a part:
// it belongs to Slot 1 and carries no V_N.
//
// The split is read back off the finished word rather than collected on
// the way in, because DissimilateGlides rewrites a vowel that follows a
// glide (yia becomes yuä) and the pieces have to be the letters that
// survived that. Conjuncts alternate consonant and vowel here, so
// walking them back is exact; anything left over is a disagreement
// between this and ModularAdjunct, and it says so rather than handing
// back a split that does not add up.
func ModularParts(m g.ModularAdjunct) ([]ModularPart, error) {
	word, err := ModularAdjunct(m)
	if err != nil {
		return nil, err
	}
	shape, err := modularShape(m)
	if err != nil {
		return nil, err
	}
	bare, _ := phonology.Strip(word)
	conjs := phonology.SplitConjuncts(bare)
	if ModularScopePrefix(m.Scope) != "" {
		if len(conjs) == 0 {
			return nil, fmt.Errorf("modular adjunct %q: no scope prefix to read", word)
		}
		conjs = conjs[1:]
	}
	out := make([]ModularPart, 0, len(shape))
	for _, p := range shape {
		if len(conjs) == 0 {
			return nil, fmt.Errorf("modular adjunct %q: slot %d has no vowel left to read", word, p.Slot)
		}
		p.Vn, conjs = conjs[0], conjs[1:]
		if p.Cn != "" {
			if len(conjs) == 0 {
				return nil, fmt.Errorf("modular adjunct %q: slot %d has no consonant left to read", word, p.Slot)
			}
			p.Cn, conjs = conjs[0], conjs[1:]
		}
		out = append(out, p)
	}
	if len(conjs) > 0 {
		return nil, fmt.Errorf("modular adjunct %q: %v is written but belongs to no slot", word, conjs)
	}
	return out, nil
}

// modularShape lists the slots Content fills and which of them spend a
// consonant, without spelling anything: ModularParts takes the letters
// from the word. A V_H Slot 4 is one of these too, holding a reach
// rather than a Content value.
func modularShape(m g.ModularAdjunct) ([]ModularPart, error) {
	if len(m.Content) == 0 {
		return nil, fmt.Errorf("modular adjunct: no content to write")
	}
	pairs, slot4 := m.Content, -1
	if m.Reach == g.ModularReachNone && fitsSlot4(m.Content[len(m.Content)-1]) {
		slot4 = len(m.Content) - 1
		pairs = m.Content[:slot4]
	}
	var out []ModularPart
	for i, s := range pairs {
		// Cn is a placeholder here; ModularParts replaces it with the
		// conjunct actually written. Only its emptiness is read, to say
		// whether this slot spends a consonant at all.
		out = append(out, ModularPart{Slot: i + 2, Vn: "", Cn: "?", Value: s})
	}
	if slot4 >= 0 {
		out = append(out, ModularPart{Slot: 4, Value: m.Content[slot4]})
	} else if m.Reach != g.ModularReachNone {
		out = append(out, ModularPart{Slot: 4})
	}
	return out, nil
}

// fitsSlot4 reports whether a value can be written as §4.3's bare Slot 4
// vowel. ParseModular reads that vowel as an Aspect at the default
// Mood/Case-Scope, because there is no C_N beside it to say otherwise,
// so nothing else survives the trip.
func fitsSlot4(s g.SlotVIII) bool {
	_, isAspect := s.(g.VnCnAspect)
	return isAspect && g.SlotVIIIMoodScope(s) == g.FAC
}

// reachVH writes a §4.3 Slot 4 V_H vowel. "i" and "u" both read as the
// formative reach; "i" is the one written back.
func reachVH(r g.ModularReach) string {
	switch r {
	case g.ModularReachCaseMoodIll:
		return "a"
	case g.ModularReachCaseMood:
		return "e"
	case g.ModularReachFormative:
		return "i"
	case g.ModularReachAdjacent:
		return "o"
	}
	return ""
}
