package parse

import (
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// c1Table maps each (Referent, Effect) pair to its consonant form, per
// the §4.6 table.
var c1Table = map[g.PersonalRef]string{
	{Referent: g.R1m, Effect: g.NEU}: "l", {Referent: g.R1m, Effect: g.BEN}: "r", {Referent: g.R1m, Effect: g.DET}: "ř",
	{Referent: g.R2m, Effect: g.NEU}: "s", {Referent: g.R2m, Effect: g.BEN}: "š", {Referent: g.R2m, Effect: g.DET}: "ž",
	{Referent: g.R2p, Effect: g.NEU}: "n", {Referent: g.R2p, Effect: g.BEN}: "t", {Referent: g.R2p, Effect: g.DET}: "d",
	{Referent: g.Rma, Effect: g.NEU}: "m", {Referent: g.Rma, Effect: g.BEN}: "p", {Referent: g.Rma, Effect: g.DET}: "b",
	{Referent: g.Rpa, Effect: g.NEU}: "ň", {Referent: g.Rpa, Effect: g.BEN}: "k", {Referent: g.Rpa, Effect: g.DET}: "g",
	{Referent: g.Rmi, Effect: g.NEU}: "z", {Referent: g.Rmi, Effect: g.BEN}: "ţ", {Referent: g.Rmi, Effect: g.DET}: "ḑ",
	{Referent: g.Rpi, Effect: g.NEU}: "ẓ", {Referent: g.Rpi, Effect: g.BEN}: "f", {Referent: g.Rpi, Effect: g.DET}: "v",
	{Referent: g.Rmx, Effect: g.NEU}: "c", {Referent: g.Rmx, Effect: g.BEN}: "č", {Referent: g.Rmx, Effect: g.DET}: "j",
	{Referent: g.Rrdp, Effect: g.NEU}: "th", {Referent: g.Rrdp, Effect: g.BEN}: "ph", {Referent: g.Rrdp, Effect: g.DET}: "kh",
	{Referent: g.Robv, Effect: g.NEU}: "ll", {Referent: g.Robv, Effect: g.BEN}: "rr", {Referent: g.Robv, Effect: g.DET}: "řř",
	{Referent: g.Rpvs, Effect: g.NEU}: "mm", {Referent: g.Rpvs, Effect: g.BEN}: "nn", {Referent: g.Rpvs, Effect: g.DET}: "ňň",
}

// refAffixAlternates are the second forms the §4.6 table gives for the
// Obviative and Provisional rows. Its footnote confines them to §4.6.5
// referential affixes, "to avoid ambiguity with geminated C_A forms",
// so they decode only through DecomposeRefAffixCs. Admitting them
// everywhere would collide with the category affixes: "lç" is the
// Nomic modifier on 1m, and only the narrower entry point can tell the
// two readings apart.
//
// The collision is total, not incidental. §4.6 shows the NOMIC
// category by adding -ç- to a referent, so all six alternates are
// exactly what that produces: lç is 1m/NEU + NOMIC, rç is 1m/BEN +
// NOMIC, mç is ma/NEU + NOMIC, nç is 2p/NEU + NOMIC, ňç is pa/NEU +
// NOMIC. The alternates were introduced to remove an ambiguity with
// geminated C_A forms and introduced a different one.
//
// mç is the worst of them, because §4.6 does not merely permit the
// second reading, it prescribes it: "The IPa and IPi Impersonal
// categories ... will instead be shown by adding the NOMIC affix above
// to the ma or mi affixes." Nothing rules it out either — §4.6.5 bars
// exactly one increment in a referential affix, ABSTRACT -w or -y, and
// says nothing about NOMIC.
//
// Inside a referential affix this map wins, so mç reads as PVS. That
// is a choice: it keeps the footnote's own purpose (the alternates
// exist precisely so an affix can spell Obv and PVS) and leaves the
// ma+NOMIC sense to the ordinary spelling, which the category path
// still produces everywhere else.
var refAffixAlternates = map[string]g.PersonalRef{
	"lç": {Referent: g.Robv, Effect: g.NEU},
	"rç": {Referent: g.Robv, Effect: g.BEN},
	"řç": {Referent: g.Robv, Effect: g.DET},
	"mç": {Referent: g.Rpvs, Effect: g.NEU},
	"nç": {Referent: g.Rpvs, Effect: g.BEN},
	"ňç": {Referent: g.Rpvs, Effect: g.DET},
}

// RefC1 returns the consonant form spelling a PersonalRef.
func RefC1(p g.PersonalRef) string { return c1Table[p] }

// c1Reverse is the decode direction of c1Table, built once at init.
var c1Reverse = func() map[string]g.PersonalRef {
	m := make(map[string]g.PersonalRef, len(c1Table))
	for ref, form := range c1Table {
		m[form] = ref
	}
	return m
}()

// LookupRefC1 decodes a single referential consonant form, mono- or
// biconsonantal, to its PersonalRef.
func LookupRefC1(c string) (g.PersonalRef, bool) {
	p, ok := c1Reverse[c]
	return p, ok
}

// biconsonantalForms enumerates the two-character referential forms.
// Greedy decomposition checks these first so a leading "ll" reads as
// Obv/NEU rather than two 1m/NEU in a row. "tļ" is listed because it
// is the Agglomerative modifier: matching it here and then failing the
// lookup is what makes a cluster carrying one fall through to the
// category-stripping path rather than being misread as "t" + "ļ".
//
// That misreading is a real alternative, not a hypothetical. §4.6
// spells AGGLOMERATIVE -ļ- or -tļ-, the second because a lone ļ has
// nowhere legal to stand (§3.1 bars it word-initially, §5.1
// intervocalically), and t is itself the 2p/BEN referent. So a chain
// ending -tļ is either X + AGGLOMERATIVE or X + 2p/BEN +
// AGGLOMERATIVE, and mtļ is "he and co." or "he and you(pl.,
// beneficial) and co." Both are built by rules the same paragraph
// gives. Taking tļ whole is the reading here, since the two-consonant
// spelling exists for no other reason.
var biconsonantalForms = map[string]bool{
	"tļ": true,
	"th": true, "ph": true, "kh": true,
	"ll": true, "rr": true, "řř": true,
	"mm": true, "nn": true, "ňň": true,
	"hl": true, "hm": true, "hn": true, "hň": true,
}

// runeAt returns the first n runes of s as a string.
func runeAt(s string, n int) string {
	if n <= 0 {
		return ""
	}
	count := 0
	for i := range s {
		count++
		if count > n {
			return s[:i]
		}
	}
	return s
}

// runeLen returns the rune count of s.
func runeLen(s string) int {
	n := 0
	for range s {
		n++
	}
	return n
}

// DecomposeRefCluster splits a referential consonant cluster into the
// chain of PersonalRefs it spells, greedily left to right with the
// biconsonantal forms tried first. ok is false if any part fails to
// resolve.
func DecomposeRefCluster(s string) ([]g.PersonalRef, bool) {
	var out []g.PersonalRef
	for s != "" {
		if runeLen(s) >= 2 {
			bi := runeAt(s, 2)
			if biconsonantalForms[bi] {
				p, ok := LookupRefC1(bi)
				if !ok {
					return nil, false
				}
				out = append(out, p)
				s = s[len(bi):]
				continue
			}
		}
		mono := runeAt(s, 1)
		p, ok := LookupRefC1(mono)
		if !ok {
			return nil, false
		}
		out = append(out, p)
		s = s[len(mono):]
	}
	return out, true
}

// DecomposeRefAffixCs decodes the C_S of a §4.6.5 referential affix.
// It is DecomposeRefCluster plus the Obv/PVS alternate forms, which
// the §4.6 footnote admits in this position and nowhere else.
func DecomposeRefAffixCs(s string) ([]g.PersonalRef, bool) {
	if p, ok := refAffixAlternates[s]; ok {
		return []g.PersonalRef{p}, true
	}
	return DecomposeRefCluster(s)
}

// RefCategoryForm is one spelling of a §4.6 category modifier, with the
// placements the source sanctions for it.
type RefCategoryForm struct {
	Form     string
	Category g.RefCategory
	// Suffix and Prefix record which side the source writes the form
	// on. §4.6 hyphenates Agglomerative and Nomic on both sides
	// ("-ļ-", "-ç-"), which is its notation for an affix that may
	// precede or follow; Abstract it writes with a leading hyphen only
	// ("-w", "-y"), so that one is a suffix.
	Prefix bool
	Suffix bool
}

// RefCategoryForms lists every category spelling. Longer forms come
// first so "tļ" is tried before "ļ".
var RefCategoryForms = []RefCategoryForm{
	{Form: "tļ", Category: g.Agglomerative, Prefix: true, Suffix: true},
	{Form: "ç", Category: g.Nomic, Prefix: true, Suffix: true},
	{Form: "x", Category: g.Nomic, Prefix: true, Suffix: true},
	{Form: "w", Category: g.Abstract, Suffix: true},
	{Form: "y", Category: g.Abstract, Suffix: true},
	{Form: "ļ", Category: g.Agglomerative, Prefix: true, Suffix: true},
}

// DecomposeRefWithCategory decodes a cluster as a referent chain with
// an optional category modifier attached before or after it. cat is nil
// when the cluster is a plain chain.
func DecomposeRefWithCategory(s string) (cat *g.RefCategory, refs []g.PersonalRef, ok bool) {
	if r, dok := DecomposeRefCluster(s); dok && len(r) > 0 {
		return nil, r, true
	}
	for _, ca := range RefCategoryForms {
		if ca.Prefix {
			if rest, found := strings.CutPrefix(s, ca.Form); found && rest != "" {
				if r, dok := DecomposeRefCluster(rest); dok && len(r) > 0 {
					c := ca.Category
					return &c, r, true
				}
			}
		}
		if ca.Suffix {
			if rest, found := strings.CutSuffix(s, ca.Form); found && rest != "" {
				if r, dok := DecomposeRefCluster(rest); dok && len(r) > 0 {
					c := ca.Category
					return &c, r, true
				}
			}
		}
	}
	return nil, nil, false
}
