// Package referentials implements Ithkuil V4 personal references
// (Chapter 9). A referential word identifies an entity by category
// (speaker, addressee, third party, etc.) plus an effect (neutral,
// beneficial, detrimental). Multiple referents can be chained in a
// single consonant cluster; an optional category modifier
// (Agglomerative / Nomic / Abstract) attaches as a prefix or suffix.
package referentials

import "strings"

// Referent is one of the 11 referent categories.
type Referent int

const (
	R1m  Referent = iota // monadic speaker ("I")
	R2m                  // monadic addressee ("you sg.")
	R2p                  // polyadic addressee ("you pl.")
	Rma                  // monadic animate 3rd party ("he/she")
	Rpa                  // polyadic animate 3rd party ("they")
	Rmi                  // monadic inanimate 3rd party ("it")
	Rpi                  // polyadic inanimate 3rd party ("those things")
	Rmx                  // mixed animate/inanimate
	Rrdp                 // reduplicative (resumptive reference)
	Robv                 // obviative (other 3rd party)
	Rpvs                 // provisional ("whatever")
)

var referentAbbrevs = [...]string{
	"1m", "2m", "2p",
	"ma", "pa",
	"mi", "pi",
	"Mx",
	"Rdp", "Obv", "PVS",
}

var referentLabels = [...]string{
	"I", "you(sg.)", "you(pl.)",
	"he/she", "they(anim.)",
	"it", "them(inanim.)",
	"it+they(mixed)",
	"aforementioned", "the other one", "whatever",
}

func (r Referent) String() string { return referentAbbrevs[r] }

// Label returns a longer English gloss for the referent.
func (r Referent) Label() string { return referentLabels[r] }

// AllReferents enumerates the 11 referent categories in declaration order.
var AllReferents = []Referent{
	R1m, R2m, R2p, Rma, Rpa, Rmi, Rpi, Rmx, Rrdp, Robv, Rpvs,
}

// Effect is the referent's effect on the speaker/event: neutral,
// beneficial, or detrimental.
type Effect int

const (
	NEU Effect = iota // Neutral
	BEN               // Beneficial
	DET               // Detrimental
)

func (e Effect) String() string {
	return [...]string{"NEU", "BEN", "DET"}[e]
}

// AllEffects enumerates the three effects.
var AllEffects = []Effect{NEU, BEN, DET}

// PersonalRef is a (Referent, Effect) pair. The C1 consonant cluster
// in a referential word encodes one or more of these.
type PersonalRef struct {
	Referent Referent
	Effect   Effect
}

// c1Table maps (Referent, Effect) to its C1 consonant form. Entries
// follow the Sec. 9.1 table.
var c1Table = map[PersonalRef]string{
	{R1m, NEU}: "l", {R1m, BEN}: "r", {R1m, DET}: "ř",
	{R2m, NEU}: "s", {R2m, BEN}: "š", {R2m, DET}: "ž",
	{R2p, NEU}: "n", {R2p, BEN}: "t", {R2p, DET}: "d",
	{Rma, NEU}: "m", {Rma, BEN}: "p", {Rma, DET}: "b",
	{Rpa, NEU}: "ň", {Rpa, BEN}: "k", {Rpa, DET}: "g",
	{Rmi, NEU}: "z", {Rmi, BEN}: "ţ", {Rmi, DET}: "ḑ",
	{Rpi, NEU}: "ẓ", {Rpi, BEN}: "f", {Rpi, DET}: "v",
	{Rmx, NEU}: "c", {Rmx, BEN}: "č", {Rmx, DET}: "j",
	{Rrdp, NEU}: "th", {Rrdp, BEN}: "ph", {Rrdp, DET}: "kh",
	{Robv, NEU}: "ll", {Robv, BEN}: "rr", {Robv, DET}: "řř",
	{Rpvs, NEU}: "mm", {Rpvs, BEN}: "nn", {Rpvs, DET}: "ňň",
}

// RefC1 returns the C1 consonant form for a PersonalRef.
func RefC1(p PersonalRef) string { return c1Table[p] }

// c1Reverse is the reverse map for cluster decomposition. Built once
// at init time; includes the alternate form "ļ" for pi.NEU.
var c1Reverse = func() map[string]PersonalRef {
	m := make(map[string]PersonalRef, len(c1Table)+1)
	for ref, form := range c1Table {
		m[form] = ref
	}
	m["ļ"] = PersonalRef{Rpi, NEU} // alternate form
	return m
}()

// LookupRefC1 decodes a single C1 consonant form (mono- or bi-consonantal)
// to its PersonalRef.
func LookupRefC1(c string) (PersonalRef, bool) {
	p, ok := c1Reverse[c]
	return p, ok
}

// biconsonantalForms enumerates the 2-character C1 forms (including "tļ"
// reserved for the agglomerative modifier). Greedy decomposition checks
// these first so a leading "ll" parses as Robv/NEU rather than two l's.
var biconsonantalForms = map[string]bool{
	"tļ": true,
	"th": true, "ph": true, "kh": true,
	"ll": true, "rr": true, "řř": true,
	"mm": true, "nn": true, "ňň": true,
	"hl": true, "hm": true, "hn": true, "hň": true,
}

// runeAt returns the rune-prefix of length n from s as a string.
// Used to peel off candidate biconsonantal forms regardless of UTF-8
// byte length.
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

// DecomposeRefCluster splits a C1 consonant cluster into a list of
// individual PersonalRefs. Uses greedy left-to-right matching with
// biconsonantal forms checked first. Returns ok=false if any portion
// can't be resolved.
//
// A leading biconsonantal-shaped pair that isn't itself a valid C1
// (like "tļ", the agglomerative prefix) makes the entire decomposition
// fail — it's not silently re-parsed as two single C1s. This lets
// DecomposeRefWithCategory cleanly try a category-stripped path.
func DecomposeRefCluster(s string) ([]PersonalRef, bool) {
	var out []PersonalRef
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

// Category is an optional modifier on a referential cluster:
//
//   - Agglomerative ("each/every X")
//   - Nomic ("someone/something")
//   - Abstract ("everything about X")
type Category int

const (
	Agglomerative Category = iota
	Nomic
	Abstract
)

func (c Category) String() string {
	return [...]string{"AGM", "NOM", "ABS"}[c]
}

// categoryAffixes lists prefix/suffix pairs that mark each Category.
// Longer affixes come first so "tļ" matches before "ļ".
var categoryAffixes = []struct {
	form string
	cat  Category
}{
	{"tļ", Agglomerative},
	{"ç", Nomic},
	{"x", Nomic},
	{"w", Abstract},
	{"y", Abstract},
	{"ļ", Agglomerative},
}

// DecomposeRefWithCategory tries to decode a cluster as referential C1
// possibly preceded or followed by a category modifier. Returns
// (category, refs, true) on success. If the cluster decodes as plain
// C1 without any modifier, category is nil. Returns false if neither
// shape applies.
func DecomposeRefWithCategory(s string) (cat *Category, refs []PersonalRef, ok bool) {
	// Try plain decomposition first.
	if r, dok := DecomposeRefCluster(s); dok && len(r) > 0 {
		return nil, r, true
	}
	// Then try stripping each prefix/suffix.
	for _, ca := range categoryAffixes {
		if rest, found := strings.CutPrefix(s, ca.form); found && rest != "" {
			if r, dok := DecomposeRefCluster(rest); dok && len(r) > 0 {
				c := ca.cat
				return &c, r, true
			}
		}
		if rest, found := strings.CutSuffix(s, ca.form); found && rest != "" {
			if r, dok := DecomposeRefCluster(rest); dok && len(r) > 0 {
				c := ca.cat
				return &c, r, true
			}
		}
	}
	return nil, nil, false
}
