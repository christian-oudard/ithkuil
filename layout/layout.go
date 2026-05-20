// Package layout is Layer C of the parse/render pipeline. It pairs raw
// surface conjuncts with the slot positions they occupy, without
// decoding any grammar values. Both parse and render lean on it:
//
//	surface text  ──(surface.Strip/SplitConjuncts)──▶  conjuncts + Stress
//	conjuncts     ──(layout.Parse)──▶                  Layout
//	Layout        ──(layout.ToGrammar)──▶              grammar.Formative
//	Layout        ──(layout.FromGrammar)──◀            grammar.Formative
//	Layout        ──(layout.Render)──▶                 conjuncts
//	conjuncts     ──(surface.Apply/JoinConjuncts)──▶   surface text
//
// Storing the slot-labelled surface form as its own value means the
// shape-detection logic (consonant-initial vs vowel-initial, shortcut
// vs not, special-Vv Cs-root vs reference-root) lives in exactly one
// place. fullparse, render, and inspect all consume Layout instead of
// re-deriving the structure from the conjunct list.
package layout

import (
	"github.com/christian-oudard/ithkuil/surface"
)

// RootKind discriminates the three formative shapes that the spec
// recognises. The kind is determined by the surface Vv marker (or its
// absence) plus, for shortcut forms, the Cc cluster.
type RootKind int

const (
	// CrFormative is the regular lexical formative: Vv-Cr-Vr-…-Ca-…
	// or, with §3.2 elision, Cr-Vr-…-Ca-… (Vv defaulted), or with a
	// Cc shortcut, Cc-Vv-Cr-… (Vr defaulted and Ca encoded into Cc+Vv).
	CrFormative RootKind = iota
	// CsRootFormative is the Cs-root formative (§4.2). Vv is one of
	// ëi/eë/ëu/oë, Cr position holds an affix consonant, Vr encodes
	// (degree, context) instead of (function, specification, context).
	CsRootFormative
	// RefRootFormative is the reference-root formative (§5.3). Vv is
	// ae/ea, Cr position holds a referential C1 consonant, Vr is a
	// normal Function/Specification/Context vowel.
	RefRootFormative
)

// AffixChunk is one (Vx, Cs) pair as it appears on the surface. The
// rendering order differs between Slot V (Cs then Vx) and Slot VII (Vx
// then Cs), but both store the same (Vx, Cs) data here.
type AffixChunk struct {
	Vx string
	Cs string
}

// Layout is the slot-labelled surface form of a formative. Every
// string field carries a raw, bare conjunct — accents are stripped
// (those live in Stress), and §3.5.1 Vv-glottal-stops / §3.6.1
// Ca-geminations are removed (they're implied by len(SlotV)).
type Layout struct {
	// SentenceStarter is true when the surface form had a leading ç.
	SentenceStarter bool

	// Kind picks the formative shape (Cr / Cs-root / ref-root).
	Kind RootKind

	// Cc is the Slot I consonant prefix in its raw form: "", "h",
	// "hw", "w", "y", "hl", "hm", "hr", or "hn". Translation to
	// (Concat, Shortcut) happens in Layer D.
	Cc string

	// Vv is the Slot II vowel without any §3.5.1 glottal-stop.
	// Empty when Vv is elided (consonant-initial Cr formatives).
	Vv string

	// Cr is the root consonant cluster — Cr for CrFormative, Cs for
	// CsRootFormative, C1 for RefRootFormative.
	Cr string

	// Vr is the Slot IV vowel. Empty when Vr is elided (shortcut
	// form of a CrFormative).
	Vr string

	// SlotV is the list of pre-Ca affixes in their (Vx, Cs) order.
	// Surface rendering reverses each pair to Cs+Vx; the Vx field
	// here is the bare un-reversed vowel.
	SlotV []AffixChunk

	// Ca is the Slot VI consonant cluster, with any §3.6.1
	// gemination undone. Empty for the shortcut form (Ca is then
	// derived from Cc+Vv at Layer D).
	Ca string

	// SlotVII is the list of post-Ca affixes in (Vx, Cs) order.
	SlotVII []AffixChunk

	// Vn / Cn are the Slot VIII pair. Both empty when Slot VIII is
	// absent; both non-empty when it is present.
	Vn string
	Cn string

	// Vc is the Slot IX vowel (Vc or Vk depending on Final). Empty
	// when the slot is elided.
	Vc string

	// MovedGlottal is true when the §3.9.1 SPECIAL NOTE shortening
	// rule has been applied: the Vc glottal-stop for cases 37-52
	// has been shifted onto an earlier vocalic form, leaving Vc in
	// its un-glottalized shape. ToGrammar reconstructs the proper
	// case by re-adding the glottal.
	MovedGlottal bool

	// CnInCa is true when the §3.8.1.2 shortening rule has been
	// applied: a Pattern-1 Mood/Case-Scope Cn (hl/hr/hm/hn/hň) has
	// been written in the Ca slot in place of the default "l", and
	// Slot VIII's Vn is implicitly MNO ("a"). Layer C populates
	// Ca = "l" (default), Cn = the Pattern-1 cluster, Vn = "a" so
	// Layer D can build the SlotVIII normally; the flag lets Render
	// emit the shortcut form again.
	CnInCa bool

	// Stress is the prosodic stress observed (or to apply).
	Stress surface.Stress
}
