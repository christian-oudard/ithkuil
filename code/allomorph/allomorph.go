// Package allomorph builds the Slot VI Ca consonant cluster from its
// five components (Configuration, Affiliation, Perspective, Extension,
// Essence). The raw composition is in ConstructCaRaw; allomorphic
// substitutions and the bidirectional lookup tables live alongside it.
package allomorph

import (
	g "github.com/christian-oudard/ithkuil/grammar"
)

// ca1 is the Configuration component of Ca (grammar ch.3 table).
// UPX is empty — no consonant is contributed.
var ca1 = [...]string{
	g.UPX: "",
	g.DPX: "s",
	g.DSS: "c",
	g.DSC: "ks",
	g.DSF: "ps",
	g.DDS: "ţs",
	g.DDC: "fs",
	g.DDF: "š",
	g.DFS: "č",
	g.DFC: "kš",
	g.DFF: "pš",
	g.MSS: "t",
	g.MSC: "k",
	g.MSF: "p",
	g.MDS: "ţ",
	g.MDC: "f",
	g.MDF: "ç",
	g.MFS: "z",
	g.MFC: "ž",
	g.MFF: "ẓ",
}

// ca2 is the Extension component in the voiceless form, used when a
// Configuration consonant precedes it.
var ca2 = [...]string{
	g.DEL: "",
	g.PRX: "t",
	g.ICP: "k",
	g.ATV: "p",
	g.GRA: "g",
	g.DPL: "b",
}

// ca2Standalone is the Extension component in the voiced form, used when
// Configuration is UPX (no Ca1 consonant to follow).
var ca2Standalone = [...]string{
	g.DEL: "",
	g.PRX: "d",
	g.ICP: "g",
	g.ATV: "b",
	g.GRA: "gz",
	g.DPL: "bz",
}

// ca3 is the short Affiliation prefix (used before a Configuration
// consonant).
var ca3 = [...]string{
	g.CSL: "",
	g.ASO: "l",
	g.COA: "r",
	g.VAR: "ř",
}

// ca3Standalone is the long Affiliation form, used when Affiliation is
// the only marked component (no Configuration, no Extension, no
// perspective suffix).
var ca3Standalone = [...]string{
	g.CSL: "",
	g.ASO: "nļ",
	g.COA: "rļ",
	g.VAR: "ň",
}

// ca4Entry pairs the standalone and after-consonant forms of a
// Perspective+Essence combination. Standalone is used when Ca1, Ca2, Ca3
// are all empty; the after-consonant form is used otherwise.
type ca4Entry struct{ standalone, suffix string }

var ca4Table = map[ca4Key]ca4Entry{
	{g.M_, g.NRM}: {"l", ""},
	{g.G_, g.NRM}: {"r", "r"},
	{g.N_, g.NRM}: {"v", "w"},
	{g.A_, g.NRM}: {"j", "y"},
	{g.M_, g.RPV}: {"tļ", "l"},
	{g.G_, g.RPV}: {"ř", "ř"},
	{g.N_, g.RPV}: {"m", "m"},
	{g.A_, g.RPV}: {"n", "n"},
}

type ca4Key struct {
	Perspective g.Perspective
	Essence     g.Essence
}

func ca4(p g.Perspective, e g.Essence) ca4Entry {
	return ca4Table[ca4Key{p, e}]
}

// ConstructCaRaw builds the raw Ca consonant cluster from a SlotVI by
// concatenating its components in grammatical order
// (Affiliation + Configuration + Extension + Perspective/Essence) with
// special-case forms for UPX Configuration. Allomorphic substitutions
// are NOT applied here — see ApplySubstitutions for that next step.
//
// Special-case rules (grammar §3.5):
//
//  1. UPX + Extension ≠ DEL: use voiced standalone Extension form
//     (d/g/b/gz/bz) followed by the perspective suffix.
//  2. UPX + Affiliation ≠ CSL: use the long Affiliation form alone if
//     no perspective suffix; otherwise short prefix + suffix.
//  3. UPX/CSL/DEL (fully default Configuration/Affiliation/Extension):
//     use the standalone perspective form.
//  4. General: Affiliation prefix + Configuration + Extension +
//     Perspective suffix, with grammar §3.5.1 N_/A_ RPV → h/ç when
//     preceded by a stop consonant.
func ConstructCaRaw(s g.SlotVI) string {
	persp := ca4(s.Perspective, s.Essence)

	if s.Configuration == g.UPX && s.Extension != g.DEL {
		// UPX Configuration contributes no consonant of its own, so
		// Extension always takes its voiced standalone form
		// (d/g/b/gz/bz) — this is the rule for UNIPLEX in the §3.6
		// table, not the "alternate when preceded by [C]t/k/p" rule
		// that applies to non-UPX Configurations. The Affiliation
		// prefix sits in front; the original code dropped it.
		//
		// The footnote this rests on — "use the alternate form if the
		// Configuration of the word is UPX" — carries its marker on
		// all five Extension rows: t/d¹, k/g¹, p/b¹, g/gz¹, b/bz¹. So
		// it governs the whole column and no inference is needed.
		//
		// This comment used to say the marker was printed on only GRA
		// and DPL, and argued the rest from injectivity: UPX/PRX would
		// otherwise compose a bare t, which is already MSS/DEL, and
		// likewise k for UPX/ICP against MSC/DEL and p for UPX/ATV
		// against MSF/DEL, costing 96 of the 3840 forms their distinct
		// spelling. The argument holds and TestCaFormsAreDistinct still
		// checks it, but the premise was our own: morphology.md had
		// dropped three of the five markers, and the PDF at 600 dpi has
		// all five.
		return ca3[s.Affiliation] + ca2Standalone[s.Extension] + persp.suffix
	}
	if s.Configuration == g.UPX && s.Affiliation != g.CSL {
		if persp.suffix == "" {
			return ca3Standalone[s.Affiliation]
		}
		return ca3[s.Affiliation] + persp.suffix
	}
	if s.Configuration == g.UPX {
		return persp.standalone
	}

	// General compositional form.
	preceding := ca3[s.Affiliation] + ca1[s.Configuration] + ca2[s.Extension]
	suffix := persp.suffix
	if suffix == "m" && endsWithStop(preceding) {
		suffix = "h"
	} else if suffix == "n" && endsWithStop(preceding) {
		suffix = "ç"
	}
	return preceding + suffix
}

func endsWithStop(s string) bool {
	if s == "" {
		return false
	}
	r := []rune(s)
	last := r[len(r)-1]
	return last == 't' || last == 'k' || last == 'p'
}

// init verifies that the component tables cover every value in their
// respective enums. Running this at startup catches table omissions
// immediately instead of returning empty strings at parse time.
func init() {
	for _, c := range g.AllConfigurations {
		if int(c) >= len(ca1) {
			panic("allomorph: ca1 table missing entry for " + c.String())
		}
	}
	for _, e := range g.AllExtensions {
		if int(e) >= len(ca2) || int(e) >= len(ca2Standalone) {
			panic("allomorph: ca2 table missing entry for " + e.String())
		}
	}
	for _, a := range g.AllAffiliations {
		if int(a) >= len(ca3) || int(a) >= len(ca3Standalone) {
			panic("allomorph: ca3 table missing entry for " + a.String())
		}
	}
	for _, p := range g.AllPerspectives {
		for _, e := range g.AllEssences {
			if _, ok := ca4Table[ca4Key{p, e}]; !ok {
				panic("allomorph: ca4 table missing " + p.String() + "/" + e.String())
			}
		}
	}
	// Sanity check the default Ca encodes to "l".
	if got := ConstructCaRaw(g.DefaultSlotVI); got != "l" {
		panic("allomorph: default Ca raw should be \"l\", got " + got)
	}
}
