package render

import (
	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/parse"
)

// ivlCs is the consonant of the IVL ("Illocution + Validation") affix
// from §3.9.3.3. Used here as a best-effort carrier for Validation
// information that can't fit in Vk: §3.9.3.2 restricts Validation to
// ASR illocutions; for non-ASR illocutions, Vk encodes the illocution
// alone and Validation has no surface form. Emitting the Type-2 IVL
// affix lets a non-ASR formative still carry its intended Validation
// through render→parse round-trips.
const ivlCs = "nļ"

// applyIVLWorkaround returns a copy of slotVII with a trailing IVL
// affix appended if f represents a non-ASR verbal formative with a
// non-default Validation that Vk cannot encode. The original f is left
// unchanged.
//
// The IVL affix's Type-2 degree N+1 carries the Nth validation (OBS=0
// → degree 1, ..., INF=8 → degree 9). When the parser sees this affix
// it lifts the value back into the Validation field and drops the
// affix; see fullparse.absorbIVLAffix.
func applyIVLWorkaround(f g.Formative, slotVII []g.Affix) []g.Affix {
	iv, ok := f.SlotIX.(g.IllocValSlot)
	if !ok {
		return slotVII
	}
	if iv.Illocution == g.ASR || iv.Validation == g.OBS {
		return slotVII
	}
	degree := int(iv.Validation) + 1
	affix := g.Affix{
		Consonant: ivlCs,
		Vowel:     parse.Type2DegreeToVowel(degree),
		Type:      g.Type2Affix,
	}
	out := make([]g.Affix, len(slotVII)+1)
	copy(out, slotVII)
	out[len(slotVII)] = affix
	return out
}
