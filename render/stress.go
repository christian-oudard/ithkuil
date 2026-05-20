package render

import (
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/surface"
)

// applyFinalStress places a stress diacritic on the appropriate vowel
// of word, driven by the Formative's Final variant (§1.3.1):
//   - UnframedNominal: no diacritic (penultimate is the orthographic
//     default).
//   - UnframedVerbal: acute on the last vowel-conjunct (Ultimate). A
//     monosyllabic word is left unmarked — monosyllabic is implicit
//     ultimate per §3.10.
//   - FramedVerbal: acute on the third-from-last vowel-conjunct
//     (Antepenultimate).
//
// The actual byte-level work — finding the target vowel-conjunct,
// applying the acute (or circumflex on the umlauted layer), and the
// "first vowel of a multi-vowel conjunct" rule — lives in
// surface.Apply. This function only owns the Final → Stress mapping.
func applyFinalStress(word string, f g.Final) string {
	return surface.Apply(word, stressFromFinal(f))
}

// stressFromFinal returns the orthographic Stress position implied
// by a Final grammar variant.
func stressFromFinal(f g.Final) surface.Stress {
	switch f.(type) {
	case g.UnframedVerbal:
		return surface.Ultimate
	case g.FramedVerbal:
		return surface.Antepenultimate
	}
	return surface.Penultimate
}
