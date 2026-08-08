package phonology

import "testing"

// Twenty pairwise judgments from a speaker, each pair differing only in
// its medial cluster. These are the only calibration data we have that
// bears directly on effort: the corpus shows which spellings Quijada
// chose, but not which of two clusters is easier to say.
//
// The model reached seventeen of twenty once the four gaps below were
// closed. It stays skipped because three remain, and because what it
// records is a target rather than a guarantee.
//
// Still open:
//
//   - afta over asta. f and s are both cheap fricatives before a stop
//     and nothing separates the clusters.
//   - axla over aţla. Uvular is priced dear so that ř outranks r, but
//     x is uvular too and the speaker finds it easy. ř's cost belongs
//     to being a rhotic, not to being uvular, and there is no rhotic
//     term.
//   - anka over anta. A homorganic nasal and stop should cost more
//     than a heterorganic pair, which is the OCP arm's job, but the
//     travel term more than repays it over that distance.
//
// The four gaps that were closed, none of which was a weight:
//
//  1. Distance is symmetric, so nothing can tell tl from lt. The
//     speaker prefers alta to atla ("less stoppage of airflow"), and
//     the source is directional in several places too: §2.5 permits cč
//     and cj while barring čc and čẓ, §2.9 is one-directional, §2.18
//     bars dļ gļ bļ but not ļd ļg ļb.
//
//  2. Segments have no cost of their own, only transitions. Most of
//     these judgments are about a segment rather than a junction: l
//     easier than r easier than ř, y easier than w, i easier than u, s
//     easier than ţ and than š, voiceless easier than voiced.
//
//  3. Rounding is not charged. w over y and u over i are the same
//     judgment twice, and the speaker gave the same reason both times.
//
//  4. The similarity penalty has the wrong shape at zero. It is
//     largest for a geminate, and the speaker finds alla easier than
//     alra. That is also what the sources say: §1.7 permits geminates
//     outright and §6 generates them, while what §2.4 and §2.5 bar is
//     the near-miss, two homologous consonants disagreeing in voicing.
//     Similarity avoidance should peak at small non-zero distance and
//     vanish at zero, not peak at zero.
func TestEffortMatchesSpeakerJudgments(t *testing.T) {
	t.Skip("40 of 49; vowels have no cost of their own yet, see below")

	// easier, harder
	judgments := [][2]string{
		{"anta", "ampa"}, // dental easier than bilabial
		{"apta", "abda"}, // voicing is work
		{"alta", "atla"}, // less stoppage of airflow
		{"aiya", "aiwa"}, // y easier than w
		{"aiva", "auva"}, // i easier than u
		{"afta", "asta"}, //
		{"alka", "arka"}, // l easier than r
		{"ehla", "ehra"}, // l easier than r
		{"ehya", "ehwa"}, // y easier than w
		{"anla", "amla"}, // dental easier than bilabial
		{"arla", "ařla"}, // r easier than ř
		{"akya", "akwa"}, // y easier than w
		{"asra", "aţra"}, // s easier than ţ
		{"aska", "aţka"}, // s easier than ţ
		{"alţa", "arţa"}, // l easier than r
		{"apla", "apra"}, // l easier than r
		{"axla", "aţla"}, // x easier than ţ, slightly
		{"asta", "ašta"}, // s easier than š
		{"anka", "anta"}, // non-homorganic easier than homorganic
		{"alla", "alra"}, // a geminate is easier than two liquids
	}

	// Batch two. Vowels, which batch one omitted entirely, plus the
	// contrasts batch one left open.
	judgments = append(judgments, [][2]string{
		{"mela", "mala"},       // e easier than a
		{"mila", "mula"},       // i easier than u
		{"mala", "mäla"},       // a easier than ä
		{"mela", "möla"},       // rounding costs at mid front
		{"mila", "müla"},       // rounding costs at high
		{"mola", "mëla"},       // but o beats ë: ë is the marked one, not u
		{"maula", "maila"},     // after a, the back second element wins
		{"meila", "moila"},     //
		{"mëila", "mëula"},     // but after ë, the front one does
		{"miula", "muila"},     // diphthongs are order-sensitive too
		{"maola", "maöla"},     //
		{"malaula", "malaila"}, //
		{"arta", "ařta"},       // r easier than ř
		{"axta", "ařta"},       // x easier than ř, though both are uvular
		{"axla", "ařla"},       // so ř is dear for being a rhotic, not uvular
		{"aţma", "axma"},       // but ţ beats x here, where axla beat aţla
		{"asta", "assa"},       // an obstruent geminate is NOT cheap
		{"asta", "atta"},       // nor is a stop geminate
		{"alla", "anna"},       // sonorant geminates are, and l over n
		{"aspa", "apsa"},       // falling sonority again
		{"arpa", "apra"},       // and again
		{"alna", "anla"},       // and again
		{"alka", "asta"},       //
		{"anta", "asta"},       //
		{"afna", "afma"},       // n easier than m, as in batch one
		{"afma", "avma"},       // voiceless easier than voiced, as in batch one
		{"asma", "aţma"},       // s easier than ţ, as in batch one
		{"asta", "astra"},      // shorter is easier: length is real
		{"mala", "malala"},     // and again, with no cluster difference at all
	}...)

	// The a/ë split in the diphthongs is the speaker's own point, and
	// §1.2.1 gives it a mechanism. Both are back unrounded in §1.1 and
	// differ only in height, but ë is "[ɤ] or [ʌ] or [ə]", and the last
	// of those is central. From a, firmly low and back, au is a
	// vertical move and ai a diagonal one, so travel decides and the
	// back element wins. From ë as a schwa the two are near symmetric,
	// travel cancels, and the second element's own cost decides, which
	// is i over u exactly as mila over mula and mila over müla say.
	//
	// So a diphthong costs travel from first element to second plus the
	// second element's own cost, and which term dominates depends on
	// where the first element sits. One mechanism rather than ten free
	// parameters. Untested predictions: oi should lose to ou, and ei
	// should beat eu decisively.

	// Standing at 40 of 49. The rhotic term and the geminate split have
	// landed; what remains is almost all one gap, and one that only the
	// second batch could show because the first had no vowels in it.
	//
	// A vowel costs nothing of its own. SegmentCost adds only
	// roundingCost for a Vowel, so a, e and ä score identically and ë
	// comes out cheaper than o for being unrounded, which is backwards.
	// The judgments give an ordering to fit: e over a, i over u, a over
	// ä, e over ö, i over ü, and o over ë. Rounding is part of it but
	// not the whole: ë is the marked vowel, not roundness as such, and
	// §1.2.1 gives it three realisations, "[ɤ] or [ʌ] or [ə]".
	//
	// The diphthong pairs need the account already worked out below: a
	// diphthong costs travel from its first element to its second plus
	// that second element's own cost, so au beats ai after a while ëi
	// beats ëu after ë.
	//
	// Two consonant pairs are left over and unexplained: afta over asta,
	// and anka over anta. The second says a homorganic nasal and stop
	// cost more than a heterorganic pair, which is the similarity arm's
	// job, but the travel term more than repays it over that distance.
	//
	// The speaker later graded these: x is "hard-ish" and ř "pretty
	// hard", so the ordering is ţ then x then ř. Uvular is a costly
	// place and a rhotic costs again on top of it. That makes batch
	// one's axla over aţla the outlier, and it was hedged as "slightly"
	// when given.

	// Batch three, on glottal stops, came back on a different scale.
	// The speaker answered it in terms of clarity rather than effort,
	// and said so: ma'ala is "slightly harder to say but much easier to
	// hear". Effort and audibility can point opposite ways, and on
	// every glottal pair audibility won.
	//
	// That is Quijada's own pair of criteria. §2 bars conjuncts for
	// "difficulty/awkwardness in pronunciation, or because they are too
	// phonetically indistinguishable from other forms", and only the
	// first is what this file measures. It is also Boersma's two drives
	// and Lindblom's effort-subject-to-discriminability, both cited in
	// effort.go and both implemented here as one number.
	//
	// The judgments, on the clarity scale:
	//
	//	ma'ala over ma'la      ma'ila over mai'la
	//	ma'ula over mau'la     ma'ela over mae'la
	//	ma'la  over mala'      mala'i over ma'lai
	//	malu'a over ma'lua
	//
	// and on effort: malla over ma'la, ma'ala over ma'ila, ma'ila over
	// ma'ula slightly.
	//
	// Not added to the list below, because that list is scored against
	// Energy, which measures effort. A second scale has to exist before
	// these can be scored at all. The decision it governs is real: the
	// glottal stop in cases 37 through 52 carries the case, so an
	// inaudible placement loses a distinction, while the choice between
	// two spellings of a Slot IX default carries nothing. The model
	// needs to know which segments are load-bearing.

	agree := 0
	for _, j := range judgments {
		easier, harder := Energy(j[0]), Energy(j[1])
		if easier < harder {
			agree++
			continue
		}
		verdict := "scores them equal"
		if easier > harder {
			verdict = "has it backwards"
		}
		t.Errorf("%s should be easier than %s; model %s (%.3f vs %.3f)",
			j[0], j[1], verdict, easier, harder)
	}
	t.Logf("agrees with %d of %d", agree, len(judgments))
}
