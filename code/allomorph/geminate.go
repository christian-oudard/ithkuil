package allomorph

import "github.com/christian-oudard/ithkuil/phonology"

// GeminateCa applies §3.6.1 to a Ca cluster. Gemination is what marks
// where Slot V ends and Slot VI begins, so a formative with any Slot V
// affix needs it; see §3.5.1 for the problem it solves.
//
// The nine rules are a DEFAULT PLUS EXCEPTIONS, not a dispatch table
// that every form must match. §3.6.1 says the boundary is shown "by
// gemination of the C_A form", and geminating a cluster means doubling
// its initial consonant. The numbered rules say where that does not
// hold. Reading them as an exhaustive dispatch, and treating the forms
// no rule names as a gap in the language, is a mistake this repository
// has now made twice — see the note on geminateCore's last return.
//
// Which rules restate the default and which are real exceptions:
//
//	1  single consonant -> double it            default
//	2  standalone "tļ" -> "ttļ"                 default
//	3  initial stop + liquid/approximant        default
//	5  initial non-sibilant fricative or nasal  default
//	4  a sibilant anywhere: kst -> ksst         EXCEPTION
//	6  voiceless stop + fricative: pf -> pff    EXCEPTION
//	7  two-stop ending: substitution table      EXCEPTION
//	8  stop + nasal ending: substitution table  EXCEPTION
//	9  initial l/r/ř: recurse, then re-prepend  EXCEPTION
//
// The shape of the exceptions is a phonetic fact: you cannot geminate
// mid-cluster except on a fricative. Rules 4 and 6 are exactly the
// cases that double a medial consonant, and both double a fricative —
// ksst and pff are sayable. Doubling a medial stop is not: akbbla and
// akttha are unsayable where akkbla and akktha are fine. That is why
// rules 7 and 8 substitute instead of doubling, their inputs ending in
// a stop, and why nothing anywhere doubles a stop that is not the
// first consonant.
//
// Tried in order of specificity, which is not the numbered order:
func GeminateCa(cluster string) string {
	rs := []rune(cluster)
	if len(rs) == 0 {
		return cluster
	}
	// Rule 9: initial l/r/ř — geminate the rest as if the liquid
	// weren't there, "if the resulting form including the initial l-,
	// r- or ř- is not phonotactically permissible or is euphonically
	// awkward, geminate the l-, r- or ř- instead". "lw" is the case
	// that bites: the inner "w" doubles to "ww", which §1.7 and §2.22
	// both forbid, so the liquid takes the gemination and it is "llw".
	if len(rs) > 1 && isLiquid(rs[0]) {
		inner := geminateCore(string(rs[1:]))
		candidate := string(rs[0]) + inner
		if inner != string(rs[1:]) && phonology.ClusterLegalAt(phonology.Medial, candidate) {
			return candidate
		}
		return string(rs[0]) + string(rs[0]) + string(rs[1:])
	}
	return geminateCore(cluster)
}

func geminateCore(cluster string) string {
	if cluster == "tļ" {
		return "ttļ"
	}
	rs := []rune(cluster)
	n := len(rs)
	if n == 1 {
		return string(rs[0]) + string(rs[0])
	}
	// Rule 6: initial voiceless stop + restricted fricative.
	if isVoicelessStop(rs[0]) && isRule6Fricative(rs[1]) {
		return string(rs[0]) + string(rs[1]) + string(rs[1]) + string(rs[2:])
	}
	// Rule 4: geminate the first sibilant anywhere.
	for i, r := range rs {
		if isSibilant(r) {
			return string(rs[:i]) + string(r) + string(rs[i:])
		}
	}
	// Rule 3: initial stop + liquid/approximant.
	if n >= 2 && isStop(rs[0]) && isLiquidOrApproximant(rs[1]) {
		return string(rs[0]) + cluster
	}
	// Rule 5: initial non-sibilant fricative or nasal.
	if isNonSibilantFricative(rs[0]) || isNasal(rs[0]) {
		return string(rs[0]) + cluster
	}
	// Rule 7: two-stop ending.
	if n >= 2 && isStop(rs[n-2]) && isStop(rs[n-1]) {
		if sub, ok := twoStopEnding[string(rs[n-2:])]; ok {
			return string(rs[:n-2]) + sub
		}
	}
	// Rule 8: stop + nasal ending.
	if n >= 2 && isStop(rs[n-2]) && isNasal(rs[n-1]) {
		if sub, ok := stopNasalEnding[string(rs[n-2:])]; ok {
			return string(rs[:n-2]) + sub
		}
	}
	// The default: double the first consonant. This is §3.6.1's own
	// "gemination of the C_A form" with no exception applying, and it
	// is the single busiest branch — 460 of the 3840 Ca values reach
	// it, kbl -> kkbl and kth -> kkth among them.
	//
	// It reads like a fallback and is not one. Twice now it has been
	// mistaken for a hack papering over forms the rules fail to name,
	// and both times the conclusion drawn was that §3.6.1 has a hole
	// in it. It does not. Every Ca value gets exactly one geminate,
	// all legal, all distinct, none equal to a bare Ca, which
	// TestGeminate_EveryFormIsCoveredAndDistinct checks.
	return string(rs[0]) + cluster
}

var twoStopEnding = map[string]string{
	"pt": "bbḑ", "pk": "bbv",
	"kt": "ggḑ", "kp": "ggv",
	"tk": "ḑvv", "tp": "ddv",
}

var stopNasalEnding = map[string]string{
	"pm": "vvm", "pn": "vvn",
	"km": "xxm", "kn": "xxn",
	"tm": "ḑḑm", "tn": "ḑḑn",
	"bm": "mmw", "bn": "mml",
	"gm": "ňňw", "gn": "ňňl",
	"dm": "nnw", "dn": "nnl",
}

func isStop(r rune) bool {
	switch r {
	case 'p', 't', 'k', 'b', 'd', 'g':
		return true
	}
	return false
}

func isVoicelessStop(r rune) bool {
	return r == 'p' || r == 't' || r == 'k'
}

func isSibilant(r rune) bool {
	switch r {
	case 's', 'š', 'z', 'ž', 'ç', 'c', 'č':
		return true
	}
	return false
}

func isNonSibilantFricative(r rune) bool {
	switch r {
	case 'f', 'ţ', 'v', 'ḑ':
		return true
	}
	return false
}

// isRule6Fricative is the restricted set used in §3.6.1 rule 6
// (s, š, f, ţ, ç) — sibilant fricatives plus non-sibilant voiceless
// fricatives, excluding the affricates and voiced sibilants.
func isRule6Fricative(r rune) bool {
	switch r {
	case 's', 'š', 'f', 'ţ', 'ç':
		return true
	}
	return false
}

func isNasal(r rune) bool {
	switch r {
	case 'n', 'm', 'ň':
		return true
	}
	return false
}

func isLiquid(r rune) bool {
	switch r {
	case 'l', 'r', 'ř':
		return true
	}
	return false
}

func isLiquidOrApproximant(r rune) bool {
	if isLiquid(r) {
		return true
	}
	return r == 'w' || r == 'y'
}
