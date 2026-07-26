package allomorph

// GeminateCa applies the §3.6.1 gemination rules to a Ca cluster.
// Gemination is required whenever Slot V has any affixes; it marks
// where Slot V ends and Slot VI begins.
//
// The rules are tried in order of specificity:
//  9. Initial l/r/ř: recurse on the rest, then re-prepend.
//  2. Standalone "tļ" → "ttļ".
//  1. Single consonant: double it.
//  6. Initial voiceless stop (p/t/k) + fricative (s/š/f/ţ/ç): double the fricative.
//  4. Any sibilant (s/š/z/ž/ç/c/č): double the first sibilant.
//  3. Initial stop + liquid/approximant (l/r/ř/w/y): double the stop.
//  5. Initial non-sibilant fricative (f/ţ/v/ḑ) or nasal (n/m/ň): double it.
//  7. Two-stop ending: voicing-and-substitution table.
//  8. Stop + nasal ending: substitution table.
func GeminateCa(cluster string) string {
	rs := []rune(cluster)
	if len(rs) == 0 {
		return cluster
	}
	// Rule 9: initial l/r/ř — geminate the rest as if the liquid
	// weren't there. If the cluster has no other consonant to
	// geminate, double the liquid itself.
	if len(rs) > 1 && isLiquid(rs[0]) {
		inner := geminateCore(string(rs[1:]))
		if inner != string(rs[1:]) {
			return string(rs[0]) + inner
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
	// Fallback: double the first consonant. Shouldn't be reached for
	// valid Ca clusters in practice.
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
