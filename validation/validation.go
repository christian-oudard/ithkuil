// Package validation implements phonotactic constraint checking for
// Ithkuil V4. The rules follow "Phonotactic Rules for New Ithkuil,
// v.1.0".
//
// Covers the most commonly violated rules from Section 2 (Prohibited
// Consonantal Conjuncts) plus the triple-consonant check. Full coverage
// of the cluster-level rules and stress validation can be layered on later.
package validation

import (
	"fmt"
	"strings"
)

// Result captures the outcome of a validation check.
type Result struct {
	Valid  bool
	Errors []Error
}

// Error describes a single phonotactic violation.
type Error struct {
	Rule    string // short rule identifier ("2.1", "triple", …)
	Cluster string // the offending pair or triple
	Reason  string // human-readable explanation
}

func (e Error) String() string {
	if e.Cluster == "" {
		return e.Rule + ": " + e.Reason
	}
	return e.Rule + ": " + e.Reason + " (cluster " + e.Cluster + ")"
}

// CheckProhibitedPair returns a non-empty rule string and reason if
// the rune pair (a, b) violates one of the cluster pair rules.
// Returns ("", "") when the pair is OK.
func CheckProhibitedPair(a, b rune) (rule, reason string) {
	// 2.1: no consonant + glottal stop
	if b == '\'' {
		return "2.1", "consonant followed by glottal stop"
	}

	// 2.2: dental stop + sibilant (and dental + alternate dental)
	if isDentalStop(a) {
		if isSibilant(b) {
			return "2.2", "dental stop + sibilant"
		}
		if (a == 't' && b == 'ţ') || (a == 'd' && b == 'ḑ') ||
			(a == 't' && b == 'ḑ') || (a == 'd' && b == 'ţ') {
			return "2.2", "dental stop + interdental"
		}
	}

	// 2.3: velar stop + x or ň
	if isVelarStop(a) && (b == 'x' || b == 'ň') {
		return "2.3", "velar stop + " + string(b)
	}

	// 2.4: homologous stop with voicing mismatch
	if isStop(a) && isStop(b) && areHomologous(a, b) && !sameVoicing(a, b) {
		return "2.4", "homologous stop voicing mismatch"
	}

	// 2.5: homologous sibilant with voicing mismatch
	if (isSibilantFricative(a) || isSibilantAffricate(a)) &&
		(isSibilantFricative(b) || isSibilantAffricate(b)) &&
		areHomologous(a, b) && !sameVoicing(a, b) {
		return "2.5", "homologous sibilant voicing mismatch"
	}

	// 2.6: alveolo-palatal fricative + alveolar affricate
	if (a == 'š' || a == 'ž') && (b == 'c' || b == 'ẓ') {
		return "2.6", "alveolo-palatal fricative + alveolar affricate"
	}

	// 2.7: s + ẓ
	if a == 's' && b == 'ẓ' {
		return "2.7", "s + ẓ"
	}

	// 2.8: distinct sibilant fricatives
	if isSibilantFricative(a) && isSibilantFricative(b) && a != b {
		return "2.8", "distinct sibilant fricatives"
	}

	// 2.9: sibilant affricate + sibilant fricative (either order)
	if isSibilantAffricate(a) && isSibilantFricative(b) {
		return "2.9", "sibilant affricate + sibilant fricative"
	}
	if isSibilantFricative(a) && isSibilantAffricate(b) {
		return "2.9", "sibilant fricative + sibilant affricate"
	}

	// 2.10: ç restrictions
	if a == 'ç' && (isSibilantFricative(b) || b == 'ẓ' || b == 'j' ||
		b == 'ļ' || b == 'h') {
		return "2.10", "ç + restricted follower"
	}
	if isSibilantFricative(a) && b == 'ç' {
		return "2.10", "sibilant fricative + ç"
	}
	if isSibilantAffricate(a) && b == 'ç' {
		return "2.10", "sibilant affricate + ç"
	}
	if (a == 'ļ' || a == 'h' || a == 'x') && b == 'ç' {
		return "2.10", string(a) + " + ç"
	}

	// 2.16: ň + velar/uvular/y
	if a == 'ň' && (b == 'k' || b == 'g' || b == 'x' || b == 'y') {
		return "2.16", "ň + " + string(b)
	}

	// 2.11: n + sibilant affricate (nc, nč, nẓ, nj).
	if a == 'n' && isSibilantAffricate(b) {
		return "2.11", "n + sibilant affricate"
	}

	// 2.12: m + labial stop / dental stop / interdental.
	if a == 'm' && (isLabialStop(b) || isDentalStop(b) || b == 'ţ' || b == 'ḑ') {
		return "2.12", "m + " + string(b)
	}

	// 2.14: n + labial stop (np, nb).
	if a == 'n' && isLabialStop(b) {
		return "2.14", "n + labial stop"
	}

	// 2.17: x + sibilant or other prohibited followers
	if a == 'x' {
		if isSibilant(b) {
			return "2.17", "x + sibilant"
		}
		if strings.ContainsRune("gļňyhř", b) {
			return "2.17", "x + " + string(b)
		}
	}

	// 2.18: ļ restrictions
	if a == 'ļ' && isVoicedStop(b) {
		return "2.18", "ļ + voiced stop"
	}
	if a == 'h' && b == 'ļ' {
		return "2.18", "h + ļ"
	}
	if a == 'ļ' && isSibilantFricative(b) {
		return "2.18", "ļ + sibilant fricative"
	}

	// 2.19: as the final member of a conjunct, -h- cannot follow ļ, x, or ç.
	// Encoded as a pair check; the "as final" position constraint is
	// handled at the cluster level in ValidateClusterAt.
	if b == 'h' && (a == 'ļ' || a == 'x' || a == 'ç') {
		return "2.19", string(a) + " + h"
	}

	// 2.20: r and h cannot be followed by ř
	if (a == 'r' || a == 'h') && b == 'ř' {
		return "2.20", string(a) + " + ř"
	}

	// 2.21: ř cannot be followed by r
	if a == 'ř' && b == 'r' {
		return "2.21", "ř + r"
	}

	// 2.22: w and y must be conjunct-final (followed by a vowel).
	if (a == 'w' || a == 'y') && !isVowel(b) {
		return "2.22", string(a) + " not at end of conjunct"
	}

	// 2.23: ḑ + sibilant; n + ň
	if a == 'ḑ' && strings.ContainsRune("sšzž", b) {
		return "2.23", "ḑ + sibilant"
	}
	if a == 'n' && b == 'ň' {
		return "2.23", "n + ň"
	}

	// 2.24: çç and ļļ geminates not permitted
	if a == 'ç' && b == 'ç' {
		return "2.24", "çç geminate"
	}
	if a == 'ļ' && b == 'ļ' {
		return "2.24", "ļļ geminate"
	}

	return "", ""
}

// Position labels a cluster's location in a word.
type Position int

const (
	Initial Position = iota
	Medial
	Final
)

func (p Position) String() string {
	return [...]string{"initial", "medial", "final"}[p]
}

// MaxClusterLength returns the maximum allowed consonant-cluster length
// at the given position. Initial and final caps at 4; medial at 6.
func MaxClusterLength(p Position) int {
	switch p {
	case Initial, Final:
		return 4
	case Medial:
		return 6
	}
	return 0
}

// HasProhibitedGeminate reports whether s contains a geminate of '/w/y,
// which are forbidden by rule 1.7.
func HasProhibitedGeminate(s string) bool {
	prev := rune(0)
	first := true
	for _, r := range s {
		if !first && r == prev && (r == '\'' || r == 'w' || r == 'y') {
			return true
		}
		prev = r
		first = false
	}
	return false
}

// ValidateClusterAt checks a cluster at a known position. It runs the
// pair checks plus length, triple-consonant, prohibited-geminate, and
// position-specific rules.
func ValidateClusterAt(p Position, cluster string) Result {
	if cluster == "" {
		return Result{Valid: true}
	}
	var errs []Error

	// Length cap.
	n := 0
	for range cluster {
		n++
	}
	if max := MaxClusterLength(p); n > max {
		errs = append(errs, Error{
			Rule:    "length",
			Cluster: cluster,
			Reason:  fmt.Sprintf("%s cluster exceeds %d runes (got %d)", p, max, n),
		})
	}

	// Triple consonant.
	if HasTripleConsonant(cluster) {
		errs = append(errs, Error{Rule: "1.7", Cluster: cluster, Reason: "triple consonant"})
	}

	// Prohibited geminates.
	if HasProhibitedGeminate(cluster) {
		errs = append(errs, Error{Rule: "1.7", Cluster: cluster, Reason: "prohibited geminate"})
	}

	// Pair rules.
	pairs := ValidateCluster(cluster)
	if !pairs.Valid {
		errs = append(errs, pairs.Errors...)
	}

	// 3-consonant rules (windowed).
	runes := []rune(cluster)
	for i := 0; i+2 < len(runes); i++ {
		a, b, c := runes[i], runes[i+1], runes[i+2]
		// 2.13: nasal + homologous stop + sibilant is prohibited
		// (mps, mbz, ntz, ndz, ňks, ňgz, etc.).
		if isNasal(a) && isStop(b) && areHomologous(a, b) && isSibilant(c) {
			errs = append(errs, Error{
				Rule:    "2.13",
				Cluster: string([]rune{a, b, c}),
				Reason:  "nasal + homologous stop + sibilant",
			})
		}
		// 2.15: nf or nv followed by any consonant is prohibited —
		// these clusters must be followed by a vowel.
		if a == 'n' && (b == 'f' || b == 'v') && !isVowel(c) {
			errs = append(errs, Error{
				Rule:    "2.15",
				Cluster: string([]rune{a, b, c}),
				Reason:  "nf/nv must be followed by vowel",
			})
		}
		// 2.12 triples: m + bilabial stop + bilabial / interdental
		// fricative or dental stop is prohibited because the medial
		// stop is phonetically indistinct (mpf ≈ mf, mbd ≈ md, etc.).
		if a == 'm' {
			if (b == 'p' && (c == 'f' || c == 'ţ')) ||
				(b == 'b' && (c == 'v' || c == 'ḑ' || c == 'd')) {
				errs = append(errs, Error{
					Rule:    "2.12",
					Cluster: string([]rune{a, b, c}),
					Reason:  "m + bilabial stop + indistinct follower",
				})
			}
		}
		// 2.12: ngḑ specifically called out alongside the m-cluster
		// list; *nkţ* is explicitly permitted.
		if a == 'n' && b == 'g' && c == 'ḑ' {
			errs = append(errs, Error{Rule: "2.12", Cluster: "ngḑ", Reason: "ngḑ prohibited (vs. nkţ allowed)"})
		}
	}

	// Position-specific rules.
	switch p {
	case Initial:
		if cluster == "ļ" {
			errs = append(errs, Error{Rule: "3.1", Cluster: cluster, Reason: "ļ alone not allowed word-initially"})
		}
		if runeLen(cluster) > 1 && firstRune(cluster) == '\'' {
			errs = append(errs, Error{Rule: "1.5", Cluster: cluster, Reason: "glottal stop word-initial within cluster"})
		}
	case Medial:
		// 5.1: single intervocalic -ļ- is not permitted (collides
		// with the allophonically-identical -hl-).
		if cluster == "ļ" {
			errs = append(errs, Error{Rule: "5.1", Cluster: cluster, Reason: "ļ alone not allowed intervocalically"})
		}
	case Final:
		last := lastRune(cluster)
		if last == 'w' || last == 'y' {
			errs = append(errs, Error{Rule: "4.1", Cluster: cluster, Reason: string(last) + " word-finally"})
		}
		if last == '\'' && runeLen(cluster) > 1 {
			errs = append(errs, Error{Rule: "4.1", Cluster: cluster, Reason: "glottal stop word-finally"})
		}
	}

	if len(errs) == 0 {
		return Result{Valid: true}
	}
	return Result{Valid: false, Errors: errs}
}

func runeLen(s string) int {
	n := 0
	for range s {
		n++
	}
	return n
}

func firstRune(s string) rune {
	for _, r := range s {
		return r
	}
	return 0
}

func lastRune(s string) rune {
	var last rune
	for _, r := range s {
		last = r
	}
	return last
}

// permissibleDiphthongs lists the 10 falling diphthongs per Sec. 1.2.1.
var permissibleDiphthongs = map[string]bool{
	"ai": true, "ei": true, "ëi": true, "oi": true, "ui": true,
	"au": true, "eu": true, "ëu": true, "ou": true, "iu": true,
}

// validDisyllabicConjuncts is the set of two-vowel conjuncts that
// appear in the Series 3/4 vowel-form tables plus reference-root
// markers ae/ea and the Type-3 zero-degree marker üo (also the §4.6.3
// referential epenthesis prefix).
var validDisyllabicConjuncts = map[string]bool{
	// Series 3
	"ia": true, "ie": true, "io": true, "iö": true,
	"eë": true, "uö": true, "uo": true, "ue": true, "ua": true,
	// Series 3 alternates
	"uä": true, "uë": true, "üä": true, "üë": true,
	"öë": true, "öä": true, "ië": true, "iä": true,
	// Series 4
	"ao": true, "aö": true, "eo": true, "eö": true,
	"oë": true, "öe": true, "oe": true, "öa": true, "oa": true,
	// Reference-root markers
	"ae": true, "ea": true,
	// Type-3 zero-degree / §4.6.3 epenthesis
	"üo": true,
}

// ValidateVowelSequence checks a vowel sequence against rules 1.1-1.2.
// Single vowels are always valid; two-vowel sequences must be a
// permissible diphthong or a valid disyllabic conjunct; longer
// sequences are flagged.
func ValidateVowelSequence(seq string) Result {
	n := runeLen(seq)
	switch n {
	case 0, 1:
		return Result{Valid: true}
	case 2:
		if permissibleDiphthongs[seq] || validDisyllabicConjuncts[seq] {
			return Result{Valid: true}
		}
		return Result{Valid: false, Errors: []Error{
			{Rule: "1.2", Cluster: seq, Reason: "not a permissible diphthong or disyllabic conjunct"},
		}}
	default:
		// Three-vowel sequences may appear as glottalized cases (e.g.
		// "a'a" with the apostrophe stripped to "aa"), but apostrophe
		// glottalization isn't normalized here. Treat 3+ as invalid.
		return Result{Valid: false, Errors: []Error{
			{Rule: "1.2", Cluster: seq, Reason: "vowel sequence too long"},
		}}
	}
}

// ValidateCluster checks every adjacent rune pair in s. A non-Valid
// Result lists every violation found (not just the first).
func ValidateCluster(s string) Result {
	var errs []Error
	prev := rune(0)
	first := true
	for _, r := range s {
		if !first {
			if rule, reason := CheckProhibitedPair(prev, r); rule != "" {
				errs = append(errs, Error{
					Rule:    rule,
					Cluster: string([]rune{prev, r}),
					Reason:  reason,
				})
			}
		}
		first = false
		prev = r
	}
	if len(errs) == 0 {
		return Result{Valid: true}
	}
	return Result{Valid: false, Errors: errs}
}

// HasTripleConsonant reports whether s contains three identical
// consonants in a row (geminate + same consonant), which is universally
// prohibited.
func HasTripleConsonant(s string) bool {
	runes := []rune(s)
	for i := 0; i+2 < len(runes); i++ {
		a, b, c := runes[i], runes[i+1], runes[i+2]
		if a == b && b == c && !isVowel(a) {
			return true
		}
	}
	return false
}

//------------------------------------------------------------------------------
// Phoneme classification helpers.
//------------------------------------------------------------------------------

func isStop(c rune) bool { return strings.ContainsRune("ptdkgb", c) }

func isDentalStop(c rune) bool { return c == 't' || c == 'd' }

func isVelarStop(c rune) bool { return c == 'k' || c == 'g' }

func isLabialStop(c rune) bool { return c == 'p' || c == 'b' }

func isVoicedStop(c rune) bool { return strings.ContainsRune("bdg", c) }

func isSibilantFricative(c rune) bool { return strings.ContainsRune("sšzž", c) }

func isSibilantAffricate(c rune) bool { return strings.ContainsRune("cčẓj", c) }

func isSibilant(c rune) bool { return isSibilantFricative(c) || isSibilantAffricate(c) }

func isNasal(c rune) bool { return c == 'm' || c == 'n' || c == 'ň' }

// voicedOf returns the voiced counterpart of c, or c itself if already
// voiced. Returns 0 if c is not part of a voicing pair.
func voicedOf(c rune) rune {
	switch c {
	case 'p', 'b':
		return 'b'
	case 't', 'd':
		return 'd'
	case 'k', 'g':
		return 'g'
	case 'f', 'v':
		return 'v'
	case 'ţ', 'ḑ':
		return 'ḑ'
	case 's', 'z':
		return 'z'
	case 'š', 'ž':
		return 'ž'
	case 'c', 'ẓ':
		return 'ẓ'
	case 'č', 'j':
		return 'j'
	}
	return 0
}

// placeGroup returns a place-of-articulation group for two-consonant
// homology checks. 0 means "no homologous restriction".
func placeGroup(c rune) int {
	switch c {
	case 'p', 'b', 'f', 'v', 'm':
		return 1 // labial
	case 't', 'd', 'ţ', 'ḑ', 'n':
		return 2 // dental
	case 's', 'z', 'c', 'ẓ':
		return 3 // alveolar
	case 'š', 'ž', 'č', 'j':
		return 4 // alveolo-palatal
	case 'k', 'g', 'ň':
		return 5 // velar
	case 'x':
		return 6 // uvular
	}
	return 0
}

// areHomologous reports whether a and b share a place of articulation.
func areHomologous(a, b rune) bool {
	pa, pb := placeGroup(a), placeGroup(b)
	return pa > 0 && pa == pb
}

// sameVoicing reports whether a and b have the same voicing status.
// Consonants that don't participate in a voicing pair are treated as
// matching everything.
func sameVoicing(a, b rune) bool {
	va, vb := voicedOf(a), voicedOf(b)
	if va == 0 || vb == 0 {
		return true
	}
	return (a == va) == (b == vb)
}

func isVowel(r rune) bool {
	switch r {
	case 'a', 'ä', 'e', 'ë', 'i', 'o', 'ö', 'u', 'ü':
		return true
	}
	return false
}

// IsVoicedStop is exported because the cluster-level rules in callers
// sometimes need the predicate. (Kept thin to avoid leaking the rest.)
func IsVoicedStop(c rune) bool { return isVoicedStop(c) }
