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

	// 2.5: homologous fricative or affricate with a voicing mismatch.
	// The rule says "applies to fricatives and to affricates" — the
	// two manners separately, not across them. Its whole example list
	// pairs like with like: fv, vf, ţḑ, ḑţ are fricative-fricative and
	// cẓ, ẓc, čj, jč are affricate-affricate. Reading it across manners
	// rejects zc 'chop/dice', žč and šj, which the lexicon and the §4.4
	// examples both use.
	if isFricative(a) && isFricative(b) &&
		areHomologous(a, b) && !sameVoicing(a, b) {
		return "2.5", "homologous fricative voicing mismatch"
	}
	if isSibilantAffricate(a) && isSibilantAffricate(b) &&
		areHomologous(a, b) && !sameVoicing(a, b) {
		return "2.5", "homologous affricate voicing mismatch"
	}

	// 2.5: an alveolo-palatal affricate cannot precede an alveolar
	// one. The reverse order (cč, cj, ẓč, ẓj) is explicitly permitted,
	// so this is about sequence, not about the pair.
	if (a == 'č' || a == 'j') && (b == 'c' || b == 'ẓ') {
		return "2.5", "alveolo-palatal affricate + alveolar affricate"
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

	// 2.9: sibilant affricate followed by a sibilant fricative. The
	// rule is one-directional — every form it names (čs, cz, ẓz, čž,
	// ẓs, js, jz, jš) has the affricate first. The reverse order is
	// ordinary: sc 'wash/bathe', zc 'chop/dice', šč and žč all appear
	// in the lexicon, and weščayá, žžjádu'u and arţtudëužči'a in the
	// §4.4/§6 examples. What the reverse order *is* barred for is the
	// alveolo-palatal fricative before an apico-alveolar affricate
	// (§2.6, just above) and s before ẓ (§2.7).
	if isSibilantAffricate(a) && isSibilantFricative(b) {
		return "2.9", "sibilant affricate + sibilant fricative"
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

	// §2.12 constrains m + bilabial stop + a third consonant, so it is
	// a rule about triples and is checked in ValidateCluster. The pair
	// on its own is fine: §2.12 names mt, md, mţ and mḑ as the very
	// forms the prohibited triples collapse to, §2.13 builds mps and
	// mbz on top of mp and mb, and §2.14 bans np/nb precisely because
	// they assimilate to the permitted mp/mb.

	// 2.14: n + labial stop (np, nb).
	if a == 'n' && isLabialStop(b) {
		return "2.14", "n + labial stop"
	}

	// 2.17: x + sibilant fricative or other prohibited followers. The
	// rule lists the four sibilant fricatives by name and stops there;
	// the affricates are not on it, and xc 'equine', xč 'murder', xj
	// 'tapir' and xẓ 'dusty' are all live roots.
	if a == 'x' {
		if isSibilantFricative(b) {
			return "2.17", "x + sibilant fricative"
		}
		if strings.ContainsRune("gļňyhř", b) {
			return "2.17", "x + " + string(b)
		}
	}

	// 2.18: ļ restrictions. The rule is directional — ļ "cannot be
	// preceded by a voiced stop nor by -h- or -ç-. It cannot be
	// followed by any sibilant fricative nor by -h- or -ç-." So the
	// prohibited pairs are dļ/gļ/bļ, not ļd/ļg/ļb.
	if isVoicedStop(a) && b == 'ļ' {
		return "2.18", "voiced stop + ļ"
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

	// §2.24 bars çç and ļļ, but the morphology generates both and the
	// spec contradicts itself on this point. §3.6.1 rule 4 geminates a
	// sibilant "in any position" and gives çkl → ççkl as its own
	// worked example; rule 6 gives tçkl → tççkl. The bias-adjunct
	// table holds pļļ (CMD) and kçç (EXA). The Discord corpus has 40
	// words with çç or ļļ, among them formatives whose geminated Ca
	// marks the end of Slot V: wiapļļalká, hamphelsuirççaité.
	//
	// So §2.24 constrains the root and affix conjuncts that are §2's
	// subject; it does not reach a form the grammar itself builds.
	// Enforcing it here rejected every one of those words.

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
		// §1.2.2 assimilates the dental n to velar [ŋ] before k and g,
		// so "nks" is the same sequence of sounds as "ňks" and falls
		// under this rule as well. The spec lists both.
		if isNasal(a) && isStop(b) && isSibilant(c) &&
			(areHomologous(a, b) || (a == 'n' && (b == 'k' || b == 'g'))) {
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
		// §3.1 and §3.2 are an inventory of what may open a word, and
		// ValidWordInitial already holds it. Consult it here so the
		// two do not drift: without this, *pz, *kļ and *pm passed
		// general validation while the elision guard rejected them.
		//
		// Only the one- and two-consonant cases delegate. §3.3 and
		// §3.4 are approximated conservatively there, which is right
		// for declining an elision — the cost is a syllable — but as a
		// validity verdict it would reject legal words.
		//
		// Geminates are the exception at any length: §6.3.1, §6.4.1
		// and §6.5 state exactly which conjuncts hold, so there is no
		// approximation to be careful about.
		if n := runeLen(cluster); n == 1 || n == 2 || hasGeminate(cluster) {
			if !ValidWordInitial(cluster) {
				rule := "3.2"
				switch {
				case n == 1:
					rule = "3.1"
				case hasGeminate(cluster):
					rule = "6.2"
				}
				errs = append(errs, Error{
					Rule:    rule,
					Cluster: cluster,
					Reason:  "not permissible word-initially",
				})
			}
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
// appear in the Series 3/4 vowel-form tables plus the four markers in
// the "0" row of the Slot IV Cs-root table.
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
	// Zero-degree markers from the Slot IV Cs-root table's "0" row,
	// "ae | ea | üo | üö". üo is also the §4.6.3 referential epenthesis
	// prefix, and üö is the §3.5/§3.7 Ca-stacking Vx.
	"üo": true, "üö": true,
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

// ValidWordInitial reports whether cluster is permissible at the start
// of a word, per §3.1 (single consonant) and §3.2 (biconsonantal). It
// matters because the renderer may elide a leading default Vv, which
// moves the root cluster into word-initial position where a narrower
// set of clusters is legal than medially.
//
// Clusters of three or more are conservatively rejected unless §3.3's
// opening condition holds — a word-initial stop. Declining a legal
// elision only costs a syllable; allowing an illegal one emits a word
// nobody can say.
//
// §3 opens by exempting geminates ("not including rules for geminated
// forms — see Sec. 6"), so those go to validInitialGeminate first.
func ValidWordInitial(cluster string) bool {
	rs := []rune(cluster)
	if ok, handled := validInitialGeminate(rs); handled {
		return ok
	}
	switch len(rs) {
	case 0:
		return false
	case 1:
		// §3.1: any single consonant except ļ, which is indistinguishable
		// from the allophone of word-initial hl-.
		return rs[0] != 'ļ'
	case 2:
		return validInitialPair(rs[0], rs[1])
	case 3:
		return validInitialTriple(rs[0], rs[1], rs[2])
	case 4:
		// §3.4.1: a tri-conjunct ending in a stop takes a liquid or
		// approximant. §3.4.2: sibilant + stop + l takes a following -y.
		if !validInitialTriple(rs[0], rs[1], rs[2]) {
			return false
		}
		if isStop(rs[2]) {
			return strings.ContainsRune("rlřwy", rs[3])
		}
		return isSibilantFricative(rs[0]) && isStop(rs[1]) && rs[2] == 'l' && rs[3] == 'y'
	}
	return false
}

// validInitialGeminate implements §6.2, §6.3.1 and §6.4.1 for a
// word-initial conjunct containing a geminate. handled is false when
// the conjunct has no geminate, leaving it to the §3 rules.
//
// §6.2: any consonant that geminates intervocalically may also
// geminate word-initially, except the stops. §6.1 excludes ', w and y
// from gemination outright. That is what lets rrala 'cat', mřřala,
// sstilomke, vvralá and žžjádu'u start the way they do.
// hasGeminate reports whether s contains a doubled consonant.
func hasGeminate(s string) bool {
	var prev rune
	for i, r := range s {
		if i > 0 && r == prev {
			return true
		}
		prev = r
	}
	return false
}

func validInitialGeminate(rs []rune) (ok, handled bool) {
	geminateAt := -1
	for i := 0; i+1 < len(rs); i++ {
		if rs[i] == rs[i+1] {
			if geminateAt >= 0 {
				// §6.5: only one geminate pair per conjunct.
				return false, true
			}
			geminateAt = i
		}
	}
	if geminateAt < 0 {
		return false, false
	}
	c := rs[geminateAt]
	if isStop(c) || c == '\'' || c == 'w' || c == 'y' {
		return false, true
	}
	switch {
	case len(rs) == 2:
		return true, true
	case len(rs) == 3 && geminateAt == 0:
		// §6.3.1: #C₁C₁C₂- is fine when #C₁C₂- is.
		return validInitialPair(rs[0], rs[2]), true
	case len(rs) == 3 && geminateAt == 1:
		// §6.4.1: #C₁C₂C₂- is fine when #C₁C₂- is.
		return validInitialPair(rs[0], rs[1]), true
	}
	return false, true
}

// validInitialTriple implements §3.3 for a three-consonant word-initial
// conjunct.
func validInitialTriple(a, b, c rune) bool {
	isLiquid := func(r rune) bool { return r == 'l' || r == 'r' }
	voiced := func(r rune) bool { return strings.ContainsRune("bdgvḑzžẓj", r) }
	sameVoicing := func(x, y rune) bool { return voiced(x) == voiced(y) }
	nonSibilantFricative := func(r rune) bool { return strings.ContainsRune("fvţḑçxļh", r) }

	// §3.3.4: a sibilant fricative or ç- plus a same-voiced stop takes a
	// liquid or approximant; plus a nasal, it takes whatever semi-
	// consonant that nasal admits in a pair.
	if isSibilantFricative(a) || a == 'ç' {
		if isStop(b) && sameVoicing(a, b) {
			return strings.ContainsRune("rlřwy", c)
		}
		return isNasal(b) && (c == 'w' || c == 'y') && validInitialPair(b, c)
	}
	// §3.3.6: the same shape for a sibilant affricate.
	if isSibilantAffricate(a) {
		if isStop(b) && sameVoicing(a, b) {
			return strings.ContainsRune("rlřwy", c)
		}
		return isNasal(b) && (c == 'w' || c == 'y') && validInitialPair(b, c)
	}
	// §3.3.5: the h- triples are an explicit closed list.
	if a == 'h' {
		switch string([]rune{a, b, c}) {
		case "hlw", "hrw", "hmw", "hnw", "hmy", "hny", "hll", "hrr", "hmm", "hnn":
			return true
		}
		return false
	}
	// §3.3.7: fl- and ţl- take -w or -y. Any other fricative-plus-liquid
	// triple is intervocalic only.
	if a == 'f' || a == 'ţ' {
		return b == 'l' && (c == 'w' || c == 'y')
	}
	// §3.3.8: the x- triples are an explicit closed list.
	if a == 'x' {
		switch {
		case (b == 'p' || b == 't') && strings.ContainsRune("lrwy", c):
			return true
		case (b == 'm' || b == 'n') && (c == 'w' || c == 'y'):
			return true
		case (b == 'c' || b == 'č') && c == 'w':
			return true
		}
		return false
	}
	if !isStop(a) {
		return false
	}
	// A stop other than t/d, plus a same-voiced sibilant fricative, plus
	// anything that may follow that sibilant in a pair.
	if !isDentalStop(a) && isSibilantFricative(b) && sameVoicing(a, b) {
		if validInitialPair(b, c) {
			return true
		}
	}
	// Any stop plus a same-voiced non-sibilant fricative other than x,
	// then only the approximant that fricative admits.
	if nonSibilantFricative(b) && b != 'x' && sameVoicing(a, b) {
		if (c == 'w' || c == 'y') && validInitialPair(b, c) {
			return true
		}
	}
	// §3.3.1: stop + l/r takes -w or -y.
	if isLiquid(b) && (c == 'w' || c == 'y') {
		return true
	}
	// §3.3.2: voiceless stop + ç takes a nasal.
	if b == 'ç' && !voiced(a) && isNasal(c) {
		return true
	}
	// §3.3.3: p-/k- with f or ţ takes -y or -w; pļ- and tļ- take -y.
	if (a == 'p' || a == 'k') && (b == 'f' || b == 'ţ') && (c == 'y' || c == 'w') {
		return true
	}
	if (a == 'p' || a == 't') && b == 'ļ' && c == 'y' {
		return true
	}
	return false
}

// validInitialPair implements the §3.2 sub-rules for a two-consonant
// word-initial conjunct.
func validInitialPair(a, b rune) bool {
	isLiquid := func(c rune) bool { return c == 'l' || c == 'r' }
	isApprox := func(c rune) bool { return c == 'w' || c == 'y' || c == 'ř' }
	// "Same voicing" is a voicing class, not a place: p and ţ are both
	// voiceless and so may pair, while p and ḑ may not.
	voiced := func(c rune) bool { return strings.ContainsRune("bdgvḑzžẓj", c) }
	sameVoicing := func(x, y rune) bool { return voiced(x) == voiced(y) }
	// §3.2 pairs a stop with a non-sibilant fricative only across a
	// place difference; the labials, dentals and velars each rule out
	// their own fricative.
	samePlace := func(x, y rune) bool { return voicedOf(x) != 0 && voicedOf(x) == voicedOf(y) }

	switch {
	// §3.2.9: word-initial l- and r- take only -w or -y.
	case isLiquid(a):
		return b == 'w' || b == 'y'
	// §3.2.8: m- and n- take a liquid or an approximant; ň- excludes -y and -ř.
	case isNasal(a):
		if a == 'ň' {
			return isLiquid(b) || b == 'w'
		}
		return isLiquid(b) || isApprox(b)
	// §3.2.7: h- takes -l, -r, -m, -n or -w.
	case a == 'h':
		return isLiquid(b) || b == 'm' || b == 'n' || b == 'w'
	// §3.2.1: a sibilant fricative takes any same-voiced consonant except
	// another sibilant fricative, -ļ and -h; plus any nasal, liquid,
	// approximant or -v regardless of voicing.
	case isSibilantFricative(a):
		if isNasal(b) || isLiquid(b) || isApprox(b) || b == 'v' {
			return true
		}
		return sameVoicing(a, b) && !isSibilantFricative(b) && b != 'ļ' && b != 'h'
	// §3.2.2: a sibilant affricate takes liquids, nasals, -w, same-voiced
	// stops, same-voiced non-sibilant fricatives other than -ļ, and -v.
	case isSibilantAffricate(a):
		if isLiquid(b) || isNasal(b) || b == 'w' || b == 'v' {
			return true
		}
		if isSibilantFricative(b) || isSibilantAffricate(b) || b == 'y' || b == 'ļ' {
			return false
		}
		return sameVoicing(a, b)
	// §3.2.3 and §3.2.4: the two dorsal fricatives have explicit lists.
	case a == 'x':
		return strings.ContainsRune("ptcčmnlrw", b)
	case a == 'ç':
		return strings.ContainsRune("ptcčkmnňlrřw", b)
	// §3.2.5: f, v, ţ and ḑ take any liquid, approximant or nasal, plus
	// same-voiced stops and affricates.
	case a == 'f' || a == 'v' || a == 'ţ' || a == 'ḑ':
		if isLiquid(b) || isApprox(b) || isNasal(b) {
			return true
		}
		return (isStop(b) || isSibilantAffricate(b)) && sameVoicing(a, b)
	// §3.2.6: ļ- takes a voiceless stop or affricate, a nasal, -w or -y.
	case a == 'ļ':
		return strings.ContainsRune("ptkcč", b) || isNasal(b) || b == 'w' || b == 'y'
	// §3.2: a stop takes any liquid or approximant; a same-voiced sibilant
	// fricative; or a same-voiced non-sibilant fricative at a different
	// place of articulation. The kļ and initial-nasal exceptions follow.
	case isStop(a):
		if isLiquid(b) || isApprox(b) {
			return true
		}
		if isNasal(b) {
			// Bilabial and dental stops cannot be followed by a nasal;
			// velar stops take -m or -n.
			return isVelarStop(a) && (b == 'm' || b == 'n')
		}
		if b == 'ļ' {
			return a != 'k'
		}
		if isSibilantFricative(b) {
			// Only the labials and velars take a sibilant; §3.2 excludes
			// the dentals (*ts, *dz would clash with the affricates).
			return !isDentalStop(a) && sameVoicing(a, b)
		}
		return sameVoicing(a, b) && !samePlace(a, b)
	}
	return false
}

// isFricative covers the fricatives that take part in the §2.5
// homologous-voicing pairs: the sibilants plus f/v and ţ/ḑ.
func isFricative(r rune) bool {
	switch r {
	case 'f', 'v', 'ţ', 'ḑ':
		return true
	}
	return isSibilantFricative(r)
}
