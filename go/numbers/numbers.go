// Package numbers implements the Ithkuil V4 centesimal (base-100)
// number system: roots for digits 0-10, the TNX affix for tens, power
// roots for 100^n, and helpers for assembling number formatives plus
// the dedicated month and day-of-week affix tables.
package numbers

import "fmt"

// DigitRoots holds the root consonants for digits 0-10 (per ch.13 of
// the reference grammar).
var DigitRoots = [...]string{
	"vr", // 0
	"ll", // 1
	"ks", // 2
	"z",  // 3
	"pš", // 4
	"st", // 5
	"cp", // 6
	"ns", // 7
	"čk", // 8
	"lẓ", // 9
	"j",  // 10
}

// PowerRoots holds the root consonants for ascending powers of 100.
// Index 0 is the units place (no consonant); subsequent entries are
// 100^1, 100^2, 100^4, 100^8.
var PowerRoots = [...]string{
	"",   // 100^0 = units
	"gz", // 100^1 = 100
	"pc", // 100^2 = 10,000
	"kẓ", // 100^4 = 100,000,000
	"čg", // 100^8 = 10,000,000,000,000,000
}

// NumberRoot returns the root consonant for the number n. For 0-10
// it's the direct digit root; for 11-99 it's the ones-digit root
// (the tens are conveyed by an accompanying TNX affix). Out-of-range
// inputs return ok=false.
func NumberRoot(n int) (string, bool) {
	switch {
	case n < 0, n >= 100:
		return "", false
	case n <= 10:
		return DigitRoots[n], true
	default:
		return DigitRoots[n%10], true
	}
}

// NumberAffix returns the TNX affix (Cs="rs", degree=tens) for
// numbers 11-99. Numbers 0-10 have no TNX affix; out-of-range returns
// ok=false.
func NumberAffix(n int) (cs string, degree int, ok bool) {
	if n <= 10 || n >= 100 {
		return "", 0, false
	}
	return "rs", n / 10, true
}

// NumberStem distinguishes the four ways a number formative can be
// interpreted: cardinal (counting), ordinal (ranking), partitive
// (a subset), or collective (a group).
type NumberStem int

const (
	NSCardinal NumberStem = iota
	NSOrdinal
	NSPartitive
	NSCollective
)

func (s NumberStem) String() string {
	return [...]string{"Cardinal", "Ordinal", "Partitive", "Collective"}[s]
}

// NumberVersion distinguishes concrete (specific) from abstract
// (approximate) number readings.
type NumberVersion int

const (
	NVConcrete NumberVersion = iota
	NVAbstract
)

func (v NumberVersion) String() string {
	return [...]string{"Concrete", "Abstract"}[v]
}

// NumberVv encodes a (stem, version) pair as its Vv vowel. The
// number formative occupies the same Vv slot as a regular formative
// but uses a custom 8-cell mapping.
func NumberVv(stem NumberStem, ver NumberVersion) string {
	switch {
	case stem == NSCardinal && ver == NVConcrete:
		return "a"
	case stem == NSCardinal && ver == NVAbstract:
		return "u"
	case stem == NSOrdinal && ver == NVConcrete:
		return "e"
	case stem == NSOrdinal && ver == NVAbstract:
		return "i"
	case stem == NSPartitive && ver == NVConcrete:
		return "o"
	case stem == NSPartitive && ver == NVAbstract:
		return "ö"
	case stem == NSCollective && ver == NVConcrete:
		return "ä"
	case stem == NSCollective && ver == NVAbstract:
		return "ü"
	}
	panic(fmt.Sprintf("numbers: unreachable NumberVv(%v, %v)", stem, ver))
}

// ConstructNumber builds a minimal number formative for n in 0-99.
// Higher values (compound centesimal forms) are not yet supported.
// Returns ok=false for n outside [0, 99].
func ConstructNumber(n int, stem NumberStem, ver NumberVersion) (string, bool) {
	cr, ok := NumberRoot(n % 100)
	if !ok {
		return "", false
	}
	if n >= 100 || n < 0 {
		return "", false
	}
	vv := NumberVv(stem, ver)
	vr := "a" // basic specification
	ca := "l" // default Ca
	return vv + cr + vr + ca, true
}

// MonthAffixes are the dedicated affix forms for months 1-12. Index 0
// is January-equivalent.
var MonthAffixes = [...]string{
	"lks",  // 1
	"lz",   // 2
	"lps",  // 3
	"lst",  // 4
	"lcp",  // 5
	"lns",  // 6
	"lčk",  // 7
	"llẓ",  // 8
	"lpc",  // 9
	"lj",   // 10
	"ljks", // 11
	"ljz",  // 12
}

// DayOfWeekAffixes are the dedicated affix forms for days 1-7. Index 0
// is Monday-equivalent.
var DayOfWeekAffixes = [...]string{
	"mks", // 1
	"mz",  // 2
	"mps", // 3
	"mst", // 4
	"mcp", // 5
	"mns", // 6
	"mčk", // 7
}

// ParseNumberRoot decodes a digit-root consonant cluster (vr/ll/ks/…)
// to its integer 0-10. Returns ok=false on no match.
func ParseNumberRoot(t string) (int, bool) {
	for i, r := range DigitRoots {
		if r == t {
			return i, true
		}
	}
	return 0, false
}
