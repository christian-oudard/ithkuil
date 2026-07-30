// Package numbers implements the Ithkuil V4 centesimal (base-100)
// number system per §8 of the reference grammar.
//
// A number formative is a regular grammar.Formative whose Cr is one of
// the dedicated number-root clusters. Slot II's (Stem, Version) is
// reinterpreted by convention as (NumberStem, NumberVersion); a number
// 11-99 carries the TNX affix (-rs) at the tens degree in Slot VII.
//
// Compound numbers (≥100) are multi-word phrases that link smaller
// base units to larger ones via the PARTITIVE case. §8.1 says only
// that numbers from 101 "are formed as in Ithkuil-2011 using the
// COMITATIVE case and the COO affix", so the construction itself is
// documented in that grammar's ch. 13. The simplest pattern,
// illustrated by *ksalirsa gzalui walẓärs* = 4229, is a chain of
// [count, magnitude-in-PAR] pairs followed by a trailing ones-block
// (0-99). The richer ch. 13 patterns involving
// COMITATIVE linkers and the COO/1 coordinative affix are not yet
// produced by the encoder, but the decoder accepts them.
package numbers

import (
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/roman"
)

// DigitRoots holds the root consonants for digits 0-10. Index n is the
// root for the integer n.
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

// NonDecimalRoots holds the additional digit roots for non-decimal
// bases 11-15. These see use in compound number expressions and where
// the spec invokes a non-decimal base explicitly (e.g. month 11 uses
// -CG- in the spoken-number gloss).
var NonDecimalRoots = [...]string{
	"cg", // 11
	"jd", // 12
	"ļj", // 13
	"bc", // 14
	"ţẓ", // 15
}

// PowerRoots holds the roots for ascending powers of 100. Index i
// corresponds to 100^(2^(i-1)) for i ≥ 1; index 0 is the empty units
// placeholder. Per §8.1, dedicated roots stop at 100^8; beyond that,
// numbers are referred to as multiples of these units.
var PowerRoots = [...]string{
	"",   // 100^0 = units (no consonant)
	"gz", // 100^1 = 100
	"pc", // 100^2 = 10,000
	"kẓ", // 100^4 = 100,000,000
	"čg", // 100^8 = 10,000,000,000,000,000
}

// powerValues holds the integer value of each entry in PowerRoots so
// the encoder can convert between "place" indices and magnitudes.
var powerValues = [...]int64{
	1,
	100,
	10_000,
	100_000_000,
	10_000_000_000_000_000,
}

// tnxCs is the affix consonant for the TNX (-rs) affix that encodes
// the tens digit for numbers 11-99. Degrees 1-9 add 10, 20, ..., 90.
const tnxCs = "rs"

// Stem distinguishes the four readings of a number formative.
type Stem int

const (
	Cardinal   Stem = iota // counting (default)
	Ordinal                // ranking
	Partitive              // a subset
	Collective             // a group
)

func (s Stem) String() string {
	return [...]string{"Cardinal", "Ordinal", "Partitive", "Collective"}[s]
}

// Version distinguishes concrete (specific) from abstract (approximate)
// number readings.
type Version int

const (
	Concrete Version = iota
	Abstract
)

func (v Version) String() string {
	return [...]string{"Concrete", "Abstract"}[v]
}

// Number bundles the integer value of a number formative with its
// Stem and Version annotations.
type Number struct {
	Value   int64
	Stem    Stem
	Version Version
}

// slotIIForNumber maps a (Stem, Version) onto the regular SlotII
// encoding so the resulting Vv vowel matches §8.1's 8-cell custom
// table. The Stem/Version names are reinterpreted by convention when
// the formative's Cr is a number root.
//
//	Cardinal/Concrete   → S1/PRC  (Vv "a")
//	Cardinal/Abstract   → S3/PRC  (Vv "u")
//	Ordinal/Concrete    → S2/PRC  (Vv "e")
//	Ordinal/Abstract    → S2/CPT  (Vv "i")
//	Partitive/Concrete  → S0/PRC  (Vv "o")
//	Partitive/Abstract  → S0/CPT  (Vv "ö")
//	Collective/Concrete → S1/CPT  (Vv "ä")
//	Collective/Abstract → S3/CPT  (Vv "ü")
var slotIIForNumber = map[struct {
	s Stem
	v Version
}]g.SlotII{
	{Cardinal, Concrete}:   {Stem: g.S1, Version: g.PRC},
	{Cardinal, Abstract}:   {Stem: g.S3, Version: g.PRC},
	{Ordinal, Concrete}:    {Stem: g.S2, Version: g.PRC},
	{Ordinal, Abstract}:    {Stem: g.S2, Version: g.CPT},
	{Partitive, Concrete}:  {Stem: g.S0, Version: g.PRC},
	{Partitive, Abstract}:  {Stem: g.S0, Version: g.CPT},
	{Collective, Concrete}: {Stem: g.S1, Version: g.CPT},
	{Collective, Abstract}: {Stem: g.S3, Version: g.CPT},
}

// numberFromSlotII is the reverse lookup of slotIIForNumber.
var numberFromSlotII = func() map[g.SlotII]struct {
	s Stem
	v Version
} {
	m := make(map[g.SlotII]struct {
		s Stem
		v Version
	}, len(slotIIForNumber))
	for k, v := range slotIIForNumber {
		m[v] = k
	}
	return m
}()

// rootValue maps each number-root cluster to its integer value. Power
// roots map to their magnitude (100, 10000, …). Digit roots 0-15 map
// to 0-15.
var rootValue = func() map[string]int64 {
	m := map[string]int64{}
	for i, r := range DigitRoots {
		m[r] = int64(i)
	}
	for i, r := range NonDecimalRoots {
		m[r] = int64(11 + i)
	}
	for i, r := range PowerRoots {
		if r == "" {
			continue
		}
		m[r] = powerValues[i]
	}
	return m
}()

// IsNumberRoot reports whether cluster names a built-in number root —
// any digit 0-15 or any power-of-100 root.
func IsNumberRoot(cluster string) bool {
	_, ok := rootValue[cluster]
	return ok
}

// RootValue returns the integer value of a number-root cluster. ok=false
// for non-number clusters.
func RootValue(cluster string) (int64, bool) {
	v, ok := rootValue[cluster]
	return v, ok
}

// digitRoot returns the digit-root cluster for a value 0-15. Returns
// ok=false for any other value.
func digitRoot(n int) (string, bool) {
	switch {
	case n < 0:
		return "", false
	case n <= 10:
		return DigitRoots[n], true
	case n <= 15:
		return NonDecimalRoots[n-11], true
	}
	return "", false
}

// Formative builds a number formative for n in 0-99 with the requested
// Stem, Version, and final Case. Returns ok=false for n outside that
// range — use Phrase for compound numbers (n ≥ 100).
//
// 0-10 use the bare digit root. 11-99 use the ones-digit root plus a
// TNX affix at the tens degree in Slot VII.
func Formative(n int, stem Stem, ver Version, c g.Case) (g.Formative, bool) {
	if n < 0 || n >= 100 {
		return g.Formative{}, false
	}
	// Digits 0-10 have dedicated roots; 11-99 split into ones-digit
	// root plus a TNX affix at the tens degree.
	var cr string
	var tens int
	if n <= 10 {
		var ok bool
		cr, ok = digitRoot(n)
		if !ok {
			return g.Formative{}, false
		}
	} else {
		var ok bool
		cr, ok = digitRoot(n % 10)
		if !ok {
			return g.Formative{}, false
		}
		tens = n / 10
	}
	s2 := slotIIForNumber[struct {
		s Stem
		v Version
	}{stem, ver}]
	f := g.MinimalFormative(cr)
	cr2 := f.Root.(g.CrRoot)
	cr2.Stem = s2.Stem
	cr2.Version = s2.Version
	f.Root = cr2
	if tens > 0 {
		f.SlotVII = append(f.SlotVII, g.Affix{
			Type:      g.Type1Affix,
			Degree:    tens,
			Consonant: tnxCs,
		})
	}
	f.Final = g.UnframedNominal{Case: c}
	return f, true
}

// PowerFormative builds a formative for the i-th power-of-100 root
// (i=1 → 100, i=2 → 10000, i=3 → 100^4, i=4 → 100^8). Returns ok=false
// for out-of-range indices. The final case lets callers emit the
// magnitude word in either the PARTITIVE used by spoken compounds or
// another case for standalone reference.
func PowerFormative(i int, stem Stem, ver Version, c g.Case) (g.Formative, bool) {
	if i < 1 || i >= len(PowerRoots) {
		return g.Formative{}, false
	}
	cr := PowerRoots[i]
	s2 := slotIIForNumber[struct {
		s Stem
		v Version
	}{stem, ver}]
	f := g.MinimalFormative(cr)
	cr2 := f.Root.(g.CrRoot)
	cr2.Stem = s2.Stem
	cr2.Version = s2.Version
	f.Root = cr2
	f.Final = g.UnframedNominal{Case: c}
	return f, true
}

// Render builds the formative for n with the given Stem/Version/Case
// and returns its romanization. Convenience wrapper around Formative
// + roman.Formative.
func Render(n int, stem Stem, ver Version, c g.Case) (string, bool) {
	f, ok := Formative(n, stem, ver, c)
	if !ok {
		return "", false
	}
	return roman.Formative(f), true
}

// Decode inspects a parsed Formative and, if its Cr is a number root,
// returns the corresponding Number. ok=false for non-number formatives
// or for number formatives whose Slot II doesn't decode to a known
// (Stem, Version) pair.
//
// Decode handles 0-99 (with an optional TNX affix). For power-root
// formatives (gz/pc/kẓ/čg) the value is the magnitude itself, taken
// from PowerRoots.
func Decode(f g.Formative) (Number, bool) {
	cr, ok := f.Root.(g.CrRoot)
	if !ok {
		return Number{}, false
	}
	base, ok := rootValue[cr.Cluster]
	if !ok {
		return Number{}, false
	}
	nv, ok := numberFromSlotII[g.SlotII{Stem: cr.Stem, Version: cr.Version}]
	if !ok {
		return Number{}, false
	}
	value := base
	// 11-99: a TNX affix in Slot VII adds the tens.
	for _, a := range f.SlotVII {
		if a.Consonant == tnxCs && a.Type == g.Type1Affix {
			value += int64(a.Degree) * 10
		}
	}
	return Number{Value: value, Stem: nv.s, Version: nv.v}, true
}

// Phrase returns the spoken form of n as a sequence of romanizations
// per ch. 13. For 0 ≤ n < 100 the slice has one element. For larger n
// the encoding factors n recursively: at each level the largest fitting
// magnitude p is extracted, the count n/p is itself expressed as a
// phrase (which may chain multiple magnitudes when its value is ≥100),
// and any remainder appends as its own phrase.
//
// Case assignment per ch. 13 (as observed in the spec examples):
//   - magnitude words are always PARTITIVE
//   - the first count of the phrase is THM
//   - a final ones-block (no magnitude follows it) is THM
//   - any other count — one between two magnitudes — is COMITATIVE
//
// gzalui-omission (ch. 13): a *gzalui* (the PARTITIVE of *gzal* = 100)
// that sits between two count words is dropped, since two adjacent
// counts implicitly multiply by 100. *gzalui* between a count and any
// other magnitude is kept, because dropping it would change the value
// (e.g. *wallärsa gzalui pcalui* = 21,000,000 vs. *wallärsa pcalui* =
// 210,000).
//
// Examples (showing semantic structure, not literal rom):
//
//	4229       → [42 THM, 29 THM]                    (gzalui omitted)
//	269,766    → [26 THM, of-10000 PAR, 97 COM, 66 THM]   (one gzalui omitted)
//	21,000,000 → [21 THM, of-100 PAR, of-10000 PAR]  (gzalui kept; required)
//
// Returns ok=false for negative n.
func Phrase(n int64, stem Stem, ver Version) ([]string, bool) {
	if n < 0 {
		return nil, false
	}
	terms := omitRedundantGzalui(phraseTerms(n))
	// Walk terms, rendering each. Count case depends on neighbors:
	// COM only when both neighbors are magnitudes (in the unreduced
	// term list — gzalui-omission doesn't promote a now-adjacent
	// count to THM, because the implicit ×100 still sits between them).
	words := make([]string, 0, len(terms))
	for i, t := range terms {
		if t.isMag {
			w, ok := powerWord(int(t.value), stem, ver)
			if !ok {
				return nil, false
			}
			words = append(words, w)
			continue
		}
		c := g.THM
		prevMag := i > 0 && terms[i-1].isMag
		nextMag := i+1 < len(terms) && terms[i+1].isMag
		// A count is also intermediate when it sits between a previous
		// magnitude and a following count (i.e. a *gzalui* that would
		// have appeared after it was omitted).
		nextCount := i+1 < len(terms) && !terms[i+1].isMag
		if prevMag && (nextMag || nextCount) {
			c = g.COM
		}
		w, ok := Render(int(t.value), stem, ver, c)
		if !ok {
			return nil, false
		}
		words = append(words, w)
	}
	return words, true
}

// omitRedundantGzalui drops any gz-magnitude term that sits between
// two count terms. The implicit ×100 between adjacent counts makes
// the gzalui recoverable by the parser, so per ch. 13 it is normally
// omitted in speech.
func omitRedundantGzalui(terms []phraseTerm) []phraseTerm {
	out := make([]phraseTerm, 0, len(terms))
	for i, t := range terms {
		isGz := t.isMag && t.value == 1 // powerValues[1] = 100 = gz
		prevCount := i > 0 && !terms[i-1].isMag
		nextCount := i+1 < len(terms) && !terms[i+1].isMag
		if isGz && prevCount && nextCount {
			continue
		}
		out = append(out, t)
	}
	return out
}

// phraseTerm is a single position in a number phrase — either a count
// (value 0-99) or a magnitude (value = the powerValues index).
type phraseTerm struct {
	isMag bool
	value int64
}

// phraseTerms decomposes n into the count/magnitude term sequence that
// Phrase walks to assign case. The recursion mirrors Phrase's previous
// shape: at each level the largest fitting magnitude is extracted, the
// count n/p is expanded as its own term sequence (so multi-magnitude
// chains like 21 × 100 × 10⁴ fall out naturally), then the magnitude
// is appended, then the remainder's term sequence follows.
func phraseTerms(n int64) []phraseTerm {
	if n < 100 {
		return []phraseTerm{{value: n}}
	}
	for i := len(powerValues) - 1; i >= 1; i-- {
		p := powerValues[i]
		if p > n {
			continue
		}
		count := n / p
		remainder := n - count*p
		terms := phraseTerms(count)
		terms = append(terms, phraseTerm{isMag: true, value: int64(i)})
		if remainder > 0 {
			terms = append(terms, phraseTerms(remainder)...)
		}
		return terms
	}
	// Unreachable: n ≥ 100 must match at least powerValues[1] = 100.
	return nil
}

// powerWord returns the romanization of the i-th power-of-100 root
// rendered in PARTITIVE case ("gzalui", "wapcui", "ẓkẓalui", "čgalui"),
// agreeing with the surrounding chain's stem/version.
func powerWord(i int, stem Stem, ver Version) (string, bool) {
	f, ok := PowerFormative(i, stem, ver, g.PAR)
	if !ok {
		return "", false
	}
	return roman.Formative(f), true
}

// MonthAffixes lists the dedicated affix forms for months 1-12. Index 0
// corresponds to month 1 (January-equivalent).
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

// DayOfWeekAffixes lists the dedicated affix forms for days 1-7. Index 0
// corresponds to day 1 (Monday-equivalent).
var DayOfWeekAffixes = [...]string{
	"mks", // 1
	"mz",  // 2
	"mps", // 3
	"mst", // 4
	"mcp", // 5
	"mns", // 6
	"mčk", // 7
}

// SPT degree constants name the nine §6 calendar/time positions. Use
// these with SPTFormative to compose date and time-of-day formatives.
const (
	SPTSecond      = 1 // seconds of the minute
	SPTMinute      = 2 // minutes of the hour
	SPTHour        = 3 // hour of the day (time of day)
	SPTDayOfWeek   = 4 // day of the week (1 = Monday)
	SPTDayOfMonth  = 5 // day of the month
	SPTWeekOfMonth = 6 // week of the month
	SPTMonth       = 7 // month of the year
	SPTYear        = 8 // year
	SPTCentury     = 9 // century
)

// SptCs is the affix consonant for the SPT (Specified Points in
// Calendrical Time) affix per §6. The spec gives two romanizations
// (-rw- / -ry-); -rw- is the canonical pre-vowel form.
const SptCs = "rw"

// SPTFormative builds a date/time formative for value n at the given
// SPT degree (1-9, names above) with the requested Stem and Version.
// The spec recommends Cardinal/Abstract (Vv "u") for typical use.
// Returns ok=false for n outside 0-99 or sptDegree outside 1-9.
//
// The case is fixed to THM; callers needing a different case (e.g.
// SIT for "in the situation of the Nth ...") should override f.Final
// on the returned formative.
func SPTFormative(n int, sptDegree int, stem Stem, ver Version) (g.Formative, bool) {
	if sptDegree < 1 || sptDegree > 9 {
		return g.Formative{}, false
	}
	f, ok := Formative(n, stem, ver, g.THM)
	if !ok {
		return g.Formative{}, false
	}
	f.SlotVII = append(f.SlotVII, g.Affix{
		Type:      g.Type1Affix,
		Degree:    sptDegree,
		Consonant: SptCs,
	})
	return f, true
}

// SPTDegree inspects a Formative and, if its Slot VII carries an SPT
// affix, returns (degree, true) where degree names which calendar/time
// position the formative denotes. Returns (0, false) when no SPT affix
// is present.
func SPTDegree(f g.Formative) (int, bool) {
	for _, a := range f.SlotVII {
		if a.Consonant == SptCs {
			return a.Degree, true
		}
	}
	return 0, false
}

// RenderSPT builds and renders the SPT formative for n at the given
// SPT degree. Uses the canonical w-shortcut romanization (Cc=w,
// Vr/Ca elided) so the output matches spec convention for date and
// time-of-day expressions. Returns ok=false for out-of-range inputs.
func RenderSPT(n int, sptDegree int, stem Stem, ver Version) (string, bool) {
	f, ok := SPTFormative(n, sptDegree, stem, ver)
	if !ok {
		return "", false
	}
	return roman.Formative(f), true
}

// SPTDegreeLabel returns a human-readable label for an SPT degree, e.g.
// "hour" for SPTHour. Returns "" for out-of-range degrees.
func SPTDegreeLabel(d int) string {
	switch d {
	case SPTSecond:
		return "second"
	case SPTMinute:
		return "minute"
	case SPTHour:
		return "hour"
	case SPTDayOfWeek:
		return "weekday"
	case SPTDayOfMonth:
		return "day"
	case SPTWeekOfMonth:
		return "week"
	case SPTMonth:
		return "month"
	case SPTYear:
		return "year"
	case SPTCentury:
		return "century"
	}
	return ""
}

// MonthAffix returns the Cs cluster encoding month m (1-12). Returns
// ok=false for out-of-range m. The affix is intended to attach to the
// SPT formative; the caller chooses degree and type.
func MonthAffix(m int) (string, bool) {
	if m < 1 || m > 12 {
		return "", false
	}
	return MonthAffixes[m-1], true
}

// DayOfWeekAffix returns the Cs cluster encoding day d (1-7). Returns
// ok=false for out-of-range d.
func DayOfWeekAffix(d int) (string, bool) {
	if d < 1 || d > 7 {
		return "", false
	}
	return DayOfWeekAffixes[d-1], true
}
