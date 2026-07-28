package parse

import (
	"testing"

	"github.com/christian-oudard/ithkuil/grammar"
)

// The V_N tables below are transcribed from the "V_N values" table in
// morphology.md (Slot VIII, Pattern 1) and the "ASPECT V_N values" table
// (Pattern 2), row order preserved. Read them against those tables, not
// against parse/slot_viii.go: a table test whose expectations come from
// the table under test asserts only that the code equals itself.
//
// Every category is listed in full. The four Pattern-1 series occupy
// disjoint vowel forms, and Pattern 2 reuses all four series at once, so
// a single mistyped vowel shows up as a gap in one table and a stranger
// in another. TestVnSeriesDisjoint and TestVnAspectSpansEverySeries below
// check those two structural facts directly.

type vnRow[T comparable] struct {
	vowel string
	want  T
}

func checkVn[T comparable](t *testing.T, name string, parse func(string) (T, bool), rows []vnRow[T]) {
	t.Helper()
	for _, r := range rows {
		got, ok := parse(r.vowel)
		if !ok || got != r.want {
			t.Errorf("%s(%q) = (%v,%v), want (%v,true)",
				name, r.vowel, got, ok, r.want)
		}
	}
}

// Series 1. MNO is written (a) in the source: parenthesized because it is
// the default and elides, not because the vowel is anything else.
var valenceTable = []vnRow[grammar.Valence]{
	{"a", grammar.MNO}, {"ä", grammar.PRL}, {"e", grammar.CRO},
	{"i", grammar.RCP}, {"ëi", grammar.CPL}, {"ö", grammar.DUP},
	{"o", grammar.DEM}, {"ü", grammar.CNG}, {"u", grammar.PTI},
}

// Series 2.
var phaseTable = []vnRow[grammar.Phase]{
	{"ai", grammar.PCT}, {"au", grammar.ITR}, {"ei", grammar.REP},
	{"eu", grammar.ITM}, {"ëu", grammar.RCT}, {"ou", grammar.FRE},
	{"oi", grammar.FRG}, {"iu", grammar.VAC}, {"ui", grammar.FLC},
}

// Series 3, canonical then the alternate taken after a y- or w- glide
// (§1.4 footnote). Form 5 eë has no alternate.
var effectTable = []vnRow[grammar.Effect]{
	{"ia", grammar.BEN1}, {"ie", grammar.BEN2}, {"io", grammar.BEN3},
	{"iö", grammar.BSLF}, {"eë", grammar.UNK}, {"uö", grammar.DSLF},
	{"uo", grammar.DET3}, {"ue", grammar.DET2}, {"ua", grammar.DET1},

	{"uä", grammar.BEN1}, {"uë", grammar.BEN2}, {"üä", grammar.BEN3},
	{"üë", grammar.BSLF}, {"öë", grammar.DSLF}, {"öä", grammar.DET3},
	{"ië", grammar.DET2}, {"iä", grammar.DET1},
}

// Series 4.
var levelTable = []vnRow[grammar.Level]{
	{"ao", grammar.MIN}, {"aö", grammar.SBE}, {"eo", grammar.IFR},
	{"eö", grammar.DFT}, {"oë", grammar.EQU}, {"öe", grammar.SUR},
	{"oe", grammar.SPL}, {"öa", grammar.SPQ}, {"oa", grammar.MAX},
}

// Aspect fills all four series. Column 3 carries the same glide
// alternates as the Effect column.
var aspectTable = []vnRow[grammar.Aspect]{
	{"a", grammar.RTR}, {"ä", grammar.PRS}, {"e", grammar.HAB},
	{"i", grammar.PRG}, {"ëi", grammar.IMM}, {"ö", grammar.PCS},
	{"o", grammar.REG}, {"ü", grammar.SMM}, {"u", grammar.ATP},

	{"ai", grammar.RSM}, {"au", grammar.CSS}, {"ei", grammar.PAU},
	{"eu", grammar.RGR}, {"ëu", grammar.PCL}, {"ou", grammar.CNT},
	{"oi", grammar.ICS}, {"iu", grammar.EXP}, {"ui", grammar.IRP},

	{"ia", grammar.PMP}, {"ie", grammar.CLM}, {"io", grammar.DLT},
	{"iö", grammar.TMP}, {"eë", grammar.XPD}, {"uö", grammar.LIM},
	{"uo", grammar.EPD}, {"ue", grammar.PTC}, {"ua", grammar.PPR},

	{"uä", grammar.PMP}, {"uë", grammar.CLM}, {"üä", grammar.DLT},
	{"üë", grammar.TMP}, {"öë", grammar.LIM}, {"öä", grammar.EPD},
	{"ië", grammar.PTC}, {"iä", grammar.PPR},

	{"ao", grammar.DCL}, {"aö", grammar.CCL}, {"eo", grammar.CUL},
	{"eö", grammar.IMD}, {"oë", grammar.TRD}, {"öe", grammar.TNS},
	{"oe", grammar.ITC}, {"öa", grammar.MTV}, {"oa", grammar.SQN},
}

func TestParseVnValence(t *testing.T) {
	checkVn(t, "ParseVnValence", ParseVnValence, valenceTable)
}

func TestParseVnPhase(t *testing.T) {
	checkVn(t, "ParseVnPhase", ParseVnPhase, phaseTable)
}

func TestParseVnEffect(t *testing.T) {
	checkVn(t, "ParseVnEffect", ParseVnEffect, effectTable)
}

func TestParseVnLevel(t *testing.T) {
	checkVn(t, "ParseVnLevel", ParseVnLevel, levelTable)
}

func TestParseVnAspect(t *testing.T) {
	checkVn(t, "ParseVnAspect", ParseVnAspect, aspectTable)
}

// Each Pattern-1 category owns one vowel series, so no vowel names two of
// them. ParseVnPattern1 leans on this: it probes Valence, then Phase, then
// Effect, then Level, and returns the first hit. An overlap would not
// fail loudly, it would let declaration order silently pick a winner.
func TestVnSeriesDisjoint(t *testing.T) {
	owner := map[string]string{}
	claim := func(series string, vowels []string) {
		for _, v := range vowels {
			if prev, dup := owner[v]; dup {
				t.Errorf("vowel %q is in both %s and %s; ParseVnPattern1 "+
					"resolves it by probe order rather than by the table",
					v, prev, series)
			}
			owner[v] = series
		}
	}
	claim("Valence", vowelsOf(valenceTable))
	claim("Phase", vowelsOf(phaseTable))
	claim("Effect", vowelsOf(effectTable))
	claim("Level", vowelsOf(levelTable))
}

// Pattern 2 spends all four series on Aspect alone, so its vowel set is
// exactly the union of the Pattern-1 categories. Anything present in one
// and missing from the other is a typo in whichever table is the odd one
// out, which is the failure a per-row test cannot see.
func TestVnAspectSpansEverySeries(t *testing.T) {
	pattern1 := map[string]bool{}
	for _, vs := range [][]string{
		vowelsOf(valenceTable), vowelsOf(phaseTable),
		vowelsOf(effectTable), vowelsOf(levelTable),
	} {
		for _, v := range vs {
			pattern1[v] = true
		}
	}
	aspect := map[string]bool{}
	for _, v := range vowelsOf(aspectTable) {
		aspect[v] = true
	}
	for v := range pattern1 {
		if !aspect[v] {
			t.Errorf("vowel %q is a Pattern-1 V_N but no Aspect", v)
		}
	}
	for v := range aspect {
		if !pattern1[v] {
			t.Errorf("vowel %q is an Aspect but no Pattern-1 V_N", v)
		}
	}
}

func vowelsOf[T comparable](rows []vnRow[T]) []string {
	vs := make([]string, len(rows))
	for i, r := range rows {
		vs[i] = r.vowel
	}
	return vs
}

// A vowel outside the tables is not a V_N of any category.
func TestParseVnRejectsUnknownVowels(t *testing.T) {
	for _, v := range []string{"", "x", "aa", "i'a", "ëo"} {
		if c, ok := ParseVnValence(v); ok {
			t.Errorf("ParseVnValence(%q) = %v, want failure", v, c)
		}
		if c, ok := ParseVnPhase(v); ok {
			t.Errorf("ParseVnPhase(%q) = %v, want failure", v, c)
		}
		if c, ok := ParseVnEffect(v); ok {
			t.Errorf("ParseVnEffect(%q) = %v, want failure", v, c)
		}
		if c, ok := ParseVnLevel(v); ok {
			t.Errorf("ParseVnLevel(%q) = %v, want failure", v, c)
		}
		if c, ok := ParseVnAspect(v); ok {
			t.Errorf("ParseVnAspect(%q) = %v, want failure", v, c)
		}
	}
}

// C_N values are §3.8.1's table: Pattern 1 in one column, Pattern 2 in the
// next, one Mood and one Case-Scope sharing each form.
func TestParseCnMood(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.Mood
	}{
		{"h", grammar.FAC}, {"hl", grammar.SUB}, {"hr", grammar.ASM},
		{"hm", grammar.SPC}, {"hn", grammar.COU}, {"hň", grammar.HYP},
	}
	for _, c := range cases {
		got, ok := ParseCnMood(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseCnMood(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestParseCnMoodP2(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.Mood
	}{
		{"w", grammar.FAC}, {"y", grammar.FAC},
		{"hw", grammar.SUB}, {"hrw", grammar.ASM},
		{"hmw", grammar.SPC}, {"hnw", grammar.COU}, {"hňw", grammar.HYP},
	}
	for _, c := range cases {
		got, ok := ParseCnMoodP2(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseCnMoodP2(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestParseCnCaseScope(t *testing.T) {
	cases := []struct {
		in   string
		want grammar.CaseScope
	}{
		{"h", grammar.CCN}, {"w", grammar.CCN}, {"y", grammar.CCN},
		{"hl", grammar.CCA}, {"hw", grammar.CCA},
		{"hr", grammar.CCS}, {"hrw", grammar.CCS},
		{"hm", grammar.CCQ}, {"hmw", grammar.CCQ},
		{"hn", grammar.CCP}, {"hnw", grammar.CCP},
		{"hň", grammar.CCV}, {"hňw", grammar.CCV},
	}
	for _, c := range cases {
		got, ok := ParseCnCaseScope(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseCnCaseScope(%q) = (%v,%v), want (%v,true)",
				c.in, got, ok, c.want)
		}
	}
}

func TestIsValidCn(t *testing.T) {
	for _, c := range []string{"h", "hl", "hr", "hm", "hn", "hň",
		"w", "y", "hw", "hrw", "hmw", "hnw", "hňw"} {
		if !IsValidCn(c) {
			t.Errorf("IsValidCn(%q) = false, want true", c)
		}
	}
	for _, c := range []string{"", "x", "hp", "p", "n"} {
		if IsValidCn(c) {
			t.Errorf("IsValidCn(%q) = true, want false", c)
		}
	}
}

func TestIsPattern2Cn(t *testing.T) {
	p2 := []string{"w", "y", "hw", "hrw", "hmw", "hnw", "hňw"}
	p1 := []string{"h", "hl", "hr", "hm", "hn", "hň"}
	for _, c := range p2 {
		if !IsPattern2Cn(c) {
			t.Errorf("IsPattern2Cn(%q) = false, want true", c)
		}
	}
	for _, c := range p1 {
		if IsPattern2Cn(c) {
			t.Errorf("IsPattern2Cn(%q) = true, want false (it's Pattern 1)", c)
		}
	}
}

func TestSlotVIIIEnumCounts(t *testing.T) {
	if n := len(grammar.AllValences); n != 9 {
		t.Errorf("AllValences = %d, want 9", n)
	}
	if n := len(grammar.AllPhases); n != 9 {
		t.Errorf("AllPhases = %d, want 9", n)
	}
	if n := len(grammar.AllEffects); n != 9 {
		t.Errorf("AllEffects = %d, want 9", n)
	}
	if n := len(grammar.AllLevels); n != 9 {
		t.Errorf("AllLevels = %d, want 9", n)
	}
	if n := len(grammar.AllAspects); n != 36 {
		t.Errorf("AllAspects = %d, want 36", n)
	}
	if n := len(grammar.AllMoods); n != 6 {
		t.Errorf("AllMoods = %d, want 6", n)
	}
	if n := len(grammar.AllCaseScopes); n != 6 {
		t.Errorf("AllCaseScopes = %d, want 6", n)
	}
}

func TestMoodCaseScopeRoundTrip(t *testing.T) {
	for _, m := range grammar.AllMoods {
		if got := grammar.CaseScopeToMood(grammar.MoodToCaseScope(m)); got != m {
			t.Errorf("Mood %v → CaseScope → Mood = %v", m, got)
		}
	}
	for _, c := range grammar.AllCaseScopes {
		if got := grammar.MoodToCaseScope(grammar.CaseScopeToMood(c)); got != c {
			t.Errorf("CaseScope %v → Mood → CaseScope = %v", c, got)
		}
	}
}
