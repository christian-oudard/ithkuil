package numbers

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/roman"
)

func TestRootValue_DigitsAndPowers(t *testing.T) {
	cases := []struct {
		cluster string
		want    int64
	}{
		{"vr", 0}, {"ll", 1}, {"ks", 2}, {"z", 3}, {"pš", 4},
		{"st", 5}, {"cp", 6}, {"ns", 7}, {"čk", 8}, {"lẓ", 9},
		{"j", 10},
		{"cg", 11}, {"jd", 12}, {"ļj", 13}, {"bc", 14}, {"ţẓ", 15},
		{"gz", 100}, {"pc", 10000}, {"kẓ", 100000000},
		{"čg", 10_000_000_000_000_000},
	}
	for _, c := range cases {
		got, ok := RootValue(c.cluster)
		if !ok || got != c.want {
			t.Errorf("RootValue(%q) = (%d,%v), want (%d,true)",
				c.cluster, got, ok, c.want)
		}
	}
	if _, ok := RootValue("ml"); ok {
		t.Error("RootValue(ml) ok=true, want false (not a number root)")
	}
}

func TestIsNumberRoot(t *testing.T) {
	for _, c := range []string{"vr", "j", "cg", "gz", "čg"} {
		if !IsNumberRoot(c) {
			t.Errorf("IsNumberRoot(%q) = false, want true", c)
		}
	}
	for _, c := range []string{"ml", "lr", ""} {
		if IsNumberRoot(c) {
			t.Errorf("IsNumberRoot(%q) = true, want false", c)
		}
	}
}

func TestFormative_RoundTrip_0to99(t *testing.T) {
	// Build, render, parse, decode for every n in 0-99. The number
	// should survive the trip unchanged.
	for n := 0; n < 100; n++ {
		f, ok := Formative(n, Cardinal, Concrete, g.THM)
		if !ok {
			t.Fatalf("Formative(%d): ok=false", n)
		}
		// Round-trip via the rendered romanization so we exercise the
		// real render → parse pipeline.
		// Skip romanization verification for now; just call Decode directly
		// on the built Formative.
		dec, ok := Decode(f)
		if !ok {
			t.Errorf("Decode(%d) ok=false", n)
			continue
		}
		if dec.Value != int64(n) {
			t.Errorf("Decode(%d) Value = %d, want %d", n, dec.Value, n)
		}
		if dec.Stem != Cardinal || dec.Version != Concrete {
			t.Errorf("Decode(%d) (Stem,Version) = (%v,%v), want (Cardinal,Concrete)",
				n, dec.Stem, dec.Version)
		}
	}
}

func TestFormative_StemVersionMatrix(t *testing.T) {
	// Every (Stem, Version) pair should round-trip.
	for _, stem := range []Stem{Cardinal, Ordinal, Partitive, Collective} {
		for _, ver := range []Version{Concrete, Abstract} {
			f, ok := Formative(7, stem, ver, g.THM)
			if !ok {
				t.Errorf("Formative(7,%v,%v): ok=false", stem, ver)
				continue
			}
			dec, ok := Decode(f)
			if !ok {
				t.Errorf("Decode(7,%v,%v) ok=false", stem, ver)
				continue
			}
			if dec.Stem != stem || dec.Version != ver {
				t.Errorf("(stem,ver) round-trip = (%v,%v), want (%v,%v)",
					dec.Stem, dec.Version, stem, ver)
			}
		}
	}
}

func TestFormative_OutOfRange(t *testing.T) {
	for _, n := range []int{-1, 100, 1000} {
		if _, ok := Formative(n, Cardinal, Concrete, g.THM); ok {
			t.Errorf("Formative(%d) ok=true, want false", n)
		}
	}
}

func TestRender_NonEmptyForAll0to99(t *testing.T) {
	for n := 0; n < 100; n++ {
		got, ok := Render(n, Cardinal, Concrete, g.THM)
		if !ok || got == "" {
			t.Errorf("Render(%d): (%q,%v), want non-empty", n, got, ok)
		}
	}
}

func TestRender_RoundTripsThroughFullparse(t *testing.T) {
	// Build → render → fullparse → decode, on a handful of representative
	// values. This is the real end-to-end pipeline.
	cases := []int{0, 1, 5, 10, 11, 42, 99}
	for _, n := range cases {
		rom, ok := Render(n, Cardinal, Concrete, g.THM)
		if !ok {
			t.Fatalf("Render(%d): ok=false", n)
		}
		f, err := roman.ParseFormative(rom)
		if err != nil {
			t.Errorf("Parse(%q): %v", rom, err)
			continue
		}
		dec, ok := Decode(f)
		if !ok {
			t.Errorf("Decode after parse(%q): ok=false (formative %+v)", rom, f)
			continue
		}
		if dec.Value != int64(n) {
			t.Errorf("end-to-end %d: romanization %q → %d", n, rom, dec.Value)
		}
	}
}

func TestDecode_RejectsNonNumberRoot(t *testing.T) {
	f := g.MinimalFormative("ml") // "ml" isn't a number root
	if _, ok := Decode(f); ok {
		t.Error("Decode of non-number formative returned ok=true")
	}
}

func TestPowerFormative_AllPowers(t *testing.T) {
	for i := 1; i < len(PowerRoots); i++ {
		f, ok := PowerFormative(i, Cardinal, Concrete, g.THM)
		if !ok {
			t.Errorf("PowerFormative(%d): ok=false", i)
			continue
		}
		dec, ok := Decode(f)
		if !ok || dec.Value != powerValues[i] {
			t.Errorf("PowerFormative(%d): Decode = (%+v,%v), want value=%d",
				i, dec, ok, powerValues[i])
		}
	}
}

func TestPhrase_SingleFormative(t *testing.T) {
	// n < 100 returns a one-element slice.
	words, ok := Phrase(42, Cardinal, Concrete)
	if !ok {
		t.Fatal("Phrase(42): ok=false")
	}
	if len(words) != 1 {
		t.Errorf("Phrase(42): %d words, want 1: %v", len(words), words)
	}
}

func TestPhrase_FourThousandPlus(t *testing.T) {
	// 4229 = 42 × 100 + 29. The gzalui (×100) sits between two counts
	// and is omitted per Ithkuil-2011's ch. 13, so the phrase is two words.
	words, ok := Phrase(4229, Cardinal, Concrete)
	if !ok {
		t.Fatal("Phrase(4229): ok=false")
	}
	if len(words) != 2 {
		t.Errorf("Phrase(4229) words = %v (count %d), want 2", words, len(words))
	}
}

func TestPhrase_RoundMagnitude(t *testing.T) {
	// 10000 = 100 × 100 — exact multiple of 10000.
	words, ok := Phrase(10000, Cardinal, Concrete)
	if !ok {
		t.Fatal("Phrase(10000): ok=false")
	}
	// Expect a "1" count + "of 10000" magnitude word.
	if len(words) < 2 {
		t.Errorf("Phrase(10000) words = %v, want at least 2", words)
	}
}

func TestPhrase_IntermediateCountUsesCOM(t *testing.T) {
	// 269,766 = 26 × 10⁴ + 97 × 100 + 66. After gzalui-omission the
	// phrase is [26, pcalui, 97 COM, 66] — the 97 retains COM because
	// the omitted gzalui still implicitly sits between it and the 66.
	words, ok := Phrase(269_766, Cardinal, Concrete)
	if !ok {
		t.Fatal("Phrase(269766): ok=false")
	}
	if len(words) != 4 {
		t.Fatalf("Phrase(269766) got %d words, want 4: %v", len(words), words)
	}
	mid := words[2] // 97
	f, err := roman.ParseFormative(mid)
	if err != nil {
		t.Fatalf("parse %q: %v", mid, err)
	}
	un, ok := f.Final.(g.UnframedNominal)
	if !ok || un.Case != g.COM {
		t.Errorf("intermediate count Final = %+v, want UnframedNominal{COM}", f.Final)
	}
}

func TestPhrase_FirstAndLastCountUseTHM(t *testing.T) {
	// In 4229 the phrase reduces to [42, 29] after gzalui-omission.
	// Both counts are THM (first and trailing).
	words, ok := Phrase(4229, Cardinal, Concrete)
	if !ok {
		t.Fatal("Phrase(4229): ok=false")
	}
	for idx := range words {
		f, err := roman.ParseFormative(words[idx])
		if err != nil {
			t.Errorf("parse %q: %v", words[idx], err)
			continue
		}
		un, ok := f.Final.(g.UnframedNominal)
		if !ok || un.Case != g.THM {
			t.Errorf("word[%d] %q: Final = %+v, want THM", idx, words[idx], f.Final)
		}
	}
}

func TestPhrase_GzaluiOmission(t *testing.T) {
	// 4229: a single gzalui between two counts is omitted.
	words, _ := Phrase(4229, Cardinal, Concrete)
	if len(words) != 2 {
		t.Errorf("Phrase(4229) = %v, want 2 words (gzalui omitted)", words)
	}
	// 269,766: one gzalui (between 97 and 66) is omitted, but pcalui
	// (a different magnitude) is kept.
	words, _ = Phrase(269_766, Cardinal, Concrete)
	if len(words) != 4 {
		t.Errorf("Phrase(269766) = %v, want 4 words (one gzalui omitted)", words)
	}
	// 21,000,000: gzalui sits between a count and another magnitude,
	// so it must be kept (omission would change the value to 210,000).
	words, _ = Phrase(21_000_000, Cardinal, Concrete)
	if len(words) != 3 {
		t.Errorf("Phrase(21M) = %v, want 3 words (gzalui kept)", words)
	}
}

func TestPhrase_ChainsMagnitudes(t *testing.T) {
	// 21,000,000 = 21 × 100 × 10000. The expected romanization is a chain
	// of three words: 21, gz-PAR, pc-PAR.
	words, ok := Phrase(21_000_000, Cardinal, Concrete)
	if !ok {
		t.Fatal("Phrase(21M): ok=false")
	}
	if len(words) != 3 {
		t.Errorf("Phrase(21M) got %d words, want 3: %v", len(words), words)
	}
}

func TestPhrase_RejectsNegative(t *testing.T) {
	if _, ok := Phrase(-1, Cardinal, Concrete); ok {
		t.Error("Phrase(-1) ok=true, want false")
	}
}

func TestMonthAffix(t *testing.T) {
	cases := []struct {
		m    int
		want string
	}{
		{1, "lks"}, {7, "lčk"}, {12, "ljz"},
	}
	for _, c := range cases {
		got, ok := MonthAffix(c.m)
		if !ok || got != c.want {
			t.Errorf("MonthAffix(%d) = (%q,%v), want (%q,true)", c.m, got, ok, c.want)
		}
	}
	if _, ok := MonthAffix(0); ok {
		t.Error("MonthAffix(0) ok=true, want false")
	}
	if _, ok := MonthAffix(13); ok {
		t.Error("MonthAffix(13) ok=true, want false")
	}
}

func TestDayOfWeekAffix(t *testing.T) {
	cases := []struct {
		d    int
		want string
	}{
		{1, "mks"}, {4, "mst"}, {7, "mčk"},
	}
	for _, c := range cases {
		got, ok := DayOfWeekAffix(c.d)
		if !ok || got != c.want {
			t.Errorf("DayOfWeekAffix(%d) = (%q,%v), want (%q,true)", c.d, got, ok, c.want)
		}
	}
	if _, ok := DayOfWeekAffix(0); ok {
		t.Error("DayOfWeekAffix(0) ok=true, want false")
	}
	if _, ok := DayOfWeekAffix(8); ok {
		t.Error("DayOfWeekAffix(8) ok=true, want false")
	}
}

func TestPhrase_RoundTripsThroughFullparse(t *testing.T) {
	// Render a phrase, parse each word back, decode each as a number,
	// and verify the magnitude chain recovers the original value.
	//
	// Decode walks the words left-to-right. A count (<100) closes the
	// previous segment (count × accumulated_mag) and starts a new one.
	// Two adjacent counts (no magnitude between) imply an omitted
	// *gzalui* — the first count is multiplied by 100. Consecutive
	// magnitudes multiply onto the same count.
	cases := []int64{100, 200, 4229, 10000, 9999, 269_766, 1_000_000, 21_000_000}
	for _, n := range cases {
		words, ok := Phrase(n, Cardinal, Concrete)
		if !ok {
			t.Errorf("Phrase(%d): ok=false", n)
			continue
		}
		var total int64
		var count int64
		var mag int64 = 1
		prevWasCount := false
		for _, w := range words {
			f, err := roman.ParseFormative(w)
			if err != nil {
				t.Errorf("Phrase(%d) parse %q: %v", n, w, err)
				continue
			}
			num, ok := Decode(f)
			if !ok {
				t.Errorf("Phrase(%d) decode %q: not a number", n, w)
				continue
			}
			if num.Value < 100 {
				if prevWasCount {
					// Implicit ×100 between two adjacent counts.
					total += count * 100
				} else {
					total += count * mag
				}
				count = num.Value
				mag = 1
				prevWasCount = true
			} else {
				mag *= num.Value
				prevWasCount = false
			}
		}
		total += count * mag
		if total != n {
			t.Errorf("Phrase(%d) round-trip → %d (words: %v)", n, total, words)
		}
	}
}

func TestSPTFormative_RoundTrip(t *testing.T) {
	cases := []struct {
		n   int
		deg int
	}{
		{8, SPTHour},
		{6, SPTDayOfWeek},
		{15, SPTDayOfMonth},
		{3, SPTMonth},
		{99, SPTYear},
		{21, SPTCentury},
	}
	for _, c := range cases {
		f, ok := SPTFormative(c.n, c.deg, Cardinal, Abstract)
		if !ok {
			t.Errorf("SPTFormative(%d, %d): ok=false", c.n, c.deg)
			continue
		}
		got, ok := Decode(f)
		if !ok {
			t.Errorf("Decode(SPTFormative(%d, %d)) failed", c.n, c.deg)
			continue
		}
		if got.Value != int64(c.n) {
			t.Errorf("SPTFormative(%d, %d) value=%d, want %d", c.n, c.deg, got.Value, c.n)
		}
		deg, hasSPT := SPTDegree(f)
		if !hasSPT || deg != c.deg {
			t.Errorf("SPTDegree(SPTFormative(%d, %d)) = %d/%v, want %d/true",
				c.n, c.deg, deg, hasSPT, c.deg)
		}
	}
}

func TestSPTFormative_RejectsBadInputs(t *testing.T) {
	if _, ok := SPTFormative(0, 0, Cardinal, Abstract); ok {
		t.Error("SPTFormative with degree 0 should fail")
	}
	if _, ok := SPTFormative(0, 10, Cardinal, Abstract); ok {
		t.Error("SPTFormative with degree 10 should fail")
	}
	if _, ok := SPTFormative(100, SPTHour, Cardinal, Abstract); ok {
		t.Error("SPTFormative with n=100 should fail (out of 0-99 range)")
	}
}

func TestRenderSPT_ShortcutForm(t *testing.T) {
	got, ok := RenderSPT(8, SPTHour, Cardinal, Abstract)
	if !ok {
		t.Fatalf("RenderSPT(8, SPTHour): ok=false")
	}
	// "wučkerwa": w + u + čk(8) + e(T1D3) + rw(SPT) + a(THM).
	// The THM "a" cannot elide here: §2.22 lets w appear only as the
	// last member of a conjunct and only when a vowel follows it, and
	// §4.1 bars a word-final -w outright.
	want := "wučkerwa"
	if got != want {
		t.Errorf("RenderSPT(8, SPTHour) = %q, want %q", got, want)
	}
}

func TestRenderSPT_ParsesBack(t *testing.T) {
	// Every (n, degree) combination renders to a romanization that
	// parses back to the same value and SPT degree.
	for n := 0; n <= 12; n++ {
		for deg := SPTSecond; deg <= SPTCentury; deg++ {
			surf, ok := RenderSPT(n, deg, Cardinal, Abstract)
			if !ok {
				t.Errorf("RenderSPT(%d, %d) ok=false", n, deg)
				continue
			}
			parsed, err := roman.ParseFormative(surf)
			if err != nil {
				t.Errorf("RenderSPT(%d, %d) = %q parse-err: %v", n, deg, surf, err)
				continue
			}
			num, ok := Decode(parsed)
			if !ok || num.Value != int64(n) {
				t.Errorf("RenderSPT(%d, %d) = %q decodes to %d", n, deg, surf, num.Value)
			}
			d, hasSPT := SPTDegree(parsed)
			if !hasSPT || d != deg {
				t.Errorf("RenderSPT(%d, %d) = %q SPTDegree=%d/%v", n, deg, surf, d, hasSPT)
			}
		}
	}
}

func TestSPTDegreeLabel(t *testing.T) {
	cases := map[int]string{
		SPTSecond:      "second",
		SPTMinute:      "minute",
		SPTHour:        "hour",
		SPTDayOfWeek:   "weekday",
		SPTDayOfMonth:  "day",
		SPTWeekOfMonth: "week",
		SPTMonth:       "month",
		SPTYear:        "year",
		SPTCentury:     "century",
	}
	for d, want := range cases {
		if got := SPTDegreeLabel(d); got != want {
			t.Errorf("SPTDegreeLabel(%d) = %q, want %q", d, got, want)
		}
	}
	if got := SPTDegreeLabel(0); got != "" {
		t.Errorf("SPTDegreeLabel(0) = %q, want \"\"", got)
	}
	if got := SPTDegreeLabel(10); got != "" {
		t.Errorf("SPTDegreeLabel(10) = %q, want \"\"", got)
	}
}

func TestSPTDegree_NoAffix(t *testing.T) {
	f, _ := Formative(8, Cardinal, Abstract, g.THM)
	if d, ok := SPTDegree(f); ok {
		t.Errorf("SPTDegree(plain number) = %d/true, want 0/false", d)
	}
}

func TestStemAndVersion_StringerSanity(t *testing.T) {
	// Just ensure non-empty strings — defends against table drift.
	for _, s := range []Stem{Cardinal, Ordinal, Partitive, Collective} {
		if s.String() == "" {
			t.Errorf("Stem(%d).String() empty", s)
		}
	}
	for _, v := range []Version{Concrete, Abstract} {
		if v.String() == "" {
			t.Errorf("Version(%d).String() empty", v)
		}
	}
}
