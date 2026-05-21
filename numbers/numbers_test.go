package numbers

import (
	"strings"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/fullparse"
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
		// Round-trip via the rendered surface form so we exercise the
		// real render → parse pipeline.
		// Skip surface verification for now; just call Decode directly
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
		surface, ok := Render(n, Cardinal, Concrete, g.THM)
		if !ok {
			t.Fatalf("Render(%d): ok=false", n)
		}
		f, err := fullparse.ParseFormative(surface)
		if err != nil {
			t.Errorf("Parse(%q): %v", surface, err)
			continue
		}
		dec, ok := Decode(f)
		if !ok {
			t.Errorf("Decode after parse(%q): ok=false (formative %+v)", surface, f)
			continue
		}
		if dec.Value != int64(n) {
			t.Errorf("end-to-end %d: surface %q → %d", n, surface, dec.Value)
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
	// 4229 = 42 × 100 + 29 — should produce [count, mag-PAR, ones].
	words, ok := Phrase(4229, Cardinal, Concrete)
	if !ok {
		t.Fatal("Phrase(4229): ok=false")
	}
	if len(words) != 3 {
		t.Errorf("Phrase(4229) words = %v (count %d), want 3", words, len(words))
	}
	// The middle word should be a PAR-cased rendering of gz=100.
	if !strings.Contains(words[1], "gz") {
		t.Errorf("Phrase(4229)[1] = %q, expected gz magnitude word", words[1])
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

func TestPhrase_ChainsMagnitudes(t *testing.T) {
	// 21,000,000 = 21 × 100 × 10000. The expected surface is a chain
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
	// Decode walks the words left-to-right. Counts (<100) start a new
	// segment; consecutive magnitudes (≥100) multiply together; when
	// the next count appears, the previous (count × accumulated_mag)
	// is added to the total.
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
		for _, w := range words {
			f, err := fullparse.ParseFormative(w)
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
				total += count * mag
				count = num.Value
				mag = 1
			} else {
				mag *= num.Value
			}
		}
		total += count * mag
		if total != n {
			t.Errorf("Phrase(%d) round-trip → %d (words: %v)", n, total, words)
		}
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
