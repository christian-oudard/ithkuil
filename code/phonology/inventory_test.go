package phonology

import "testing"

func TestInventorySizes(t *testing.T) {
	if len(Consonants) != 31 {
		t.Errorf("want 31 consonants, got %d", len(Consonants))
	}
	if len(Vowels) != 9 {
		t.Errorf("want 9 vowels, got %d", len(Vowels))
	}
}

func TestVowelFormTableShape(t *testing.T) {
	if len(VowelFormTable) != 4 {
		t.Fatalf("want 4 series, got %d", len(VowelFormTable))
	}
	for s, row := range VowelFormTable {
		if len(row) != 9 {
			t.Errorf("series %d: want 9 forms, got %d", s+1, len(row))
		}
	}
}

func TestVowelForm(t *testing.T) {
	cases := []struct {
		series, form int
		want         string
	}{
		{1, 1, "a"},
		{1, 5, "ëi"},
		{2, 1, "ai"},
		{3, 1, "ia"},
		{4, 9, "oa"},
	}
	for _, c := range cases {
		if got := VowelForm(c.series, c.form); got != c.want {
			t.Errorf("VowelForm(%d,%d) = %q, want %q", c.series, c.form, got, c.want)
		}
	}
}

func TestVowelFormLookup(t *testing.T) {
	cases := []struct {
		in   string
		s, f int
		ok   bool
	}{
		{"a", 1, 1, true},
		{"ëi", 1, 5, true},
		{"ai", 2, 1, true},
		{"ia", 3, 1, true},
		{"uä", 3, 1, true}, // series-3 alternate
		{"iä", 3, 9, true}, // series-3 alternate
		{"xyz", 0, 0, false},
	}
	for _, c := range cases {
		s, f, ok := VowelFormLookup(c.in)
		if ok != c.ok || s != c.s || f != c.f {
			t.Errorf("VowelFormLookup(%q) = (%d,%d,%v), want (%d,%d,%v)",
				c.in, s, f, ok, c.s, c.f, c.ok)
		}
	}
}

// TestVowelFormTableSpellings pins which cells carry two spellings.
// Series 3 is the only one the source prints as "x / y", and its form 5
// (eë) is the one cell in that row with a single spelling.
func TestVowelFormTableSpellings(t *testing.T) {
	for s, row := range VowelFormTable {
		for f, cell := range row {
			if cell[0] == "" {
				t.Errorf("series %d form %d: no primary spelling", s+1, f+1)
			}
			want := s+1 == 3 && f+1 != 5
			if got := cell[1] != ""; got != want {
				t.Errorf("series %d form %d: alternate=%v (%q), want %v",
					s+1, f+1, got, cell[1], want)
			}
		}
	}
}

// §1.6's footnote: a Series-3 form beginning with -i- dissimilates after
// y-, and one beginning with -u- after w-. Nothing else moves, and
// neither glide touches a form whose initial does not match it.
func TestVowelFormAfterGlide(t *testing.T) {
	cases := []struct {
		prev rune
		in   string
		want string
	}{
		{'y', "ia", "uä"}, // the footnote's own example
		{'w', "ua", "iä"}, // the footnote's other example
		{'y', "iö", "üë"},
		{'w', "uö", "öë"},
		{'y', "ua", "ua"},   // u-initial after y: nothing to dissimilate
		{'w', "ia", "ia"},   // i-initial after w: likewise
		{'y', "eë", "eë"},   // series 3 form 5 has no alternate
		{'y', "uä", "uä"},   // already dissimilated: idempotent
		{'l', "ia", "ia"},   // not a glide
		{'y', "a", "a"},     // series 1
		{'y', "ai", "ai"},   // series 2
		{'y', "ao", "ao"},   // series 4
		{'y', "zzz", "zzz"}, // not a vowel-form at all
	}
	for _, c := range cases {
		if got := VowelFormAfterGlide(c.prev, c.in); got != c.want {
			t.Errorf("VowelFormAfterGlide(%q, %q) = %q, want %q",
				c.prev, c.in, got, c.want)
		}
	}
}
