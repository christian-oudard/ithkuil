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
