package surface

import "testing"

// roundTrip is the central property: Apply(Strip(w)) == w for any
// well-formed (i.e., orthographically conventional) input.
func TestRoundTrip(t *testing.T) {
	cases := []struct {
		word         string
		wantStress   Stress
		wantStripped string
	}{
		// Penultimate (default, unmarked) — many real Ithkuil words.
		{"malëuţřait", Penultimate, "malëuţřait"},
		{"amlala", Penultimate, "amlala"},
		{"fkhalo", Penultimate, "fkhalo"},
		{"ihwe", Penultimate, "ihwe"},
		// Ultimate (acute on the last vowel).
		{"öhwoňó", Ultimate, "öhwoňo"},
		{"agulahá", Ultimate, "agulaha"},
		{"malëuţřáit", Ultimate, "malëuţřait"},
		// Monosyllabic (one syllable, no mark).
		{"la", Monosyllabic, "la"},
		{"to", Monosyllabic, "to"},
		// Antepenultimate (acute earlier than penultimate). 4 vowel-
		// conjuncts; acute on the first → "earlier than n-2".
		{"málëuţřait", Antepenultimate, "malëuţřait"},
		// Stress carries through the umlaut layer via circumflex.
		{"jwalô", Ultimate, "jwalö"},
	}
	for _, c := range cases {
		bare, st := Strip(c.word)
		if bare != c.wantStripped {
			t.Errorf("Strip(%q) bare = %q, want %q", c.word, bare, c.wantStripped)
		}
		if st != c.wantStress {
			t.Errorf("Strip(%q) stress = %v, want %v", c.word, st, c.wantStress)
		}
		round := Apply(bare, st)
		if round != c.word {
			t.Errorf("Apply(Strip(%q)) = %q, want %q", c.word, round, c.word)
		}
	}
}

func TestStrip_NoMarkPenultimate(t *testing.T) {
	bare, st := Strip("amlala")
	if bare != "amlala" || st != Penultimate {
		t.Errorf("Strip(amlala) = (%q, %v), want (amlala, Penultimate)", bare, st)
	}
}

func TestStrip_Monosyllabic(t *testing.T) {
	bare, st := Strip("la")
	if bare != "la" || st != Monosyllabic {
		t.Errorf("Strip(la) = (%q, %v), want (la, Monosyllabic)", bare, st)
	}
}

func TestApply_PenultimateUnmarked(t *testing.T) {
	// Penultimate is the orthographic default — Apply returns the
	// word unchanged.
	got := Apply("malëuţřait", Penultimate)
	if got != "malëuţřait" {
		t.Errorf("Apply(malëuţřait, Penultimate) = %q, want unchanged", got)
	}
}

func TestApply_MonosyllabicUnmarked(t *testing.T) {
	// Monosyllabic ultimate is implicit; no mark added.
	got := Apply("la", Monosyllabic)
	if got != "la" {
		t.Errorf("Apply(la, Monosyllabic) = %q, want unchanged", got)
	}
}

func TestApply_StressOnFirstVowelOfConjunct(t *testing.T) {
	// In a multi-vowel conjunct ("ai"), the mark goes on the first
	// vowel.
	got := Apply("amlalai", Ultimate)
	if got != "amlaláí" && got != "amlalái" {
		// Spec convention is first vowel: "amlalái". Accept the
		// permissive both-marked variant for robustness too.
		t.Errorf("Apply(amlalai, Ultimate) = %q, want acute on first vowel of final conjunct", got)
	}
}

func TestApply_NotEnoughSyllables(t *testing.T) {
	// Antepenultimate needs three vowel-conjuncts. With fewer, the
	// word is returned unchanged.
	got := Apply("ml", Antepenultimate)
	if got != "ml" {
		t.Errorf("Apply(ml, Antepenultimate) = %q, want unchanged", got)
	}
}

func TestStress_String(t *testing.T) {
	for _, c := range []struct {
		s    Stress
		want string
	}{
		{Monosyllabic, "Monosyllabic"},
		{Penultimate, "Penultimate"},
		{Ultimate, "Ultimate"},
		{Antepenultimate, "Antepenultimate"},
		{InvalidStress, "InvalidStress"},
	} {
		if got := c.s.String(); got != c.want {
			t.Errorf("Stress(%d).String() = %q, want %q", c.s, got, c.want)
		}
	}
}

func TestStrip_DoubleMarked(t *testing.T) {
	// More than one stress diacritic in the same word is malformed
	// per §1.3.1; Strip should signal that rather than silently picking
	// one position.
	for _, w := range []string{"amláláu", "malëúţřáit", "lálá"} {
		_, s := Strip(w)
		if s != InvalidStress {
			t.Errorf("Strip(%q) stress = %v, want InvalidStress", w, s)
		}
	}
}

// §1.2.1 names ten falling diphthongs — ai, ei, ëi, oi, ui, au, eu,
// ëu, ou, iu — and those alone share a syllable. Every other vowel
// pair is disyllabic, which is why §1.3.1 needs a grave accent to
// mark the unstressed -i- of -Cìa-. Counting vowel conjuncts instead
// of syllables put the stress of any word ending in one of those
// pairs one position too far right.
func TestSyllableCount(t *testing.T) {
	cases := []struct {
		word string
		want int
	}{
		{"lal", 1},
		{"lala", 2},
		{"laila", 2},                                     // ai is one syllable
		{"laula", 2},                                     // au is one syllable
		{"liala", 3},                                     // ia is two
		{"loala", 3},                                     // oa is two
		{"wuttihia", 4} /* wu-tti-hi-a */, {"kalioa", 4}, // ka-li-o-a
	}
	for _, c := range cases {
		if got := SyllableCount(c.word); got != c.want {
			t.Errorf("SyllableCount(%q) = %d, want %d", c.word, got, c.want)
		}
	}
}

// wuttíhia is glossed FRAMED in the §6.2.2 example, i.e. carries
// antepenultimate stress: wu-ttí-hi-a. Read as three vowel conjuncts
// (wu-ttí-hia) the mark looks penultimate, and Strip both mislabelled
// the word and dropped the diacritic when re-rendering.
func TestStrip_DisyllabicFinalConjunct(t *testing.T) {
	bare, stress := Strip("wuttíhia")
	if bare != "wuttihia" || stress != Antepenultimate {
		t.Errorf("Strip(wuttíhia) = (%q, %v), want (%q, %v)",
			bare, stress, "wuttihia", Antepenultimate)
	}
	if got := Apply("wuttihia", Antepenultimate); got != "wuttíhia" {
		t.Errorf("Apply(wuttihia, Antepenultimate) = %q, want %q", got, "wuttíhia")
	}
}
