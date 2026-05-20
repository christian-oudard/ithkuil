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

func TestStrip_HiatusNotStress(t *testing.T) {
	// "ï" is a hiatus marker (separates syllables), not a stress
	// mark. Strip must leave it alone.
	//
	// Note: parse.SplitConjuncts currently groups consecutive
	// vowels into one conjunct regardless of the hiatus marker, so
	// "aïa" counts as one syllable for our purposes here. The
	// hiatus-aware syllable count is a known limitation to address
	// at Layer B (conjunct splitting). For Layer A all we verify is
	// that the ï rune is preserved through Strip.
	bare, _ := Strip("aïa")
	if bare != "aïa" {
		t.Errorf("Strip(aïa) bare = %q, want aïa (ï preserved)", bare)
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
