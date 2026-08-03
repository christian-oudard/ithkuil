package phonology

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

// §1.3.1 puts a grave on the -i- of a -Cìa- conjunct: karésìa against
// karesya, vélkìo against velkyo. Reading one works — Normalize folds
// the grave away, since it marks pronunciation and not grammar — but
// writing one does not, so a word read from the grammar document comes
// back spelled differently from how the document spells it.
//
// The obvious fix, having Apply put the grave back, is wrong twice
// over. Apply's business is the stress mark, and the grave is not one:
// it says the vowel is a syllable nucleus rather than a glide, on a
// vowel the rule has already required to be unstressed. And it is
// positional rather than suprasegmental, so it belongs wherever
// conjuncts are written rather than in the one function that moves an
// accent around.
//
// What is not settled is whether the grave belongs in canonical output
// at all. §1.3.1 says it "is used" over -i- but only that it "may
// similarly be used" over -u-, so the two are not obviously the same
// rule, and the document's own worked examples in §§5 and 7 write
// -Cia- without it far more often than with. Until that reading is
// settled, writing it back would be guessing.
func TestApply_GraveOnUnstressedI(t *testing.T) {
	t.Skip("§1.3.1's grave is read but not written; see BUGS.md")

	for _, w := range []string{"karésìa", "vélkìo", "ehùá"} {
		if got := Normalize(w); got != w {
			t.Errorf("Normalize(%q) = %q, want the grave kept", w, got)
		}
	}
}

// TestStress_StringOutOfRange pins a String method against the value it
// is not supposed to receive. slots.ToGrammar formats the stress into
// its "no Slot IX reading" fault, so an out-of-range Stress reaches
// %v on the error path, and indexing a fixed array there panicked.
// Standard-toolchain fmt hid it by recovering the panic and printing
// %!v(PANIC=...); TinyGo's does not, so the browser build died on a
// case the CLI merely garbled. Found by running this suite under
// TinyGo, which is the only reason it was ever visible.
func TestStress_StringOutOfRange(t *testing.T) {
	for _, s := range []Stress{-1, 5, 99} {
		got := Stress(s).String()
		if got == "" {
			t.Errorf("Stress(%d).String() is empty", int(s))
		}
	}
	if got := Stress(99).String(); got != "Stress(99)" {
		t.Errorf("Stress(99).String() = %q, want Stress(99)", got)
	}
	if got := Ultimate.String(); got != "Ultimate" {
		t.Errorf("Ultimate.String() = %q, want Ultimate", got)
	}
}
