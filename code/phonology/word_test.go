package phonology

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/fault"
)

func TestParseWord_KeepsWhatItRead(t *testing.T) {
	// The reading is the point: a Word carries the normalized text, the
	// stress the mark put on it, and the conjunct split, so no later
	// layer has to derive any of it again.
	w, err := ParseWord("Maţřëullait")
	if err != nil {
		t.Fatalf("ParseWord: %v", err)
	}
	// Capitals are orthographic (sentence-initial, proper nouns), and
	// the canonical name of the language is conventionally written with
	// one; the reading lowercases it.
	if w.String() != "maţřëullait" {
		t.Errorf("String() = %q, want the normalized word", w.String())
	}
	if w.Bare() != "maţřëullait" {
		t.Errorf("Bare() = %q, want the word with no stress mark", w.Bare())
	}
	if w.Stress() != Penultimate {
		t.Errorf("Stress() = %v, want Penultimate (unmarked default)", w.Stress())
	}
	if got := strings.Join(w.Conjuncts(), "-"); got != "m-a-ţř-ëu-ll-ai-t" {
		t.Errorf("Conjuncts() = %q, want the vowel/consonant runs", got)
	}
}

func TestParseWord_ConjunctsAreACopy(t *testing.T) {
	// A Word is evidence that outlives the call. Handing out the
	// backing array would let a caller edit the reading under everyone
	// else holding the same Word.
	w, err := ParseWord("malëuţřait")
	if err != nil {
		t.Fatalf("ParseWord: %v", err)
	}
	w.Conjuncts()[0] = "zzz"
	if w.Conjuncts()[0] == "zzz" {
		t.Error("Conjuncts() exposed the Word's own slice")
	}
}

func TestParseWord_NonIthkuilCharacters(t *testing.T) {
	// 'ø' (U+00F8) is not in the V4 alphabet — Norwegian/Danish o-slash.
	// Nothing else can be trusted once a rune is unreadable, so this
	// violation is reported on its own.
	_, err := ParseWord("møl")
	if err == nil {
		t.Fatal("expected a chars violation for 'møl'")
	}
	ill, ok := err.(fault.Faults)
	if !ok {
		t.Fatalf("error %v is not fault.Faults", err)
	}
	if len(ill.List) != 1 {
		t.Fatalf("violations = %v, want the chars one alone", ill.List)
	}
	for _, want := range []string{`'ø'`, `(U+00F8)`} {
		if !strings.Contains(ill.List[0].Fix, want) {
			t.Errorf("reason %q missing %q", ill.List[0].Fix, want)
		}
	}
}

func TestParseWord_ReadsWhatTheRulesReject(t *testing.T) {
	// "akx" breaks §2.3, but it is still readable: every rune is in the
	// alphabet and the conjuncts split. Reading it and judging it are
	// separate, so ParseWord succeeds and the judgment comes from
	// Violations.
	w, err := ParseWord("akx")
	if err != nil {
		t.Fatalf("ParseWord(akx) = %v, want a reading", err)
	}
	vs := w.Violations()
	if len(vs) == 0 {
		t.Fatal("akx should break a cluster rule")
	}
	if vs[0].Code != "2.3" {
		t.Errorf("rule = %q, want 2.3", vs[0].Code)
	}
}

func TestParseWord_RejectsAChain(t *testing.T) {
	// Each link of a §3.1.7 chain carries its own stress and its own
	// word-initial and word-final positions, so a chain is not one word.
	if _, err := ParseWord("hakšal-uḑfarf"); err == nil {
		t.Error("a hyphenated chain is not one word")
	}
	if _, err := ParseChain("hakšal-uḑfarf"); err != nil {
		t.Errorf("ParseChain: %v", err)
	}
}

func TestParseChain_LinkPerWord(t *testing.T) {
	words, err := ParseChain("hakšal-uḑfarf")
	if err != nil {
		t.Fatalf("ParseChain: %v", err)
	}
	if len(words) != 2 {
		t.Fatalf("links = %d, want 2", len(words))
	}
	if words[0].String() != "hakšal" || words[1].String() != "uḑfarf" {
		t.Errorf("links = %q, %q", words[0], words[1])
	}
}

func TestParseWord_Empty(t *testing.T) {
	// There is no word to read, so there is no Word to hand back.
	if _, err := ParseWord(""); err == nil {
		t.Error("empty text should not read as a word")
	}
}

func TestViolations_Clean(t *testing.T) {
	for _, w := range []string{"malëuţřait", "amlalú", "ámlala", "ah", "řřx"} {
		if err := CheckText(w); err != nil {
			t.Errorf("CheckText(%q) = %v, want clean", w, err)
		}
	}
}

func TestViolations_StressedDiphthong(t *testing.T) {
	// A stress mark sits on the first vowel of a diphthong (§1.3.1), so
	// ultimate stress on a final diphthong gives "áu", "ói" and friends.
	// The phonotactic tables list bare vowels, so the mark has to come
	// off before the conjunct is looked up.
	for _, w := range []string{"walţmáu", "attaláu", "avļarüřjiatói", "amlaléi"} {
		if err := CheckText(w); err != nil {
			t.Errorf("CheckText(%q) = %v, want clean", w, err)
		}
	}
}

func TestViolations_MarkedDefaultStress(t *testing.T) {
	// "lá" is monosyllabic, where the stress is unmarked by convention.
	if err := CheckText("lá"); err == nil {
		t.Error("expected a stress violation")
	}
}

func TestViolations_BadVowelSequence(t *testing.T) {
	// "aa" is not a permissible diphthong.
	if err := CheckText("aa"); err == nil {
		t.Error("expected a vowel-sequence violation")
	}
}

func TestCheckText_ReportsEveryLinkOfAChain(t *testing.T) {
	// Checking the joined string instead would read two stress marks as
	// one over-marked word and mistake both interior edges for medial
	// clusters.
	if err := CheckText("akx-akx"); err == nil {
		t.Fatal("expected violations from both links")
	} else if ill, ok := err.(fault.Faults); !ok || len(ill.List) != 2 {
		t.Errorf("violations = %v, want one per link", err)
	}
}

func TestLegal(t *testing.T) {
	if !Legal("malëuţřait") {
		t.Error("Legal(malëuţřait) = false")
	}
	if Legal("akx") {
		t.Error("Legal(akx) = true, want false: it breaks §2.3")
	}
	if Legal("møl") {
		t.Error("Legal(møl) = true, want false: it is not even readable")
	}
}
