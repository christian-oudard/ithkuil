package validation

import (
	"strings"
	"testing"
)

func TestValidateWord_Valid(t *testing.T) {
	// "malëuţřait" — the canonical Ithkuil V4 word, must be valid.
	r := ValidateWord("malëuţřait")
	if !r.Valid {
		t.Errorf("malëuţřait should validate, got %v", r.Errors)
	}
}

func TestValidateWord_CapitalizedIsValid(t *testing.T) {
	// Capital letters are orthographic (sentence-initial, proper
	// nouns); the canonical name of the language is conventionally
	// written "Maţřëullait" and must validate.
	r := ValidateWord("Maţřëullait")
	if !r.Valid {
		t.Errorf("Maţřëullait should validate, got %v", r.Errors)
	}
}

func TestValidateWord_BadStress(t *testing.T) {
	// "lá" — single-syllable with explicit stress = MarkedDefaultStress.
	r := ValidateWord("lá")
	if r.Valid {
		t.Error("expected stress error, got Valid")
	}
}

func TestValidateWord_BadCluster(t *testing.T) {
	// "akx" has a velar stop + uvular fricative violation (2.3).
	r := ValidateWord("akx")
	if r.Valid {
		t.Error("expected cluster error, got Valid")
	}
}

func TestValidateWord_BadVowelSequence(t *testing.T) {
	// "aa" is not a permissible diphthong.
	r := ValidateWord("aa")
	if r.Valid {
		t.Error("expected vowel-sequence error, got Valid")
	}
}

func TestValidateWord_Empty(t *testing.T) {
	if r := ValidateWord(""); !r.Valid {
		t.Errorf("empty word should be valid; got %v", r.Errors)
	}
}

func TestValidateChars_NonIthkuil(t *testing.T) {
	// 'ø' (U+00F8) is not in the V4 alphabet — Norwegian/Danish o-slash.
	r := ValidateChars("møl")
	if r.Valid {
		t.Fatal("expected non-Ithkuil error for 'møl'")
	}
	got := r.Errors[0].Reason
	for _, want := range []string{`'ø'`, `(U+00F8)`} {
		if !strings.Contains(got, want) {
			t.Errorf("error reason %q missing %q", got, want)
		}
	}
}

func TestValidateChars_AllIthkuil(t *testing.T) {
	for _, w := range []string{"malëuţřait", "amlalú", "ámlala", "ah", "řřx"} {
		if r := ValidateChars(w); !r.Valid {
			t.Errorf("ValidateChars(%q) flagged %v, want clean", w, r.Errors)
		}
	}
}
