package validation

import "testing"

func TestValidateWord_Valid(t *testing.T) {
	// "malëuţřait" — the canonical Ithkuil V4 word, must be valid.
	r := ValidateWord("malëuţřait")
	if !r.Valid {
		t.Errorf("malëuţřait should validate, got %v", r.Errors)
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
