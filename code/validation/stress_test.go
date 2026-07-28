package validation

import (
	"testing"

	"github.com/christian-oudard/ithkuil/parse"
)

func TestValidateStress_Monosyllabic(t *testing.T) {
	s, err := ValidateStress("la")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if s != parse.Monosyllabic {
		t.Errorf("got %v, want Monosyllabic", s)
	}
}

func TestValidateStress_Penultimate(t *testing.T) {
	s, err := ValidateStress("malëuţřait")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if s != parse.Penultimate {
		t.Errorf("got %v, want Penultimate", s)
	}
}

func TestValidateStress_Ultimate(t *testing.T) {
	s, err := ValidateStress("agulahá")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if s != parse.Ultimate {
		t.Errorf("got %v, want Ultimate", s)
	}
}

func TestValidateStress_Antepenultimate(t *testing.T) {
	// 3-syllable word with stress on the first syllable: fromEnd = 2,
	// which is the antepenultimate.
	s, err := ValidateStress("águla")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if s != parse.Antepenultimate {
		t.Errorf("got %v, want Antepenultimate", s)
	}
}

func TestValidateStress_DoubleMarked(t *testing.T) {
	_, err := ValidateStress("águlahá")
	if err != DoubleMarkedStress {
		t.Errorf("got %v, want DoubleMarkedStress", err)
	}
}

func TestValidateStress_MarkedDefault_Monosyllabic(t *testing.T) {
	_, err := ValidateStress("lá")
	if err != MarkedDefaultStress {
		t.Errorf("got %v, want MarkedDefaultStress", err)
	}
}

func TestValidateStress_MarkedDefault_Penult(t *testing.T) {
	// Accent on penultimate of a 3-syllable word is the default
	// position. "agúla" = a-g-ú-l-a, syllables [a, ú, a], stress at
	// index 1 of 3 → fromEnd 1 → penult = MarkedDefault.
	_, err := ValidateStress("agúla")
	if err != MarkedDefaultStress {
		t.Errorf("got %v, want MarkedDefaultStress", err)
	}
}

func TestStressErrorString(t *testing.T) {
	if DoubleMarkedStress.Error() != "DoubleMarkedStress" {
		t.Errorf("got %q", DoubleMarkedStress.Error())
	}
}
