package validation

import "testing"

func TestCheckProhibitedPair_Rule21(t *testing.T) {
	// Consonant + glottal stop.
	if rule, _ := CheckProhibitedPair('t', '\''); rule != "2.1" {
		t.Errorf("expected rule 2.1 for t', got %q", rule)
	}
}

func TestCheckProhibitedPair_Rule22(t *testing.T) {
	// Dental stop + sibilant.
	for _, c2 := range []rune{'s', 'z', 'š', 'ž', 'c', 'č', 'ẓ', 'j'} {
		if rule, _ := CheckProhibitedPair('t', c2); rule != "2.2" {
			t.Errorf("expected 2.2 for t+%s, got %q", string(c2), rule)
		}
	}
	// Dental stop + interdental.
	if rule, _ := CheckProhibitedPair('t', 'ţ'); rule != "2.2" {
		t.Errorf("expected 2.2 for t+ţ, got %q", rule)
	}
}

func TestCheckProhibitedPair_Rule23(t *testing.T) {
	if rule, _ := CheckProhibitedPair('k', 'x'); rule != "2.3" {
		t.Errorf("expected 2.3 for kx, got %q", rule)
	}
	if rule, _ := CheckProhibitedPair('g', 'ň'); rule != "2.3" {
		t.Errorf("expected 2.3 for gň, got %q", rule)
	}
}

func TestCheckProhibitedPair_Rule24(t *testing.T) {
	// Same-place voiceless+voiced: t+b is not homologous (place 2 vs 1).
	// p+d also not homologous. k+b not homologous.
	// Homologous voicing mismatch: t and d are place 2 — but same voicing
	// pair, so t+d is voicing mismatch. p+b same place + voicing mismatch.
	if rule, _ := CheckProhibitedPair('p', 'b'); rule != "2.4" {
		t.Errorf("expected 2.4 for pb, got %q", rule)
	}
	if rule, _ := CheckProhibitedPair('t', 'd'); rule != "2.4" {
		t.Errorf("expected 2.4 for td, got %q", rule)
	}
}

func TestCheckProhibitedPair_Rule28(t *testing.T) {
	// Distinct sibilant fricatives.
	if rule, _ := CheckProhibitedPair('s', 'š'); rule != "2.8" {
		t.Errorf("expected 2.8 for sš, got %q", rule)
	}
}

func TestCheckProhibitedPair_Allowed(t *testing.T) {
	// Some valid pairs that shouldn't trigger anything.
	for _, p := range []struct{ a, b rune }{
		{'m', 'l'},
		{'t', 'r'},
		{'k', 'r'},
		{'s', 'p'},
		{'l', 'm'},
	} {
		if rule, _ := CheckProhibitedPair(p.a, p.b); rule != "" {
			t.Errorf("expected no rule for %s%s, got %q", string(p.a), string(p.b), rule)
		}
	}
}

func TestValidateCluster(t *testing.T) {
	r := ValidateCluster("ml")
	if !r.Valid {
		t.Errorf("ml should be valid, got %v", r.Errors)
	}
	r = ValidateCluster("kx")
	if r.Valid {
		t.Error("kx should fail rule 2.3")
	}
}

func TestHasTripleConsonant(t *testing.T) {
	if !HasTripleConsonant("mmml") {
		t.Error("mmml should have triple consonant")
	}
	if !HasTripleConsonant("xřřř") {
		t.Error("xřřř should have triple consonant")
	}
	if HasTripleConsonant("mml") {
		t.Error("mml is only a geminate, not triple")
	}
	if HasTripleConsonant("aaa") {
		t.Error("aaa is vowels, not a consonant triple")
	}
}

func TestAreHomologous(t *testing.T) {
	cases := []struct {
		a, b rune
		want bool
	}{
		{'p', 'b', true},  // both labial
		{'p', 'm', true},  // both labial
		{'t', 'd', true},  // both dental
		{'t', 'k', false}, // dental vs velar
		{'s', 'z', true},  // both alveolar
		{'š', 'ž', true},  // both alveolo-palatal
		{'s', 'š', false}, // alveolar vs alveolo-palatal
		{'l', 'r', false}, // no place group
	}
	for _, c := range cases {
		if got := areHomologous(c.a, c.b); got != c.want {
			t.Errorf("areHomologous(%s, %s) = %v, want %v",
				string(c.a), string(c.b), got, c.want)
		}
	}
}

func TestSameVoicing(t *testing.T) {
	if !sameVoicing('p', 't') {
		t.Error("both voiceless p, t should match")
	}
	if !sameVoicing('b', 'd') {
		t.Error("both voiced b, d should match")
	}
	if sameVoicing('p', 'b') {
		t.Error("p (voiceless) vs b (voiced) should mismatch")
	}
	// One of them not in voicing pair → treated as matching.
	if !sameVoicing('m', 'p') {
		t.Error("m has no voicing pair, should pass-through as matching")
	}
}

func TestCheckProhibitedPair_Rule218(t *testing.T) {
	if rule, _ := CheckProhibitedPair('ļ', 'b'); rule != "2.18" {
		t.Errorf("ļ + voiced stop b: rule = %q, want 2.18", rule)
	}
	if rule, _ := CheckProhibitedPair('h', 'ļ'); rule != "2.18" {
		t.Errorf("h + ļ: rule = %q, want 2.18", rule)
	}
	if rule, _ := CheckProhibitedPair('ļ', 's'); rule != "2.18" {
		t.Errorf("ļ + s: rule = %q, want 2.18", rule)
	}
}

func TestCheckProhibitedPair_Rule220_221(t *testing.T) {
	if rule, _ := CheckProhibitedPair('r', 'ř'); rule != "2.20" {
		t.Errorf("r + ř: rule = %q, want 2.20", rule)
	}
	if rule, _ := CheckProhibitedPair('h', 'ř'); rule != "2.20" {
		t.Errorf("h + ř: rule = %q, want 2.20", rule)
	}
	if rule, _ := CheckProhibitedPair('ř', 'r'); rule != "2.21" {
		t.Errorf("ř + r: rule = %q, want 2.21", rule)
	}
}

func TestCheckProhibitedPair_Rule222(t *testing.T) {
	// w/y not at conjunct end: followed by another consonant.
	if rule, _ := CheckProhibitedPair('w', 'm'); rule != "2.22" {
		t.Errorf("w + m: rule = %q, want 2.22", rule)
	}
	// w followed by vowel is fine.
	if rule, _ := CheckProhibitedPair('w', 'a'); rule != "" {
		t.Errorf("w + a should be ok, got %q", rule)
	}
}

func TestCheckProhibitedPair_Rule224(t *testing.T) {
	if rule, _ := CheckProhibitedPair('ç', 'ç'); rule != "2.24" {
		t.Errorf("çç: rule = %q, want 2.24", rule)
	}
	if rule, _ := CheckProhibitedPair('ļ', 'ļ'); rule != "2.24" {
		t.Errorf("ļļ: rule = %q, want 2.24", rule)
	}
}

func TestMaxClusterLength(t *testing.T) {
	if MaxClusterLength(Initial) != 4 {
		t.Errorf("initial = %d, want 4", MaxClusterLength(Initial))
	}
	if MaxClusterLength(Medial) != 6 {
		t.Errorf("medial = %d, want 6", MaxClusterLength(Medial))
	}
	if MaxClusterLength(Final) != 4 {
		t.Errorf("final = %d, want 4", MaxClusterLength(Final))
	}
}

func TestHasProhibitedGeminate(t *testing.T) {
	if !HasProhibitedGeminate("aww") {
		t.Error("\"aww\" has ww geminate, should be flagged")
	}
	if !HasProhibitedGeminate("yy") {
		t.Error("\"yy\" should be flagged")
	}
	if !HasProhibitedGeminate("a''") {
		t.Error("'' should be flagged")
	}
	if HasProhibitedGeminate("mm") {
		t.Error("mm (allowed nasal geminate) should pass")
	}
}

func TestValidateClusterAt_Length(t *testing.T) {
	// Initial cluster max 4.
	r := ValidateClusterAt(Initial, "mlnrs")
	if r.Valid {
		t.Error("5-rune initial cluster should fail length check")
	}
	// Medial cluster of 5 is OK.
	r = ValidateClusterAt(Medial, "mlnrs")
	// (May still flag pair errors but length isn't the problem.)
	var lengthErr bool
	for _, e := range r.Errors {
		if e.Rule == "length" {
			lengthErr = true
		}
	}
	if lengthErr {
		t.Error("5-rune medial cluster should not fail length check")
	}
}

func TestValidateClusterAt_Final(t *testing.T) {
	// w word-final → rule 4.1.
	r := ValidateClusterAt(Final, "aw")
	if r.Valid {
		t.Error("final w should fail rule 4.1")
	}
	// l word-final is fine.
	r = ValidateClusterAt(Final, "ml")
	for _, e := range r.Errors {
		if e.Rule == "4.1" {
			t.Errorf("final ml should not trigger 4.1, got %v", e)
		}
	}
}

func TestCheckProhibitedPair_Rule211(t *testing.T) {
	// n + sibilant affricate (n+c, n+č, n+ẓ, n+j).
	for _, c2 := range []rune{'c', 'č', 'ẓ', 'j'} {
		if rule, _ := CheckProhibitedPair('n', c2); rule != "2.11" {
			t.Errorf("expected 2.11 for n+%s, got %q", string(c2), rule)
		}
	}
}

func TestCheckProhibitedPair_Rule212(t *testing.T) {
	// m + labial / dental / interdental.
	for _, c2 := range []rune{'p', 'b', 't', 'd', 'ţ', 'ḑ'} {
		if rule, _ := CheckProhibitedPair('m', c2); rule != "2.12" {
			t.Errorf("expected 2.12 for m+%s, got %q", string(c2), rule)
		}
	}
}

func TestCheckProhibitedPair_Rule214(t *testing.T) {
	for _, c2 := range []rune{'p', 'b'} {
		if rule, _ := CheckProhibitedPair('n', c2); rule != "2.14" {
			t.Errorf("expected 2.14 for n+%s, got %q", string(c2), rule)
		}
	}
}

func TestValidateClusterAt_Rule213(t *testing.T) {
	// Nasal + homologous stop + sibilant.
	r := ValidateClusterAt(Medial, "mps")
	if r.Valid {
		t.Error("expected 2.13 error for mps")
	}
	var hit bool
	for _, e := range r.Errors {
		if e.Rule == "2.13" {
			hit = true
		}
	}
	if !hit {
		t.Errorf("expected rule 2.13 in errors, got %v", r.Errors)
	}
}

func TestValidateClusterAt_Rule215(t *testing.T) {
	// nf/nv must be followed by a vowel; nfp is invalid.
	r := ValidateClusterAt(Medial, "nfp")
	if r.Valid {
		t.Error("expected 2.15 error for nfp")
	}
	var hit bool
	for _, e := range r.Errors {
		if e.Rule == "2.15" {
			hit = true
		}
	}
	if !hit {
		t.Errorf("expected rule 2.15 in errors, got %v", r.Errors)
	}
}

func TestValidateClusterAt_Initial_LoneL(t *testing.T) {
	r := ValidateClusterAt(Initial, "ļ")
	if r.Valid {
		t.Error("lone ļ initial should fail rule 3.1")
	}
}

func TestValidateVowelSequence(t *testing.T) {
	cases := []struct {
		in    string
		valid bool
	}{
		{"a", true},          // single
		{"ai", true},         // diphthong
		{"ëi", true},         // diphthong
		{"ia", true},         // disyllabic conjunct (Series 3)
		{"ao", true},         // disyllabic conjunct (Series 4)
		{"ae", true},         // reference-root marker
		{"aa", false},        // not a permissible diphthong
		{"abc", false},       // too long
	}
	for _, c := range cases {
		r := ValidateVowelSequence(c.in)
		if r.Valid != c.valid {
			t.Errorf("ValidateVowelSequence(%q).Valid = %v, want %v (%v)",
				c.in, r.Valid, c.valid, r.Errors)
		}
	}
}
