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
	// §2.18 is directional: ļ "cannot be preceded by a voiced stop".
	if rule, _ := CheckProhibitedPair('b', 'ļ'); rule != "2.18" {
		t.Errorf("voiced stop b + ļ: rule = %q, want 2.18", rule)
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

func TestCheckProhibitedPair_Rule224_GeminatesAreAllowed(t *testing.T) {
	// §2.24 bars çç and ļļ, but §3.6.1 gemination builds both — its
	// own worked examples are çkl → ççkl and tçkl → tççkl — and the
	// corpus attests them (wiapļļalká, hamphelsuirççaité). The rule
	// governs root and affix conjuncts, not forms the morphology
	// itself generates.
	for _, r := range []rune{'ç', 'ļ'} {
		if rule, _ := CheckProhibitedPair(r, r); rule != "" {
			t.Errorf("%c%c should be allowed, got %s", r, r, rule)
		}
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

func TestValidateClusterAt_Rule212_Triples(t *testing.T) {
	// §2.12 triples — m + bilabial stop + indistinct follower.
	for _, c := range []string{"mpf", "mpţ", "mbv", "mbḑ", "mbd"} {
		r := ValidateClusterAt(Medial, c)
		if r.Valid {
			t.Errorf("expected %q to fail rule 2.12", c)
			continue
		}
		found := false
		for _, e := range r.Errors {
			if e.Rule == "2.12" {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("expected %q to flag rule 2.12, got %v", c, r.Errors)
		}
	}
	// "ngḑ" specifically prohibited; "nkţ" allowed.
	if r := ValidateClusterAt(Medial, "ngḑ"); r.Valid {
		t.Error("ngḑ should fail rule 2.12")
	}
	// nkţ is fine — but our existing pair checks may complain about
	// other things, so test only that 2.12 doesn't fire.
	for _, e := range ValidateClusterAt(Medial, "nkţ").Errors {
		if e.Rule == "2.12" {
			t.Errorf("nkţ should not trigger 2.12: %v", e)
		}
	}
}

func TestCheckProhibitedPair_Rule219(t *testing.T) {
	// §2.19: final-h preceded by ļ/x/ç is prohibited. Some of these
	// pairs also fail under other rules (xh under §2.17, çh under
	// §2.10); we only require that they get rejected — the specific
	// rule that catches them first is informational.
	for _, a := range []rune{'ļ', 'x', 'ç'} {
		rule, _ := CheckProhibitedPair(a, 'h')
		if rule == "" {
			t.Errorf("%s+h not flagged by any rule (expected at least §2.19)", string(a))
		}
	}
	// ļ+h is the case §2.19 catches that nothing else does.
	if rule, _ := CheckProhibitedPair('ļ', 'h'); rule != "2.19" {
		t.Errorf("ļ+h: expected 2.19, got %q", rule)
	}
}

func TestValidateClusterAt_Rule51_IntervocalicLam(t *testing.T) {
	// §5.1: bare intervocalic -ļ- prohibited.
	r := ValidateClusterAt(Medial, "ļ")
	if r.Valid {
		t.Error("intervocalic ļ alone should fail rule 5.1")
	}
	// Same letter in cluster is fine if not alone.
	for _, e := range ValidateClusterAt(Medial, "pļ").Errors {
		if e.Rule == "5.1" {
			t.Errorf("pļ should not trigger 5.1: %v", e)
		}
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

func TestCheckProhibitedPair_Rule212_PairsAreLegal(t *testing.T) {
	// §2.12 is about triples. The pairs it names — mt, md, mţ, mḑ —
	// are the permitted forms the prohibited triples collapse to, and
	// mp/mb carry the Ca complex's MSF configuration.
	for _, c2 := range []rune{'p', 'b', 't', 'd', 'ţ', 'ḑ'} {
		if rule, reason := CheckProhibitedPair('m', c2); rule != "" {
			t.Errorf("m+%s should be legal, got %s: %s", string(c2), rule, reason)
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
		{"a", true},    // single
		{"ai", true},   // diphthong
		{"ëi", true},   // diphthong
		{"ia", true},   // disyllabic conjunct (Series 3)
		{"ao", true},   // disyllabic conjunct (Series 4)
		{"ae", true},   // reference-root marker
		{"aa", false},  // not a permissible diphthong
		{"abc", false}, // too long
	}
	for _, c := range cases {
		r := ValidateVowelSequence(c.in)
		if r.Valid != c.valid {
			t.Errorf("ValidateVowelSequence(%q).Valid = %v, want %v (%v)",
				c.in, r.Valid, c.valid, r.Errors)
		}
	}
}

func TestError_String(t *testing.T) {
	e := Error{Rule: "1.2", Cluster: "xy", Reason: "test reason"}
	if got := e.String(); got != "1.2: test reason (cluster xy)" {
		t.Errorf("Error.String() = %q, want with cluster", got)
	}
	e = Error{Rule: "1.2", Reason: "test reason"}
	if got := e.String(); got != "1.2: test reason" {
		t.Errorf("Error.String() = %q, want without cluster", got)
	}
}

func TestMaxClusterLength_AllPositions(t *testing.T) {
	cases := []struct {
		p    Position
		want int
	}{
		{Initial, 4},
		{Medial, 6},
		{Final, 4},
		{Position(99), 0},
	}
	for _, c := range cases {
		if got := MaxClusterLength(c.p); got != c.want {
			t.Errorf("MaxClusterLength(%v) = %d, want %d", c.p, got, c.want)
		}
	}
}

func TestPosition_String(t *testing.T) {
	if Initial.String() != "initial" || Medial.String() != "medial" || Final.String() != "final" {
		t.Error("Position.String() mismatched")
	}
}

func TestIsVoicedStop(t *testing.T) {
	for _, r := range []rune{'b', 'd', 'g'} {
		if !IsVoicedStop(r) {
			t.Errorf("IsVoicedStop(%c) = false, want true", r)
		}
	}
	for _, r := range []rune{'p', 't', 'k', 'a'} {
		if IsVoicedStop(r) {
			t.Errorf("IsVoicedStop(%c) = true, want false", r)
		}
	}
}

func TestVoicedOf_AllPairs(t *testing.T) {
	cases := []struct {
		in, want rune
	}{
		{'p', 'b'}, {'b', 'b'},
		{'t', 'd'}, {'d', 'd'},
		{'k', 'g'}, {'g', 'g'},
		{'f', 'v'}, {'v', 'v'},
		{'ţ', 'ḑ'}, {'ḑ', 'ḑ'},
		{'s', 'z'}, {'z', 'z'},
		{'š', 'ž'}, {'ž', 'ž'},
		{'c', 'ẓ'}, {'ẓ', 'ẓ'},
		{'č', 'j'}, {'j', 'j'},
	}
	for _, c := range cases {
		if got := voicedOf(c.in); got != c.want {
			t.Errorf("voicedOf(%c) = %c, want %c", c.in, got, c.want)
		}
	}
	if got := voicedOf('m'); got != 0 {
		t.Errorf("voicedOf(m) = %c, want 0", got)
	}
}

func TestValidateClusterAt_InitialGlottal(t *testing.T) {
	// 1.5: glottal stop word-initial within a multi-rune cluster.
	r := ValidateClusterAt(Initial, "'l")
	if r.Valid {
		t.Error("ValidateClusterAt(initial, 'l): expected invalid")
	}
}

func TestValidateClusterAt_MTripleIndistinct(t *testing.T) {
	for _, c := range []string{"mpf", "mpţ", "mbv", "mbḑ", "mbd"} {
		r := ValidateClusterAt(Medial, c)
		if r.Valid {
			t.Errorf("ValidateClusterAt(medial, %q): expected invalid", c)
		}
	}
}

func TestValidateClusterAt_MedialProhibitedCluster(t *testing.T) {
	r := ValidateClusterAt(Medial, "ngḑ")
	if r.Valid {
		t.Error("ValidateClusterAt(medial, ngḑ): expected invalid")
	}
}

func TestValidateClusterAt_FinalGlottalAndYW(t *testing.T) {
	if ValidateClusterAt(Final, "ly").Valid {
		t.Error("ValidateClusterAt(final, ly): expected invalid")
	}
	if ValidateClusterAt(Final, "l'").Valid {
		t.Error("ValidateClusterAt(final, l'): expected invalid")
	}
}

func TestCheckProhibitedPair_RemainingRules(t *testing.T) {
	cases := []struct {
		a, b rune
		rule string
	}{
		{'š', 'c', "2.6"},
		{'c', 's', "2.9"},
		{'s', 'c', "2.9"},
		{'ç', 's', "2.10"},
		{'s', 'ç', "2.10"},
		{'c', 'ç', "2.10"},
		{'ļ', 'ç', "2.10"},
		{'ň', 'k', "2.16"},
		{'x', 's', "2.17"},
		{'x', 'g', "2.17"},
		{'b', 'ļ', "2.18"},
		{'h', 'ļ', "2.18"},
		{'ļ', 's', "2.18"},
		{'ļ', 'h', "2.19"},
		{'r', 'ř', "2.20"},
		{'h', 'ř', "2.20"},
		{'ř', 'r', "2.21"},
		{'w', 'p', "2.22"},
		{'ḑ', 's', "2.23"},
		{'n', 'ň', "2.23"},
	}
	for _, c := range cases {
		rule, reason := CheckProhibitedPair(c.a, c.b)
		if rule != c.rule {
			t.Errorf("CheckProhibitedPair(%c,%c) rule = %q, want %q (reason=%q)",
				c.a, c.b, rule, c.rule, reason)
		}
	}
}

func TestValidateClusterAt_ProhibitedGeminate(t *testing.T) {
	r := ValidateClusterAt(Medial, "''")
	if r.Valid {
		t.Error("ValidateClusterAt(medial, ''): expected invalid")
	}
}

// TestValidWordInitial checks the §3.1/§3.2 word-initial inventory
// against the clusters the spec itself names, in both directions. The
// renderer consults this before eliding a leading default Vv, which is
// what moves a root cluster into word-initial position.
func TestValidWordInitial(t *testing.T) {
	// Drawn from the example lists in §3.2 and its sub-rules.
	legal := []string{
		"m", "k", "h",
		"pţ", "pf", "bv", "bḑ", "pļ", "px", "tç", "ph", "tf", "dv",
		"tx", "tļ", "th", "kç", "kf", "gv", "kţ", "gḑ", "kh",
		"ps", "gz", "kš", "bž", "tr", "kl", "gy",
		"ml", "nr", "ňl", "hl", "hw", "lw", "ry", "sm", "žv",
		// §3.3 triples.
		"psm", "kšp", "pfw", "kţy", "pļy", "tļy", "kçm", "tlw", "kry",
		"skl", "zgr", "çpw", "smw", "cpr", "hlw", "hmy", "flw", "ţly",
		"xpl", "xmw", "xcw",
		// §3.4 quadruples.
		"pskw", "gzdr", "kšpl", "bždy", "pstř", "skly", "zgly",
	}
	illegal := []string{
		"",
		"ļ",        // §3.1: indistinguishable from word-initial hl-
		"rd", "ln", // §3.2.9: l- and r- take only -w or -y
		"kļ",                   // §3.2: explicit exception, too close to tļ
		"pm", "bn", "tn", "dm", // §3.2: bilabial/dental stop + nasal
		"pz", "gs", "kž", "bš", // §3.2: sibilant of the wrong voicing
		"ňy", "ňř", // §3.2.8: ň- excludes -y and -ř
		"rdv", "mlk", // §3.3: no rule admits a liquid- or nasal-initial triple
		"hxw",        // §3.3.5: the h- triples are a closed list
		"flm", "xfl", // §3.3.7 / §3.3.8: neither list admits these
		"kţgy", // §3.4: the tri-prefix "kţg" is itself illegal
	}
	for _, c := range legal {
		if !ValidWordInitial(c) {
			t.Errorf("ValidWordInitial(%q) = false, want true", c)
		}
	}
	for _, c := range illegal {
		if ValidWordInitial(c) {
			t.Errorf("ValidWordInitial(%q) = true, want false", c)
		}
	}
}
