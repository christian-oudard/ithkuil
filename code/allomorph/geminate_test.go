package allomorph

import "testing"

// TestGeminateCa_SpecExamples exercises the worked examples from §3.6.1.
func TestGeminateCa_SpecExamples(t *testing.T) {
	cases := []struct{ in, want string }{
		// Rule 1: single consonant → double.
		{"p", "pp"}, {"t", "tt"}, {"m", "mm"},
		{"c", "cc"}, {"ẓ", "ẓẓ"}, {"r", "rr"}, {"s", "ss"},
		{"l", "ll"},
		// Rule 2: standalone tļ → ttļ.
		{"tļ", "ttļ"},
		// Rule 3: stop + liquid/approximant.
		{"pl", "ppl"}, {"gw", "ggw"},
		// Rule 4: sibilant anywhere.
		{"kst", "ksst"}, {"gz", "gzz"},
		{"çkl", "ççkl"}, {"čtw", "ččtw"},
		// Rule 5: initial non-sibilant fricative or nasal.
		{"fk", "ffk"}, {"mpw", "mmpw"},
		// Rule 6: voiceless stop + fricative.
		{"pf", "pff"}, {"tçkl", "tççkl"},
		// Rule 7: two-stop endings.
		{"pt", "bbḑ"}, {"pk", "bbv"}, {"kt", "ggḑ"},
		{"kp", "ggv"}, {"tk", "ḑvv"}, {"tp", "ddv"},
		// Rule 8: stop+nasal endings.
		{"pm", "vvm"}, {"pn", "vvn"}, {"km", "xxm"},
		{"kn", "xxn"}, {"tm", "ḑḑm"}, {"tn", "ḑḑn"},
		{"bm", "mmw"}, {"bn", "mml"}, {"gm", "ňňw"},
		{"gn", "ňňl"}, {"dm", "nnw"}, {"dn", "nnl"},
	}
	for _, c := range cases {
		if got := GeminateCa(c.in); got != c.want {
			t.Errorf("GeminateCa(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

// TestGeminateCa_AllCaForms verifies that every one of the 3840 Ca
// clusters has a non-empty geminated form distinct from the plain
// cluster.
func TestGeminateCa_AllCaForms(t *testing.T) {
	for slot, plain := range CaForward {
		gem := GeminateCa(plain)
		if gem == "" {
			t.Errorf("empty geminated form for Ca %q (%v)", plain, slot)
		}
		if gem == plain {
			t.Errorf("gemination is a no-op for Ca %q (%v)", plain, slot)
		}
	}
}

// §3.6.1's nine rules are a default plus exceptions, not a dispatch
// table. The default is what "gemination of the C_A form" means on its
// own: double the initial consonant. These cases pin that, because
// reading the rules as exhaustive and calling the forms none of them
// names a gap in the language is a mistake made here twice.
func TestGeminate_DefaultIsDoubleTheFirst(t *testing.T) {
	// No numbered rule names any of these. The default answers them.
	for _, c := range []struct{ in, want string }{
		{"kbl", "kkbl"}, // stop + stop + liquid: rule 3 wants the FIRST
		{"kth", "kkth"}, // ...not the stop next to the h
		{"kb", "kkb"},   // rule 7's table omits the voiced pairs
		{"tg", "ttg"},
		{"pň", "ppň"}, // rule 8's table omits ň
		{"xw", "xxw"}, // rule 5's list omits x
		{"kv", "kkv"}, // rule 6's list omits v
	} {
		if got := GeminateCa(c.in); got != c.want {
			t.Errorf("GeminateCa(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

// You cannot geminate mid-cluster except on a fricative. Rules 4 and 6
// are the only ones that double a medial consonant and both double a
// fricative; doubling a medial stop is unsayable, akbbla and akttha
// against akkbla and akktha. So these must double where they do and
// nowhere else.
func TestGeminate_MidClusterOnlyOnAFricative(t *testing.T) {
	for _, c := range []struct{ in, want, why string }{
		{"kst", "ksst", "rule 4, medial sibilant"},
		{"gz", "gzz", "rule 4"},
		{"çkl", "ççkl", "rule 4, already initial"},
		{"pf", "pff", "rule 6, fricative after a voiceless stop"},
		{"ẓb", "ẓẓb", "rule 4 reaches ẓ: phonotactics §1 defines the " +
			"sibilant affricates as c, ẓ, č, j"},
		{"pt", "bbḑ", "rule 7 substitutes: doubling would leave a stop on a stop"},
		{"pm", "vvm", "rule 8 substitutes, same reason"},
	} {
		if got := GeminateCa(c.in); got != c.want {
			t.Errorf("GeminateCa(%q) = %q, want %q (%s)", c.in, got, c.want, c.why)
		}
	}
}

// The whole space, which is what makes the default trustworthy: every
// Ca value gets exactly one geminate, all legal, all distinct from one
// another, and none equal to a bare Ca. If §3.6.1 really had a hole,
// this is where it would show as a collision or an illegal form.
func TestGeminate_EveryFormIsCoveredAndDistinct(t *testing.T) {
	bare := map[string]bool{}
	for _, ca := range CaForward {
		bare[ca] = true
	}
	source := map[string]string{}
	for _, ca := range CaForward {
		gem := GeminateCa(ca)
		if gem == ca {
			t.Errorf("Ca %q geminates to itself", ca)
		}
		if bare[gem] {
			t.Errorf("Ca %q geminates to %q, which is itself a bare Ca", ca, gem)
		}
		if prev, ok := source[gem]; ok && prev != ca {
			t.Errorf("Ca %q and %q both geminate to %q", prev, ca, gem)
		}
		source[gem] = ca
	}
}
