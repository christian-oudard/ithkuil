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
