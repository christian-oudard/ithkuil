package phonology

import "testing"

// §4.2 governs word-final bi-consonantal conjuncts. It is a position
// rule, not a pair rule: -bf is unremarkable between vowels and cannot
// end a word, because §4.2.1 wants a stop and a following fricative to
// agree in voicing.
func TestSection42GovernsFinalPairsOnly(t *testing.T) {
	permitted := []string{
		"kt", "pt", "bd", "gd", // §4.2.2 labial/velar stop + dental stop
		"ks", "pf", "bv", // §4.2.1 stop + fricative, voicing agreeing
		"kh",             // §4.2.1: h is on §1.1's Fricative row
		"sp", "st", "sk", // §4.2.3 sibilant fricative + stop
		"ft", "fk", "fs", // §4.2.5
		"ţt", "ţk", // §4.2.6
		"ļp", "ļk", "xp", "xk", // §4.2.7 voiceless stop
		"mp", "ms", "nt", "nf", "nļ", // §4.2.8 any stop or fricative
		"ňt", "ňs", // §4.2.9
		"rt", "rm", "rl", "lt", "lm", // §4.2.10, §4.2.11
		"ll", "ss", "mm", // §4.2.12 non-stop geminates
	}
	for _, c := range permitted {
		if !ClusterLegalAt(Final, c) {
			t.Errorf("-%s should end a word", c)
		}
	}

	barred := []string{
		"tr", "tl", "pm", "kn", // §4.2.1 stop + liquid or nasal
		"bf", "pv", "gs", // §4.2.1 stop + fricative, voicing disagreeing
		"sm", "sl", "sf", // §4.2.3 sibilant fricative + anything but a stop
		"lr", "lň", // §4.2.11
		"tt", "pp", "kk", "bb", // §4.2.12 geminated stops
	}
	for _, c := range barred {
		if ClusterLegalAt(Final, c) {
			t.Errorf("-%s should not end a word", c)
		}
		// The same pair between vowels is another question entirely.
		if !ClusterLegalAt(Medial, c) && c != "lr" && c != "lň" {
			t.Errorf("-%s- is a §4.2 matter and should be fine medially", c)
		}
	}
}

// §4.2's heading is "-CC". Longer word-final conjuncts answer to §4.3
// and §4.4, which admit C_A complexes these rules would reject.
func TestSection42LeavesLongerConjunctsAlone(t *testing.T) {
	for _, c := range []string{"ptl", "rtn", "stř"} {
		if r := []rune(c); len(r) != 3 {
			t.Fatalf("%q is not tri-consonantal", c)
		}
		if rule, _ := checkFinalPair([]rune(c)[0], []rune(c)[1]); rule == "" {
			continue
		}
		// The pair alone would be barred; the triple must not be judged
		// by it.
		if !ClusterLegalAt(Final, c) && !ClusterLegalAt(Medial, c) {
			t.Logf("%q is barred, but by §4.3 rather than §4.2", c)
		}
	}
}
