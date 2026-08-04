package search

import "testing"

// The vectors are from Porter's own paper and the standard voc.txt /
// output.txt pair distributed with it. They are here rather than
// derived from this code, so a wrong implementation fails rather than
// records itself.
func TestStem_Vectors(t *testing.T) {
	for _, tc := range [][2]string{
		{"caresses", "caress"}, {"ponies", "poni"}, {"ties", "ti"},
		{"caress", "caress"}, {"cats", "cat"},
		{"feed", "feed"}, {"agreed", "agre"}, {"plastered", "plaster"},
		{"bled", "bled"}, {"motoring", "motor"}, {"sing", "sing"},
		{"conflated", "conflat"}, {"troubled", "troubl"}, {"sized", "size"},
		{"hopping", "hop"}, {"tanned", "tan"}, {"falling", "fall"},
		{"hissing", "hiss"}, {"fizzed", "fizz"}, {"failing", "fail"},
		{"filing", "file"},
		{"happy", "happi"}, {"sky", "sky"},
		{"relational", "relat"}, {"conditional", "condit"},
		{"rational", "ration"}, {"valenci", "valenc"}, {"hesitanci", "hesit"},
		{"digitizer", "digit"}, {"conformabli", "conform"},
		{"radicalli", "radic"}, {"differentli", "differ"}, {"vileli", "vile"},
		{"analogousli", "analog"}, {"vietnamization", "vietnam"},
		{"predication", "predic"}, {"operator", "oper"}, {"feudalism", "feudal"},
		{"decisiveness", "decis"}, {"hopefulness", "hope"},
		{"callousness", "callous"}, {"formaliti", "formal"},
		{"sensitiviti", "sensit"}, {"sensibiliti", "sensibl"},
		{"triplicate", "triplic"}, {"formative", "form"},
		{"formalize", "formal"}, {"electriciti", "electr"},
		{"electrical", "electr"}, {"hopeful", "hope"}, {"goodness", "good"},
		{"revival", "reviv"}, {"allowance", "allow"}, {"inference", "infer"},
		{"airliner", "airlin"}, {"gyroscopic", "gyroscop"},
		{"adjustable", "adjust"}, {"defensible", "defens"},
		{"irritant", "irrit"}, {"replacement", "replac"},
		{"adjustment", "adjust"}, {"dependent", "depend"},
		{"adoption", "adopt"}, {"homologou", "homolog"},
		{"communism", "commun"}, {"activate", "activ"},
		{"angulariti", "angular"}, {"homologous", "homolog"},
		{"effective", "effect"}, {"bowdlerize", "bowdler"},
		{"probate", "probat"}, {"rate", "rate"}, {"cease", "ceas"},
		{"controll", "control"}, {"roll", "roll"},
		// The queries this was built for.
		{"speaks", "speak"}, {"speaking", "speak"}, {"trees", "tree"},
		{"studies", "studi"}, {"carries", "carri"}, {"watering", "water"},
	} {
		if got := Stem(tc[0]); got != tc[1] {
			t.Errorf("Stem(%q) = %q, want %q", tc[0], got, tc[1])
		}
	}
}

// TestStem_LeavesNonEnglishAlone pins that the lexicon's Latin
// binomials and Ithkuil clusters pass through. They are not English and
// a stem of one is noise; leaving them whole keeps them findable by the
// exact form, which is how anyone looks them up.
func TestStem_LeavesNonEnglishAlone(t *testing.T) {
	for _, w := range []string{"ţr", "l,x", "mļ", "ml", "'mļ", "🐈", "Catopuma"} {
		want := w
		if w == "Catopuma" {
			want = "catopuma"
		}
		if got := Stem(w); got != want {
			t.Errorf("Stem(%q) = %q, want %q", w, got, want)
		}
	}
}
