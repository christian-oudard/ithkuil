package surface

import "testing"

func TestFromASCII(t *testing.T) {
	cases := []struct {
		ascii string
		want  string
	}{
		// Umlaut digraphs.
		{"aa", "ä"},
		{"ee", "ë"},
		{"oo", "ö"},
		{"uu", "ü"},
		// Right-grouping on longer vowel runs.
		{"e", "e"},
		{"eee", "eë"},
		{"eeee", "ëë"},
		{"eeeee", "eëë"},
		{"aaaa", "ää"},
		// Cedilla.
		{"t,", "ţ"},
		{"d,", "ḑ"},
		{"l,", "ļ"},
		{"c,", "ç"},
		// Háček.
		{"sq", "š"},
		{"zq", "ž"},
		{"cq", "č"},
		{"nq", "ň"},
		{"rq", "ř"},
		// Underdot.
		{"dz", "ẓ"},
		// `i` has no umlaut form — passes through.
		{"i", "i"},
		{"ii", "ii"},
		// Plain ASCII passes through.
		{"mal", "mal"},
		{"", ""},
		// Mixed: the test corpus word.
		{"Mat,rqeeullait", "Maţřëullait"},
		// Starters followed by a non-digraph commit as themselves.
		{"ta", "ta"},
		{"ca", "ca"},
		{"cb", "cb"},
		{"tt", "tt"},
		// A starter at end of input commits as itself.
		{"t", "t"},
		{"c", "c"},
		// Vowel run broken by a different vowel.
		{"ae", "ae"},
		{"aae", "äe"},
		{"aaae", "aäe"},
		// Vowel form table entries — round-tripping verified separately,
		// these check FromASCII directly.
		{"ooa", "öa"},   // öa (row 4, form 8)
		{"aoo", "aö"},   // aö (row 4, form 2)
		{"oee", "oë"},   // oë (row 4, form 5)
		{"ooe", "öe"},   // öe (row 4, form 6)
		{"eei", "ëi"},   // ëi (row 1, form 5)
		{"eee", "eë"},   // eë (row 3, form 5)
		{"ooaa", "öä"},  // öä (series-3 alternate 6)
		{"uuaa", "üä"},  // üä (series-3 alternate 3)
		{"iaa", "iä"},   // iä (series-3 alternate 9)
		// Stress on plain vowels.
		{"a/", "á"},
		{"e/", "é"},
		{"i/", "í"},
		{"o/", "ó"},
		{"u/", "ú"},
		// Stress on umlauted vowels.
		{"aa/", "â"},
		{"ee/", "ê"},
		{"oo/", "ô"},
		{"uu/", "û"},
		// Stress applies to the rightmost vowel of a multi-vowel pending run.
		{"aaa/", "aâ"},
		{"aaaa/", "äâ"},
		// Stress next to consonants.
		{"mula/", "mulá"},
		{"mulaa/", "mulâ"},
		{"prake/", "praké"},
		// Diphthong stress: first vowel of the falling pair.
		{"a/u", "áu"},
		{"ee/u", "êu"},
		// `i` is not a state-machine vowel (no umlaut), but stress still
		// works because it sits in committed when `/` arrives.
		{"i/", "í"},
		{"ai/", "aí"},
		// Glottal stop passes through.
		{"ma'a", "ma'a"},
		{"a'a/", "a'á"},
		// `/` with no preceding vowel passes through verbatim.
		{"/", "/"},
		{"t/", "t/"},
	}
	for _, c := range cases {
		got := FromASCII(c.ascii)
		if got != c.want {
			t.Errorf("FromASCII(%q) = %q, want %q", c.ascii, got, c.want)
		}
	}
}

func TestToASCII(t *testing.T) {
	cases := []struct {
		unicode string
		want    string
	}{
		{"ä", "aa"},
		{"ţ", "t,"},
		{"š", "sq"},
		{"ẓ", "dz"},
		{"Maţřëullait", "Mat,rqeeullait"},
		{"", ""},
		// Non-digraph chars pass through.
		{"abc", "abc"},
		{"i", "i"},
		{"'", "'"}, // glottal stop passes through verbatim
		// Right-grouped vowel encodes back to the raw run length.
		{"eë", "eee"},
		{"ëë", "eeee"},
		{"öä", "ooaa"},
		// Stressed plain vowels.
		{"á", "a/"},
		{"é", "e/"},
		{"í", "i/"},
		{"ó", "o/"},
		{"ú", "u/"},
		// Stressed umlaut vowels.
		{"â", "aa/"},
		{"ê", "ee/"},
		{"ô", "oo/"},
		{"û", "uu/"},
		// Stressed inside a word.
		{"mulá", "mula/"},
		{"mulâ", "mulaa/"},
		{"áu", "a/u"},
		{"êu", "ee/u"},
	}
	for _, c := range cases {
		got := ToASCII(c.unicode)
		if got != c.want {
			t.Errorf("ToASCII(%q) = %q, want %q", c.unicode, got, c.want)
		}
	}
}

// TestInputState exercises the streaming state: feed keys one at a
// time and check the committed/pending split at each step.
func TestInputState(t *testing.T) {
	cases := []struct {
		name      string
		feed      string
		committed string
		pending   string
	}{
		{"empty", "", "", ""},
		// Vowel run: pending grows, nothing committed until broken.
		{"e1", "e", "", "e"},
		{"e2", "ee", "", "ë"},
		{"e3", "eee", "", "eë"},
		{"e4", "eeee", "", "ëë"},
		// Plain vowel (i) commits immediately.
		{"i", "i", "i", ""},
		// Starter waits, resolves on second char.
		{"t alone", "t", "", "t"},
		{"t+comma", "t,", "ţ", ""},
		{"c+q", "cq", "č", ""},
		{"d+z", "dz", "ẓ", ""},
		// Starter + non-digraph → commit starter plain, new pending.
		{"t+a", "ta", "t", "a"},
		// Starter + starter → first commits, second pending.
		{"t+t", "tt", "t", "t"},
		// Vowel broken by a different vowel.
		{"aae", "aae", "ä", "e"},
		// Full word — the final `t` is a starter so it sits in
		// pending until the user types another char or commits.
		{"corpus", "Mat,rqeeullait", "Maţřëullai", "t"},
		// Without the trailing `t`, the `i` commits immediately
		// (not a starter), leaving no pending.
		{"corpus-no-t", "Mat,rqeeullai", "Maţřëullai", ""},
	}
	for _, c := range cases {
		var s InputState
		for _, r := range c.feed {
			s.Feed(r)
		}
		if got := s.Committed(); got != c.committed {
			t.Errorf("%s: committed = %q, want %q", c.name, got, c.committed)
		}
		if got := s.Pending(); got != c.pending {
			t.Errorf("%s: pending = %q, want %q", c.name, got, c.pending)
		}
	}
}

func TestInputStateBackspace(t *testing.T) {
	// "Maleeut," → committed "Malëuţ", pending "".
	var s InputState
	for _, r := range "Maleeut," {
		s.Feed(r)
	}
	if got := s.Display(); got != "Malëuţ" {
		t.Fatalf("setup display = %q, want Malëuţ", got)
	}
	// Pending is empty; Backspace pops the last committed rune (ţ).
	s.Backspace()
	if got := s.Display(); got != "Malëu" {
		t.Errorf("after 1 backspace = %q, want Malëu", got)
	}
	// Feeding three e's leaves pending "eee" → renders "eë".
	for _, r := range "eee" {
		s.Feed(r)
	}
	if got := s.Display(); got != "Malëueë" {
		t.Errorf("after eee = %q, want Malëueë", got)
	}
	// Backspace shrinks pending from "eee" to "ee" → renders "ë".
	s.Backspace()
	if got := s.Pending(); got != "ë" {
		t.Errorf("pending after backspace = %q, want ë", got)
	}
	if got := s.Display(); got != "Malëuë" {
		t.Errorf("display after backspace = %q, want Malëuë", got)
	}
}

func TestInputStateCommit(t *testing.T) {
	var s InputState
	for _, r := range "aaa" {
		s.Feed(r)
	}
	got := s.Commit()
	if got != "aä" {
		t.Errorf("Commit() = %q, want aä", got)
	}
	if s.Pending() != "" {
		t.Errorf("pending after commit = %q, want empty", s.Pending())
	}
}

// TestASCIIRoundTrip verifies FromASCII(ToASCII(w)) == w on a
// corpus of real Ithkuil orthographic words, including stress.
func TestASCIIRoundTrip(t *testing.T) {
	corpus := []string{
		"malëuţřait",
		"amlala",
		"fkhalo",
		"ihwe",
		"öhwoňo",
		"agulaha",
		"jwalö",
		"mzalörmëiňva",
		"walurx",
		"ëilal",
		"ealali",
		// Vowel-form table samples.
		"öa", "aö", "oë", "öe", "ëi", "eë", "öä", "üä", "iä",
		// Single-character samples.
		"a", "ä", "ç", "ţ", "ř",
		// Glottal stop.
		"ma'a", "a'i", "'a",
		// Stressed plain vowels.
		"á", "é", "í", "ó", "ú",
		// Stressed umlaut vowels.
		"â", "ê", "ô", "û",
		// Stressed inside real-shaped words.
		"mulá", "mulâ", "malëuţřáit", "äläntwá",
		// Stressed diphthongs (stress on the first/syllabic vowel).
		"áu", "êu", "óa", "âi",
	}
	for _, w := range corpus {
		ascii := ToASCII(w)
		round := FromASCII(ascii)
		if round != w {
			t.Errorf("round-trip %q: ToASCII=%q FromASCII=%q", w, ascii, round)
		}
	}
}

// TestASCIIGeminates verifies that every consonant digraph geminates
// uniformly: type the digraph twice. This is the orthogonal rule we
// rely on instead of per-digraph shortcuts. Shortcuts like `ssq`→`šš`
// only happen to be unambiguous for 5 of 10 digraphs (the others
// collide with valid clusters such as cč, nň, rř, lļ, dẓ); rather
// than introduce an asymmetric scheme, we double the digraph
// everywhere.
func TestASCIIGeminates(t *testing.T) {
	cases := []struct {
		ascii string
		want  string
	}{
		{"sqsq", "šš"},
		{"zqzq", "žž"},
		{"cqcq", "čč"},
		{"nqnq", "ňň"},
		{"rqrq", "řř"},
		{"t,t,", "ţţ"},
		{"d,d,", "ḑḑ"},
		{"l,l,", "ļļ"},
		{"c,c,", "çç"},
		{"dzdz", "ẓẓ"},
	}
	for _, c := range cases {
		if got := FromASCII(c.ascii); got != c.want {
			t.Errorf("FromASCII(%q) = %q, want %q", c.ascii, got, c.want)
		}
		if got := ToASCII(c.want); got != c.ascii {
			t.Errorf("ToASCII(%q) = %q, want %q", c.want, got, c.ascii)
		}
	}
}

// TestASCIINonGeminateClusters verifies that two-consonant sequences
// which share a prefix or suffix with one of our digraphs still parse
// to the intended pair, not to a digraph by accident. These are the
// cases that would *collide* if we ever adopted per-digraph gemination
// shortcuts — listed here so the conflict is visible.
func TestASCIINonGeminateClusters(t *testing.T) {
	cases := []struct {
		ascii string
		want  string
		note  string
	}{
		{"ccq", "cč", "would collide with a hypothetical čč shortcut"},
		{"nnq", "nň", "would collide with a hypothetical ňň shortcut"},
		{"rrq", "rř", "would collide with a hypothetical řř shortcut"},
		{"ll,", "lļ", "would collide with a hypothetical ļļ shortcut"},
		{"ddz", "dẓ", "would collide with a hypothetical ẓẓ shortcut"},
		// Real clusters with sibilants — these have to parse cleanly.
		{"ksq", "kš", "stop + sibilant fricative"},
		{"nsq", "nš", "nasal + sibilant fricative"},
		{"rsq", "rš", "liquid + sibilant fricative"},
		{"lsq", "lš", "liquid + sibilant fricative"},
		{"sqk", "šk", "sibilant fricative + stop"},
	}
	for _, c := range cases {
		if got := FromASCII(c.ascii); got != c.want {
			t.Errorf("FromASCII(%q) = %q, want %q (%s)", c.ascii, got, c.want, c.note)
		}
		if got := ToASCII(c.want); got != c.ascii {
			t.Errorf("ToASCII(%q) = %q, want %q (%s)", c.want, got, c.ascii, c.note)
		}
	}
}

// TestToASCIIPanicsOnUnmapped verifies that ToASCII fails loudly
// when it encounters a non-ASCII rune that isn't in the digraph
// table, rather than passing it through and producing non-ASCII
// output.
func TestToASCIIPanicsOnUnmapped(t *testing.T) {
	cases := []string{
		"aïa", // ï is not part of canonical V4 orthography
		"î",   // circumflex-i: stress on a non-existent umlaut-i
		"ǎ",   // háček: not used in V4
	}
	for _, s := range cases {
		func() {
			defer func() {
				if r := recover(); r == nil {
					t.Errorf("ToASCII(%q) did not panic", s)
				}
			}()
			_ = ToASCII(s)
		}()
	}
}

// TestASCIIRoundTripReverse verifies the other direction:
// ToASCII(FromASCII(a)) == a, on inputs that are themselves
// fixed-points of FromASCII (digraphs already resolved).
func TestASCIIRoundTripReverse(t *testing.T) {
	inputs := []string{
		"maleeut,rqait",
		"mula/",
		"mulaa/",
		"a/u",
		"ee/u",
		"ma'a",
		"a/",
		"u/",
		"oo/",
	}
	for _, a := range inputs {
		unicode := FromASCII(a)
		round := ToASCII(unicode)
		if round != a {
			t.Errorf("reverse round-trip %q: FromASCII=%q ToASCII=%q", a, unicode, round)
		}
	}
}
