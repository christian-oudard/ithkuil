package tokenize

import (
	"sort"
	"testing"
)

// Worked-example forms lifted from grammar_reference/morphology.md.
// Each subtest names the section it came from so a future failure
// points back to the spec text it covers.
//
// Each section asserts a snapshot: the exact set of words that
// currently fall through to UnknownWord. When the classifier grows to
// handle a previously-unknown form, the corresponding `unknown` slice
// shrinks — the test will fail and you should remove the now-parsing
// word from it. When a regression breaks a form that used to classify,
// the same test fails the other way. Either direction is interesting.

type corpusSection struct {
	name    string
	words   []string
	unknown []string // currently un-classified subset of words
}

func runCorpusSection(t *testing.T, s corpusSection) {
	t.Helper()
	var got []string
	for _, w := range s.words {
		if _, isUnk := ClassifyWord(w).(UnknownWord); isUnk {
			got = append(got, w)
		}
	}
	want := append([]string(nil), s.unknown...)
	sort.Strings(got)
	sort.Strings(want)
	if !equalStringSlice(got, want) {
		t.Errorf("%s: unclassified set drifted\n  got:  %v\n  want: %v",
			s.name, got, want)
	}
}

func equalStringSlice(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func TestMorphologyCorpus_Sec3_5_AffixType(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name:  "Sec3.5",
		words: []string{"wal", "walurx", "waluirx"},
	})
}

func TestMorphologyCorpus_Sec4_6_1_Referential(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec4.6.1",
		words: []string{
			"to", "zua", "laiwe", "ëpgói", "ëztewim", "zëmse", "smoyút",
			"triwejvë", "sme'e", "ka'u", "fo'we'is",
			"smlo", "püwüp", "zäwiez",
		},
		// Still falling through: fo'we'is (two glottal stops — the
		// second is a moved-glottal landing on the Vc2 of a referential
		// suffix, not yet handled) and zëmse (3-conjunct cluster shape
		// our referential matcher doesn't yet accept).
		unknown: []string{"fo'we'is", "zëmse"},
	})
}

func TestMorphologyCorpus_Sec4_6_2_CombinationRef(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name:  "Sec4.6.2",
		words: []string{"slex", "poxtanz", "ëtkexpa", "ëlsuoxxéd"},
	})
}

func TestMorphologyCorpus_Sec5_3_StativeDynamic(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.3",
		words: []string{
			"Byala", "pa", "Byula", "pu",
			"Vvrala", "mi", "wurcpei", "urcpulei",
			"Tladatra", "cskava", "Tludatra",
			"Txada", "ku", "Txuda",
			"Waltla", "wele", "lo", "Altlul",
			"Mala", "welu", "wiosadca", "espanya", "Mula",
		},
		unknown: []string{"espanya"},
	})
}

func TestMorphologyCorpus_Sec5_4_RelativeClause(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.4",
		words: []string{
			"Wenaya", "ksilo", "apcolowa", "lu", "entyarkena",
			"apcoloyu", "thaxac",
			"Zala", "li", "ksilenei", "malihu",
			"Ercadokh", "elavote", "zzjaduu", "ksivei",
			"Erculafs", "elatwe", "ainsaida", "ksivoto",
			"hlarrnei-yurkuu",
			"Yurka", "warrnernei", "ksila", "hluu",
			"Umnalorda", "zulikti", "ksilei", "welene",
			"Umnala", "ksivu", "thu",
		},
		// hlarrnei-yurkuu is a valid §3.1.7 chain (hlarrnei carries
		// Type-1 Cc shortcut hl, yurkuu is the parent) but yurkuu's
		// Vc=uu isn't recognised yet, so the parent half of the chain
		// fails parsing. Same Vc=uu issue takes hluu out of the
		// carrier-adjunct path under strict ParseCarrier.
		unknown: []string{"hlarrnei-yurkuu", "hluu", "zzjaduu"},
	})
}

func TestMorphologyCorpus_Sec5_5_AttendantCase(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.5",
		words: []string{
			"Muliutma", "mu", "hlu", "hma", "azxip", "hlie",
			"Wanzviha", "welei", "welie", "thie",
		},
		// (Was unknown: [Wanzviha] — now classifies via case-normalization
		// in slots.Parse, which lowercases the input.)
	})
}

func TestMorphologyCorpus_Sec5_6_WHQuestion(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.6",
		words: []string{
			"Weilusve", "erculei",
			"Lalutikusvu", "utxolei", "akftyaloe",
		},
	})
}

func TestMorphologyCorpus_Sec5_7_CaseStacking(t *testing.T) {
	// "Hre" appears in the source examples but is not classified today.
	// ("A" is the single-vowel RTR modular adjunct; it classifies now
	// that ClassifyWord case-normalizes.)
	runCorpusSection(t, corpusSection{
		name: "Sec5.7",
		words: []string{
			"Hre", "willyothoilya", "utplalior",
			"A", "hrelu-azcoijhailloelya",
			"Ksolaolwau",
		},
		// Ksolaolwau and hrelu-azcoijhailloelya are still unreadable.
		// Both lost diacritics along with the rest of this block (the
		// clown root is -kš-, not -ks-), but unlike the §5.8 words we
		// can't recover the original spelling from the intralinear
		// analysis alone, so they stay on the list until someone
		// checks them against the source PDF.
		unknown: []string{"Hre", "Ksolaolwau", "hrelu-azcoijhailloelya"},
	})
}

func TestMorphologyCorpus_Sec5_8_CHC_Eat(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.8 eat",
		words: []string{
			"Etxula", "welacu", "wanzekcoë",
			"Itxula",
			"Enula", "laleco", "welaculwu", "etxulie",
			"welacurzu", "welacarzulwu", "welacarzu",
			"welacerzoë", "welaculwoë", "welacorzoë",
			"Etxularza",
			"hetxejie-etxitoë",
		},
		// (Was unknown: all five -oë words, which the reference spells
		// "-ooe" because §5.8 lost its diacritics in extraction.)
	})
}

func TestMorphologyCorpus_Sec5_8_CHC_Jump(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.8 jump",
		words: []string{
			"Adcsulëuha", "welecu",
			"ampalaicoë", "welecarzu",
			"Yadcseuha", "Adcsuleuha", "Adcsularzeuha",
		},
		// (Was unknown: [Yadcseuha, ampalaicooe]. Yadcseuha now
		// classifies via case-normalization in slots.Parse; ampalaicoë
		// once its diacritic is restored.)
	})
}

func TestMorphologyCorpus_Sec5_8_CHC_Sing(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.8 sing",
		words: []string{
			"Yubskirfui", "ellyuhru", "lalacu", "ellyila",
			"ellyahru", "ellyulëihru",
			"Ellyula", "ro", "Ellyulerza", "Ellyalerza",
		},
		// ellyahru and ellyuhru previously fell through; both classify
		// now via the §3.8.1.2 Cn-in-Ca shortcut (hr in the Ca slot).
		// (Was unknown: [Yubskirfui] — now classifies via case-
		// normalization in slots.Parse.)
		//
		// (Was unknown: [ellyuleeihru], the same lost-diacritic
		// spelling of "ellyulëihru".)
	})
}

func TestMorphologyCorpus_Sec6_0_SPTDate(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec6.0",
		words: []string{
			"wuksarseirwa", "wuksarseirwiasta",
			"wustarseirwiaza", "walzarsao", "walzorsurwei",
			"wucpirwao", "wucpirwoltao",
			"wullarsurya", "wupsersaryoa",
		},
	})
}

func TestMorphologyCorpus_Sec8_3_SpokenNumbers(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec8.3",
		words: []string{
			"ksalirsa", "gzalui", "walẓärs",
			"cpalärsa", "wapcui", "wansorsë'i", "cpalörs",
			"wallärsa",
		},
	})
}
