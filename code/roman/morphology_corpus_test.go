package roman

import (
	"sort"
	"testing"
)

// Worked-example forms lifted from docs/reference/morphology.md.
// Each subtest names the section it came from so a future failure
// points back to the spec text it covers.
//
// Every word is checked against Quijada himself: the seven documents
// under $XDG_DATA_HOME/ithkuil/reference/ plus corpus/examples.txt.
// The spellings were taken from the markdown while it was still
// missing diacritics the PDF has, so 130 across this file and
// compose's copy were respelled from the source — "cskava" is çkava,
// "Mala" is Malá, and the jump examples are Aḑçulëuhá on the root
// -ḑç-. Where two documents disagree, v1.3.2 wins, being the version
// morphology.md transcribes.
//
// 30 were removed for having no source at all, and the §3.5 section
// with them. A word we cannot trace is not evidence about the
// classifier, so it does not belong in a drift guard: it can only
// fail for reasons we would have no way to judge.
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
		if _, err := ParseWord(w); err != nil {
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

// §3.5 had a section here, on wal / walurx / waluirx. All three are
// gone: the passage they illustrate is in none of Quijada's documents,
// and §3.5.0 is the same section G38 already records as ours rather
// than his. Nothing is left to assert.

func TestMorphologyCorpus_Sec4_6_1_Referential(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec4.6.1",
		words: []string{
			"to", "zua", "laiwe", "ëpgói", "ëztewim", "zëmse", "smoyút", "triwejvë",
			"smlo", "püwüp", "zäwiez",
		},
		// Every §4.6.1 example reads. Two of them are why: fo'we'is
		// carries a glottal-stop in each of its two case slots, one
		// per §1.7 placement, V_C1 by Rule 1 before the Slot 3 w and
		// V_C2 by Rule 3; and zëmse pads its z+m+s referent chain with
		// the epenthetic -ë- §4.6.1 puts "within C_1 combinations".
	})
}

func TestMorphologyCorpus_Sec4_6_2_CombinationRef(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec4.6.2",
		words: []string{
			"slex", "poxtanz", "ëtkexpa", "ëlsuoxxéd",
		},
	})
}

func TestMorphologyCorpus_Sec5_3_StativeDynamic(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.3",
		words: []string{
			"Byalá", "pa", "Byulá", "pu", "Vvralá", "mi", "wurçpëi", "urçpulëi",
			"Tladatřá", "çkava", "Tludatřá", "Txadá", "ku", "Txudá", "Waltlá",
			"wele", "lo", "Altlúl", "Malá", "welu", "wiosaḑcä", "espanya", "Mulá",
		},
		unknown: []string{
			"espanya",
		},
	})
}

func TestMorphologyCorpus_Sec5_4_RelativeClause(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.4",
		words: []string{
			"Weňayá", "kšilo", "äpçólöwa", "lu", "eňtyarkena", "thaxač", "li",
			"kšilenëi", "Erčädókh", "elavöte", "kšivëi", "Erčuláfs", "elaţwe",
			"kšivöto", "Yuřká", "kšila", "Umňälöřdá", "ẓúlikti", "kšilëi", "welene",
			"Umňälá", "kšivu", "thu",
		},
		// The Vc=uu words this section carried, hlarrnei-yurkuu and
		// hluu, are gone: neither is in any source document, so what
		// they showed about that gap was never checkable.
		unknown: nil,
	})
}

func TestMorphologyCorpus_Sec5_5_AttendantCase(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.5",
		words: []string{
			"Muliuţmá", "mu", "hlu", "hma", "ažxíp", "Wanzvihá",
		},
		// (Was unknown: [Wanzviha] — now classifies via case-
		// normalization in slots.Parse, and is spelled Wanzvihá.)
	})
}

func TestMorphologyCorpus_Sec5_6_WHQuestion(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.6",
		words: []string{
			"Weilüsve", "erčuléi", "utxoléi", "akftyäloë",
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
			"Hre", "willyothoilyá", "utplaliör", "A", "Kšölaölwáu",
		},
		// Kšölaölwáu reads, and it is the section's own subject
		// matter: it carries a §3.9.2 case-stacking affix on the -lw-
		// increment. It was unreadable while that family was
		// unimplemented, which this block's missing diacritics had
		// disguised as an extraction problem. Both are fixed now.
		unknown: []string{
			"Hre",
		},
	})
}

func TestMorphologyCorpus_Sec5_8_CHC_Eat(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.8 eat",
		words: []string{
			"Etxulá", "welacu", "wanžekcoë", "Itxulá", "Enulá", "laleco",
			"welacülwu", "etxulie", "welacurzu", "welacärzülwu", "welacärzu",
			"welacerzoë", "welaculwoë", "welacorzoë", "Etxulärzá",
		},
		// The -oë words read. The reference used to spell them "-ooe",
		// which was the extraction dropping the umlaut; it spells them
		// -oë again.
	})
}

func TestMorphologyCorpus_Sec5_8_CHC_Jump(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.8 jump",
		words: []string{
			"Aḑçulëuhá", "welecu", "welecärzu", "Yaḑçëuhá", "Aḑçulärzëuhá",
		},
		// All five read. Yaḑçëuhá needed both case-normalization in
		// slots.Parse and its cedillas back: the root is -ḑç-
		// 'jumping'. The "ampalaicooe" that sat here is gone, being in
		// no source document.
	})
}

func TestMorphologyCorpus_Sec5_8_CHC_Sing(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec5.8 sing",
		words: []string{
			"Yubškirfúi", "ellyuhrú", "lalacu", "ellyila", "ellyahrú", "Ellyulá",
			"ro", "Ellyulerzá", "Ellyalerzá",
		},
		// ellyahru and ellyuhru previously fell through; both classify
		// now via the §3.8.1.2 Cn-in-Ca shortcut (hr in the Ca slot).
		// (Was unknown: [Yubskirfui] — now classifies via case-
		// normalization in slots.Parse, and is spelled Yubškirfúi.)
	})
}

func TestMorphologyCorpus_Sec6_0_SPTDate(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec6.0",
		words: []string{
			"wuksärsëirwa", "wuksärsëirwiasta", "wustarsëirwiaza", "wullärsurya",
		},
	})
}

func TestMorphologyCorpus_Sec8_3_SpokenNumbers(t *testing.T) {
	runCorpusSection(t, corpusSection{
		name: "Sec8.3",
		words: []string{
			"ksalirsa", "gzalui", "walẓärs", "cpalärsa", "wapcui", "wansorsë'i",
			"cpalörs", "wallärsa",
		},
	})
}
