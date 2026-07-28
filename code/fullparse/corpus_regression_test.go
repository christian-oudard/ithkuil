package fullparse_test

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/render"
)

// Words in this file come from the community Discord archive (see
// tools/discord_archive). They are attested usage rather than
// constructed examples, and each one caught a defect that the
// hand-written corpus did not.
//
// The invariant under test is round-trip fidelity: parse, render, and
// parse again must land on the same gloss. Whether our canonical
// spelling matches what the author typed is a separate question and
// deliberately not asserted here, because the renderer canonicalizes.
//
// A few words predate the 2023 morphology we implement. words.py drops
// that era from the audit corpus, but a word kept here is kept because
// the rule it breaks is a v1.3.1 rule, so its vintage does not matter.
// Where the intended reading is genuinely unrecoverable the word is not
// here at all: ažxwö'rka'súm, for one, is 2020 bot output that segments
// with a glottal at the head of its Ca, and its neighbours in the same
// message are spelled with ı and ù, letters v4 does not have.

// roundTrips checks one word. A §3.1.7 chain is a sequence of
// formatives sharing one word, so each link is checked on its own.
func roundTrips(t *testing.T, word string) bool {
	t.Helper()
	if strings.Contains(word, "-") {
		ok := true
		for _, part := range strings.Split(word, "-") {
			ok = roundTripsOne(t, part) && ok
		}
		return ok
	}
	return roundTripsOne(t, word)
}

func roundTripsOne(t *testing.T, word string) bool {
	t.Helper()
	gl := &gloss.Glosser{Canonical: true}
	f, err := fullparse.Formative(word)
	if err != nil {
		t.Errorf("%s: parse: %v", word, err)
		return false
	}
	out := render.Formative(f)
	back, err := fullparse.Formative(out)
	if err != nil {
		t.Errorf("%s -> %s: re-parse: %v", word, out, err)
		return false
	}
	if a, b := gl.Formative(f), gl.Formative(back); a != b {
		t.Errorf("%s -> %s:\n  before %s\n  after  %s", word, out, a, b)
		return false
	}
	return true
}

// TestCorpus_LeadingVvElision covers the words that were rendering
// into an illegal word-initial cluster, or into a root that the
// sentence-prefix stripper would eat.
func TestCorpus_LeadingVvElision(t *testing.T) {
	for _, w := range []string{
		"ardvilëilḑá", // §3.2.9: word-initial r- takes only -w or -y
		"akţgyiva",    // §3.4: no tetra-conjunct has the tri-prefix kţg
		"ažxuisi",     // §3.3.8: the x- triples are a closed list
		"aňzkçalks",   // ňzkç- opens with a nasal
		"açmuliwá",    // ç- would be read as the §1.3.2 juncture marker
		"açpulúgmö",   //
		"amálawëi",    // m- is fine, so this one does elide
		"afbanarţu'o", // fb- is fine
	} {
		roundTrips(t, w)
	}
}

// TestCorpus_GlottalPlacement covers the largest remaining cluster of
// round-trip failures: a §1.7 glottal that render puts somewhere the
// parser then reads as belonging to a different slot. The Vv forms
// come back as an invalid Vk, and the Slot V forms move their
// end-of-slot marker onto the wrong affix.
func TestCorpus_GlottalPlacement(t *testing.T) {
	for _, w := range []string{
		// Render emits a reduplicated Vv ("a'a-") that won't re-parse.
		"aňzkçaẓäçnëumvuožžô",
		"rtawuihážžg",
		// Render emits a doubled glottal in the Vc.
		"hrarráu-wäḑxë'iza'o",
		"huţrilú-ujjäli'a",
		// The Slot V end-of-slot glottal lands on the wrong affix.
		"a'ajthuiňřoirguoẓẓwa",
		"a'ļfažíravva",
		"alçpi'lä-abzgauçmaussä",
		"arštilsau'lualla",
		// A spurious glottal upgrades the case by 36 (IND u -> RLT u'u,
		// EFF ö -> UTL ö'ö, CSD ua -> NAV u'a, PAR ui -> VOC u'i).
		"idřoutasyövvu",
		"mcialorjiřřö",
		"häsuandävussahniä",
		"hasäza-wappļaţsö",
	} {
		roundTrips(t, w)
	}
}

// TestCorpus_ConcatenatedChain covers §3.1.7 chains. A hyphen joins
// whole formatives, so it is legal in a word but never inside one;
// these all came back with their slots shifted by one because the
// hyphen was segmenting as a consonant conjunct and landing in an
// affix Cs.
func TestCorpus_ConcatenatedChain(t *testing.T) {
	for _, w := range []string{
		"heltyurëi-annarëi",
		"hlabzëicdú-afçnizyuëlla",
		"hlabzřëicdú-afçnizyuëlla",
		"hlarrëicdú-afçnizyuëlla",
		"hluňtyí-ukţgwanţëull",
		"hlainglöšdí-abvlä'löḑbu",
		"hriamžé-akbiçňuivva",
		"hroisvé-maţřëujja",
		"hropšmyí-okšňafřuilli",
		"hmuksküţmurbâ-a'rkwau'zwëillikbiažřui",
		"hmuksküţmurbâ-arkwauzwëillikbiažřu'i",
	} {
		roundTrips(t, w)
	}
}

// TestFormative_RejectsChain pins the other half of that fix: a chain
// is a sequence of formatives, so asking for one formative is an error
// rather than a best effort.
func TestFormative_RejectsChain(t *testing.T) {
	if _, err := fullparse.Formative("heltyurëi-annarëi"); err == nil {
		t.Error("Formative accepted a hyphenated chain as one formative")
	}
}

// TestCorpus_UltimateStressVf covers §3.1.3: a concatenated formative
// under ultimate stress ends in an alternate Vf, not a Vk. The stress
// promotes the Format vowel into the 37-68 range instead of switching
// the word to a verbal reading.
func TestCorpus_UltimateStressVf(t *testing.T) {
	for _, w := range []string{
		"hliařţiá-wa'aňsätļi'jva",
		"hliařţiá-wa'aňsätļijva'řga",
	} {
		roundTrips(t, w)
	}
}

// TestFormat_EveryCase walks the whole V_F table rather than the two
// words that exposed it. Cases 1-36 keep penultimate stress on a
// dependent; 37-68 lose their glottal and take ultimate stress
// instead. The parent of each pair is a standalone formative and must
// not move, which is the half a one-sided fix would break.
func TestFormat_EveryCase(t *testing.T) {
	for _, c := range g.AllCases {
		for _, concat := range []g.ConcatenationStatus{g.Type1, g.Type2, g.ConcatNone} {
			f := g.MinimalFormative("l")
			f.Concat = concat
			f.Final = g.UnframedNominal{Case: c}
			out := render.Formative(f)
			back, err := fullparse.Formative(out)
			if err != nil {
				t.Errorf("%v %v renders to %q: %v", concat, c, out, err)
				continue
			}
			if back.Concat != concat {
				t.Errorf("%v %v -> %q: Concat = %v", concat, c, out, back.Concat)
			}
			n, ok := back.Final.(g.UnframedNominal)
			if !ok {
				t.Errorf("%v %v -> %q: Final = %T, want UnframedNominal", concat, c, out, back.Final)
				continue
			}
			if n.Case != c {
				t.Errorf("%v %v -> %q: read back as %v", concat, c, out, n.Case)
			}
			if concat != g.ConcatNone && strings.Contains(out, "'") {
				t.Errorf("%v %v -> %q: §3.1.6 bars a glottal in a dependent's Vf", concat, c, out)
			}
		}
	}
}

// TestCorpus_GlottalHeadedCs pins §3.5: "No C_S form can contain a
// glottal-stop." §1.7 Rule 1 says where one between a vowel and a
// consonant belongs — after the vowel-form — so it is the Vx in front
// that carries it, never the Cs behind. We used to build the affix
// anyway; the renderer then had nowhere to put the glottal, dropped
// it, and the affix came back a degree off in a different slot.
//
// The eight words this cost are all from 2020, and the archived
// morphology of each word's own date explains it exactly. Two v1.3.1
// rules did not exist yet: C_N still needed a preceding glottal stop,
// which v0.17.0's changelog is where it goes away, and Bias was a
// word-final C_B suffix rather than the §4.7 adjunct, taking a glottal
// of its own. So every "'h" and "'l" here is a Slot VIII C_N, and
// "'kšš", "'gzz" and "'žžg" are the CTP, EUH and DEJ biases. The forms
// did not change, only their attachment, so the update is to split the
// suffix off into its own word. corpus/discord_examples.txt carries
// the readings and the modernised forms.
//
// Nothing in the language we do implement pays for the rule: across
// the 3657-word audit corpus, which words.py filters to 2023 and
// after, no word parses into an affix whose Cs holds a glottal.
//
// The same §3.5 sentence also bars a geminated C_S, and that half is
// not enforced here. Nine words in the audit corpus parse into one
// (ltsst, nnl, ggz x2, ddy, dd, jj, ll, mm), which makes it a live
// question about the language we do implement rather than about 2020,
// and it wants its own look.
func TestCorpus_GlottalHeadedCs(t *testing.T) {
	for _, w := range []string{
		"anţtaleu'há",
		"anzvarönţiçřoi'há'kšš",
		"ežfaléa'ha'rš",
		"anzvarönţiçřoi'hákšš",
		"attualië'hú'žžg",
		"aňţärko'lá'rš",
		"uňňsozahé'kšš",
		"ňvailoţmá'gzz",
	} {
		if _, err := fullparse.Formative(w); err == nil {
			t.Errorf("Formative(%q) succeeded; its Cs holds a glottal, which §3.5 forbids", w)
		}
	}
}

// TestCorpus_CsRootSlotV covers §4.2. A specialized C_S-root
// "operates like a standard formative except that Slots II and IV take
// specialized V_V and V_R forms and the Slot III C_R form is replaced
// by the C_S-form of a V_X C_S affix" — Slot V is not among the
// exceptions, so a §3.6.1 geminated C_A means there what it means
// anywhere else. We were not looking for one, and read the first
// cluster after V_R as the C_A instead, which pushed the Slot V
// affixes into Slot VII and left the geminate sitting in an affix C_S
// where §3.5 forbids it.
//
// In ëicalçeajja the C_A is "j", geminated to "jj", with "lç"+"ea" as
// a Slot V affix. We read C_A "lç" and put "ea"+"jj" in Slot VII.
func TestCorpus_CsRootSlotV(t *testing.T) {
	for _, c := range []struct{ word, ca string }{
		{"ëicalçeajja", "j"},
		{"aešalyaidde", "d"},
		{"ëitfëibëinnležói", "dn"},
		{"ëitheřviddyá", "dy"},
	} {
		f, err := fullparse.Formative(c.word)
		if err != nil {
			t.Errorf("%s: %v", c.word, err)
			continue
		}
		if len(f.SlotV) == 0 {
			t.Errorf("%s: no Slot V affix; the geminated Ca says there is one", c.word)
		}
		roundTripsOne(t, c.word)
	}
}

// TestCorpus_RootCannotStartWithHWY pins §3's restriction on root
// shape. These parsed into a Formative with Cr = "w", which is not a
// possible root, and then rendered into a word that read differently.
func TestCorpus_RootCannotStartWithHWY(t *testing.T) {
	for _, w := range []string{"awaçmas", "awaçmasá", "awaçmüsúi", "awáçmaisa"} {
		if _, err := fullparse.Formative(w); err == nil {
			t.Errorf("Formative(%q) succeeded; a root may not begin with w-", w)
		}
	}
}

// TestCorpus_NoWordFinalApproximant covers §4.1: a word may end in any
// single consonant except -w or -y, and §2.22 lets both appear only as
// the last member of a conjunct with a vowel after them. Every word
// here has a root ending in w or y, so eliding the default Vc strands
// that approximant at the end of the word.
//
// These are the last five corpus words where a legal input renders
// into something our own validator rejects.
func TestCorpus_NoWordFinalApproximant(t *testing.T) {
	for _, w := range []string{
		"wiärkwá",          // root rkw, Slot IV/VI shortcut
		"uļgwalá",          // root ļgw, long form that renders as a shortcut
		"waňtyá",           // root ňty
		"oţtaswivv",        // Ca sw
		"a'lčkhwakcažyivv", // Ca žy
	} {
		f, err := fullparse.Formative(w)
		if err != nil {
			t.Errorf("%s: %v", w, err)
			continue
		}
		out := render.Formative(f)
		if err := phonology.CheckText(out); err != nil {
			t.Errorf("%s renders to %q, which our own phonotactics reject: %v", w, out, err)
		}
	}
}
