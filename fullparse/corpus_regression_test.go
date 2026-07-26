package fullparse_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/gloss"
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

func roundTrips(t *testing.T, word string) bool {
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
	t.Skip("known defect: see the glottal round-trip group in the corpus audit")
	for _, w := range []string{
		// Render emits a reduplicated Vv ("a'a-") that won't re-parse.
		"anzvarönţiçřoi'hákšš",
		"attualië'hú'žžg",
		"aňzkçaẓäçnëumvuožžô",
		"aňţärko'lá'rš",
		"ažxwö'rka'súm",
		"rtawuihážžg",
		"uňňsozahé'kšš",
		"ňvailoţmá'gzz",
		// Render emits a doubled glottal in the Vc.
		"hrarráu-wäḑxë'iza'o",
		"huţrilú-ujjäli'a",
		// The Slot V end-of-slot glottal lands on the wrong affix.
		"a'ajthuiňřoirguoẓẓwa",
		"a'ļfažíravva",
		"alçpi'lä-abzgauçmaussä",
		"arštilsau'lualla",
		"žfaléa'ha'rš",
		// A spurious glottal upgrades the case by 36 (IND u -> RLT u'u,
		// EFF ö -> UTL ö'ö, CSD ua -> NAV u'a, PAR ui -> VOC u'i).
		"idřoutasyövvu",
		"mcialorjiřřö",
		"häsuandävussahniä",
		"hasäza-wappļaţsö",
		"hmuksküţmurbâ-a'rkwau'zwëillikbiažřui",
	} {
		roundTrips(t, w)
	}
}

// TestCorpus_ConcatenatedChains covers concatenated formatives whose
// slots come back shifted by one. Every case starts with a Type-1 or
// Type-2 Cc that also carries the Slot IV/VI shortcut (hl-, hr-, hm-).
func TestCorpus_ConcatenatedChains(t *testing.T) {
	t.Skip("known defect: see the concatenation group in the corpus audit")
	for _, w := range []string{
		"hlabzëicdú-afçnizyuëlla",
		"hlabzřëicdú-afçnizyuëlla",
		"hlarrëicdú-afçnizyuëlla",
		"hluňtyí-ukţgwanţëull",
		"hlainglöšdí-abvlä'löḑbu",
		"hriamžé-akbiçňuivva",
		"hroisvé-maţřëujja",
		"hropšmyí-okšňafřuilli",
	} {
		roundTrips(t, w)
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
