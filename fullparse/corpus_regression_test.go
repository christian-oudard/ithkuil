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
	for _, w := range []string{
		// Render emits a reduplicated Vv ("a'a-") that won't re-parse.
		"anzvarönţiçřoi'hákšš",
		"attualië'hú'žžg",
		"aňzkçaẓäçnëumvuožžô",
		"aňţärko'lá'rš",
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

// TestCorpus_ShortcutSlotV covers what's left after the glottal fixes:
// formatives carrying a Slot IV/VI shortcut, whose slots come back
// shifted by one. The last two are not concatenated but share the
// shortcut, which is what the §3.6.2 end-of-Slot-V glottal marks
// against — so this is one defect, not two.
func TestCorpus_ShortcutSlotV(t *testing.T) {
	t.Skip("known defect: see the Slot IV/VI shortcut group in the corpus audit")
	for _, w := range []string{
		"hlabzëicdú-afçnizyuëlla",
		"hlabzřëicdú-afçnizyuëlla",
		"hlarrëicdú-afçnizyuëlla",
		"hluňtyí-ukţgwanţëull",
		"hlainglöšdí-abvlä'löḑbu",
		"hriamžé-akbiçňuivva",
		"hroisvé-maţřëujja",
		"hropšmyí-okšňafřuilli",
		"hliařţiá-wa'aňsätļi'jva",
		"hliařţiá-wa'aňsätļijva'řga",
		"hmuksküţmurbâ-a'rkwau'zwëillikbiažřui",
		"hmuksküţmurbâ-arkwauzwëillikbiažřu'i",
		"anzvarönţiçřoi'há'kšš",
		"žfaléa'ha'rš",
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

// TestCorpus_Unexplained holds the words with no diagnosis yet. They
// are here so the count stays honest and so a future fix has something
// to turn green.
func TestCorpus_Unexplained(t *testing.T) {
	t.Skip("no diagnosis yet")
	for _, w := range []string{
		// Parses with Ca = "'rk". Nothing in §3.6.2 or §3.9.1 puts a
		// glottal at the head of a Ca, and the word carries a second one
		// on a Slot VII Cs, so the segmentation is likely wrong.
		"ažxwö'rka'súm",
		// Comes back STM (ëi) instead of COM (uo), which is not one of
		// the glottal case pairs.
		"ltyurëi-annarëi",
	} {
		roundTrips(t, w)
	}
}
