package phonology

import "testing"

// Cluster examples lifted from docs/reference/phonotactics.md. Each
// case names the section it came from, so a failure points back at the
// spec text it covers.
//
// The permitted cases carry the weight here. Every validator defect
// found so far has been over-rejection — mp and mb under §2.12, çç and
// ļļ under a rule that turned out not to exist — and in each one the
// document named the permitted forms within a few words of the
// prohibition. A rule read as "m
// cannot precede a labial stop" and a rule read as "m + labial stop +
// fricative is indistinct from m + fricative" reject exactly the same
// starred examples. Only the contrasting permitted forms tell the two
// readings apart, so wherever the document supplies that contrast it
// is recorded here beside the prohibition.
//
// Position is part of the case. §2 opens by saying its restrictions
// hold "whether within the same syllable or across adjacent
// syllables", so those are Medial; §3 governs word-initial forms and
// §4 word-final ones, and several clusters are legal in one position
// and not in another.

type clusterCase struct {
	section string
	pos     Position
	cluster string
	legal   bool
}

// section2 covers the general prohibitions, which hold in any
// position. Every numbered rule from §2.2 to §2.22 appears, plus the
// two constraints §8's table carries that §2's prose no longer states
// (G44 in ISSUES.md).
var section2 = []clusterCase{
	// §2.2 dental stop + any sibilant, and + its own fricative
	// counterpart.
	{"2.2", Medial, "tţ", false},
	{"2.2", Medial, "dţ", false},
	{"2.2", Medial, "tḑ", false},
	{"2.2", Medial, "dḑ", false},
	{"2.2", Medial, "ts", false},
	{"2.2", Medial, "tš", false},

	// §2.3 velar stop + x or ň.
	{"2.3", Medial, "kx", false},
	{"2.3", Medial, "gx", false},
	{"2.3", Medial, "kň", false},
	{"2.3", Medial, "gň", false},

	// §2.4 homologous stops disagreeing in voicing, in both orders.
	// "Non-homologous pairings are permitted, e.g., kd, pd, gp, tg,
	// pg, dk."
	{"2.4", Medial, "kg", false},
	{"2.4", Medial, "td", false},
	{"2.4", Medial, "pb", false},
	{"2.4", Medial, "gk", false},
	{"2.4", Medial, "dt", false},
	{"2.4", Medial, "bp", false},
	{"2.4", Medial, "kd", true},
	{"2.4", Medial, "pd", true},
	{"2.4", Medial, "gp", true},
	{"2.4", Medial, "tg", true},
	{"2.4", Medial, "pg", true},
	{"2.4", Medial, "dk", true},

	// §2.5 the same restriction "applies to fricatives and to
	// affricates" — not to the sibilants alone, which is why fv and ţḑ
	// are on the list.
	{"2.5", Medial, "fv", false},
	{"2.5", Medial, "vf", false},
	{"2.5", Medial, "ţḑ", false},
	{"2.5", Medial, "ḑţ", false},
	{"2.5", Medial, "cẓ", false},
	{"2.5", Medial, "ẓc", false},
	{"2.5", Medial, "čj", false},
	{"2.5", Medial, "jč", false},
	// Non-homologous affricates pair "if the first is alveolar and the
	// second alveolo-palatal ... but not the reverse". Stated as an
	// unordered restriction, a rule gets all eight of these wrong.
	{"2.5", Medial, "cč", true},
	{"2.5", Medial, "cj", true},
	{"2.5", Medial, "ẓč", true},
	{"2.5", Medial, "ẓj", true},
	// The rule applies "to fricatives and to affricates" — each
	// manner to itself. Every example it gives pairs like with like,
	// and reading it across the two manners rejects live roots: zc
	// 'chop/dice/mince', šj, žč, and the §6.2.2 word arţtudëužči'a.
	{"2.5", Medial, "zc", true},
	{"2.5", Medial, "šj", true},
	{"2.5", Medial, "žč", true},
	{"2.5", Medial, "sj", true},
	{"2.5", Medial, "čc", false},
	{"2.5", Medial, "jc", false},
	{"2.5", Medial, "čẓ", false},
	{"2.5", Medial, "jẓ", false},

	// §2.6 alveolo-palatal fricative + apico-alveolar affricate.
	{"2.6", Medial, "šc", false},
	{"2.6", Medial, "šẓ", false},
	{"2.6", Medial, "žc", false},
	{"2.6", Medial, "žẓ", false},

	// §2.7 s + the voiced affricate ẓ.
	{"2.7", Medial, "sẓ", false},

	// §2.8 any conjunction of sibilant fricatives, "other than
	// geminates".
	{"2.8", Medial, "sz", false},
	{"2.8", Medial, "žs", false},
	{"2.8", Medial, "šs", false},
	{"2.8", Medial, "ss", true},

	// §2.9 sibilant affricate + sibilant fricative.
	{"2.9", Medial, "čs", false},
	{"2.9", Medial, "cz", false},
	{"2.9", Medial, "ẓz", false},
	{"2.9", Medial, "čž", false},
	{"2.9", Medial, "js", false},
	{"2.9", Medial, "jz", false},
	{"2.9", Medial, "jš", false},
	// One direction only: every starred form has the affricate first.
	// The reverse order is ordinary vocabulary — sc 'wash/bathe',
	// zc 'chop/dice', šč and žč — and shows up in weščayá and
	// žžjádu'u. §2.6 and §2.7 are what bar the reverse order, for the
	// alveolo-palatal fricatives and for s + ẓ specifically.
	{"2.9", Medial, "sc", true},
	{"2.9", Medial, "sč", true},
	{"2.9", Medial, "zč", true},
	{"2.9", Medial, "šč", true},
	{"2.9", Medial, "zj", true},

	// §2.10 the ç restrictions, which run in both directions.
	{"2.10", Medial, "sç", false},
	{"2.10", Medial, "çs", false},
	{"2.10", Medial, "cç", false},
	{"2.10", Medial, "çẓ", false},
	{"2.10", Medial, "çļ", false},
	{"2.10", Medial, "ļç", false},
	{"2.10", Medial, "çh", false},
	{"2.10", Medial, "hç", false},
	{"2.10", Medial, "xç", false},

	// §2.11 nasal + affricate of the same or similar place. The rule
	// exists because these are indistinct from the nasal + fricative
	// forms, which are themselves permitted.
	{"2.11", Medial, "nc", false},
	{"2.11", Medial, "nč", false},
	{"2.11", Medial, "nẓ", false},
	{"2.11", Medial, "nj", false},
	{"2.11", Medial, "ns", true},
	{"2.11", Medial, "nš", true},
	{"2.11", Medial, "nz", true},
	{"2.11", Medial, "nž", true},

	// §2.12 is about a triple: m + bilabial stop + a bilabial or
	// interdental fricative or dental stop. The parenthetical "(vs.
	// mf, mv, mţ, mḑ, mt, md)" names the two-consonant forms those
	// triples collapse onto, and they are permitted. mp and mb carry
	// the MSF configuration of the Ca complex.
	{"2.12", Medial, "mpf", false},
	{"2.12", Medial, "mbv", false},
	{"2.12", Medial, "mbḑ", false},
	{"2.12", Medial, "mpţ", false},
	{"2.12", Medial, "mbd", false},
	{"2.12", Medial, "ngḑ", false},
	{"2.12", Medial, "mf", true},
	{"2.12", Medial, "mv", true},
	{"2.12", Medial, "mţ", true},
	{"2.12", Medial, "mḑ", true},
	{"2.12", Medial, "mt", true},
	{"2.12", Medial, "md", true},
	{"2.12", Medial, "mp", true},
	{"2.12", Medial, "mb", true},
	{"2.12", Medial, "nkţ", true},

	// §2.13 nasal + homologous stop + sibilant. nks and ngz are on the
	// list because §1.2.2 assimilates n to velar [ŋ] before k and g,
	// so they are the same sounds as ňks and ňgz. The bracketed forms
	// are the permitted collapses.
	{"2.13", Medial, "mps", false},
	{"2.13", Medial, "mbz", false},
	{"2.13", Medial, "nks", false},
	{"2.13", Medial, "ngz", false},
	{"2.13", Medial, "mpš", false},
	{"2.13", Medial, "mbž", false},
	{"2.13", Medial, "nkš", false},
	{"2.13", Medial, "ngž", false},
	{"2.13", Medial, "ňs", true},
	{"2.13", Medial, "ňš", true},
	{"2.13", Medial, "ňž", true},

	// §2.14 n + labial stop, which would assimilate to mp/mb.
	{"2.14", Medial, "np", false},
	{"2.14", Medial, "nb", false},

	// §2.15 nf and nv "must be followed by a vowel-form".
	{"2.15", Medial, "nfk", false},
	{"2.15", Medial, "nvd", false},
	{"2.15", Medial, "nf", true},
	{"2.15", Medial, "nv", true},

	// §2.16 ň before a velar or uvular, and before y. "ňř is
	// permitted, since n does not assimilate to velar [ŋ] before ř."
	{"2.16", Medial, "ňk", false},
	{"2.16", Medial, "ňg", false},
	{"2.16", Medial, "ňx", false},
	{"2.16", Medial, "ňy", false},
	{"2.16", Medial, "ňř", true},

	// §2.17 what x cannot precede.
	{"2.17", Medial, "xs", false},
	{"2.17", Medial, "xg", false},
	{"2.17", Medial, "xļ", false},
	{"2.17", Medial, "xň", false},
	{"2.17", Medial, "xy", false},
	{"2.17", Medial, "xř", false},
	{"2.17", Medial, "xp", true},
	{"2.17", Medial, "xt", true},
	{"2.17", Medial, "xm", true},
	// The rule names the four sibilant fricatives and stops there.
	// The affricates are not on its list, and xc 'equine', xč
	// 'murder', xj 'tapir' and xẓ 'dusty' are all live roots. §3.2.3
	// even makes xc- and xč- permissible word-initially.
	{"2.17", Medial, "xc", true},
	{"2.17", Medial, "xč", true},
	{"2.17", Medial, "xj", true},
	{"2.17", Medial, "xẓ", true},

	// §2.18 ļ cannot follow a voiced stop, h or ç; and cannot precede
	// a sibilant fricative, h or ç.
	{"2.18", Medial, "dļ", false},
	{"2.18", Medial, "gļ", false},
	{"2.18", Medial, "bļ", false},
	{"2.18", Medial, "ļs", false},
	{"2.18", Medial, "ļš", false},
	{"2.18", Medial, "ļk", true},
	{"2.18", Medial, "ļp", true},

	// §2.19 h as the final member of a conjunct.
	{"2.19", Medial, "ļh", false},
	{"2.19", Medial, "xh", false},

	// §2.20, §2.21 the r/ř restrictions.
	{"2.20", Medial, "rř", false},
	{"2.20", Medial, "hř", false},
	{"2.21", Medial, "řr", false},

	// §2.22 w and y "can only appear as the last member of the
	// conjunct".
	{"2.22", Medial, "wp", false},
	{"2.22", Medial, "yp", false},
	{"2.22", Medial, "lw", true},
	{"2.22", Medial, "ly", true},

	// The two constraints that moved out of §2's prose in the v0.5.0
	// renumbering and are carried by §8's table alone. Prose rules
	// §2.6 and §2.15 in v0.3 and v0.4.
	{"8", Medial, "ḑs", false},
	{"8", Medial, "ḑš", false},
	{"8", Medial, "ḑz", false},
	{"8", Medial, "ḑž", false},
	{"8", Medial, "nň", false},
}

// derived covers clusters the morphology itself builds. They are not
// §2's subject and no §2 rule reaches them, but a validator that
// over-rejects breaks the words that carry them, so the guard belongs
// with the section that generates them.
var derived = []clusterCase{
	// §3.6.1 rule 4 geminates a sibilant "in any position" and gives
	// çkl → ççkl as its own worked example; rule 6 gives tçkl → tççkl.
	// The bias-adjunct table holds pļļ (CMD) and kçç (EXA), and 39
	// corpus words use one or the other, among them formatives whose
	// geminated Ca marks the end of Slot V: wiapļļalká,
	// hamphelsuirççaité. Our markdown once carried a "§2.24" barring
	// both; no version of the phonotaxis states it (G1, G44 in
	// ISSUES.md).
	{"3.6.1", Medial, "çç", true},
	{"3.6.1", Medial, "ļļ", true},
}

// section34 covers the position-specific rules — the cases a
// single-position sweep cannot state. pm and dm are barred at the head
// of a word and unremarkable inside one; w is barred at the end of a
// word and ordinary elsewhere.
var section34 = []clusterCase{
	// §3.1 any single consonant may begin a word except ļ, which is
	// indistinguishable from the allophone of word-initial hl-.
	{"3.1", Initial, "ļ", false},
	{"3.1", Initial, "p", true},
	{"3.1", Initial, "m", true},
	{"3.1", Initial, "ř", true},

	// §3.2 a stop may take a sibilant fricative of the same voicing,
	// "e.g., ps, gz, kš, bž, etc. but not *pz, *gs, *kž".
	{"3.2", Initial, "ps", true},
	{"3.2", Initial, "gz", true},
	{"3.2", Initial, "kš", true},
	{"3.2", Initial, "pz", false},
	{"3.2", Initial, "gs", false},
	{"3.2", Initial, "kž", false},
	// kļ is called out as an exception, "phonetically too
	// indistinguishable in normal speech from tļ".
	{"3.2", Initial, "tļ", true},
	{"3.2", Initial, "kļ", false},
	// "In word-initial position, bilabial and dental stops cannot be
	// followed by a nasal ... however velar stops may be followed by
	// -m or -n." The same clusters are fine medially.
	{"3.2", Initial, "pm", false},
	{"3.2", Initial, "bn", false},
	{"3.2", Initial, "tn", false},
	{"3.2", Initial, "dm", false},
	{"3.2", Initial, "km", true},
	{"3.2", Initial, "gn", true},
	{"3.2", Medial, "pm", true},
	{"3.2", Medial, "dm", true},

	// §4.1 a single word-final consonant "may be any single consonant
	// except -w or -y".
	{"4.1", Final, "w", false},
	{"4.1", Final, "y", false},
	{"4.1", Final, "t", true},
	{"4.1", Final, "r", true},
	{"4.1", Final, "ň", true},

	// §6 governs geminates, and §3 says so in its opening line: its
	// rules run "not including rules for geminated forms — see Sec.
	// 6". §6.2 lets any intervocalically-geminable consonant geminate
	// word-initially, stops excepted. That is what rrala 'cat',
	// sstilomke, vvralá and žžjádu'u all rely on.
	{"6.2", Initial, "rr", true},
	{"6.2", Initial, "ll", true},
	{"6.2", Initial, "ss", true},
	{"6.2", Initial, "žž", true},
	{"6.2", Initial, "mm", true},
	{"6.2", Initial, "pp", false},
	{"6.2", Initial, "tt", false},
	{"6.2", Initial, "gg", false},
	// §6.3.1 #C₁C₁C₂-: legal when #C₁C₂- is. sst- holds because st-
	// does; ssm- does not, because sm- is not a permissible pair.
	{"6.3.1", Initial, "sst", true},
	{"6.3.1", Initial, "rrl", false},
	// §6.4.1 #C₁C₂C₂-: legal when #C₁C₂- is. mřř- holds because mř-
	// does.
	{"6.4.1", Initial, "mřř", true},
	{"6.4.1", Initial, "vvr", true},
	{"6.4.1", Initial, "žžj", true},
	// §6.5 only one geminate pair per conjunct.
	{"6.5", Initial, "rrll", false},
}

func runClusterCases(t *testing.T, cases []clusterCase) {
	t.Helper()
	for _, c := range cases {
		r := ClusterViolationsAt(c.pos, c.cluster)
		if c.legal && len(r) > 0 {
			t.Errorf("§%s: %q should be legal %v, rejected: %v",
				c.section, c.cluster, c.pos, r)
		}
		if !c.legal && len(r) == 0 {
			t.Errorf("§%s: %q should be rejected %v, but validates",
				c.section, c.cluster, c.pos)
		}
	}
}

func TestSpec_GeneralProhibitions(t *testing.T) { runClusterCases(t, section2) }

func TestSpec_PositionalForms(t *testing.T) { runClusterCases(t, section34) }

func TestSpec_DerivedClusters(t *testing.T) { runClusterCases(t, derived) }
