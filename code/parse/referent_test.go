package parse

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// refTable is the §4.6 referent table, transcribed from morphology.md
// rather than from c1Table, so that a wrong entry in the code cannot
// make the test agree with it.
var refTable = []struct {
	referent            g.Referent
	neu, ben, det       string
	altNeu, altBen, alt string // §4.6.5-only alternates, "" when none
}{
	{referent: g.R1m, neu: "l", ben: "r", det: "ř"},
	{referent: g.R2m, neu: "s", ben: "š", det: "ž"},
	{referent: g.R2p, neu: "n", ben: "t", det: "d"},
	{referent: g.Rma, neu: "m", ben: "p", det: "b"},
	{referent: g.Rpa, neu: "ň", ben: "k", det: "g"},
	{referent: g.Rmi, neu: "z", ben: "ţ", det: "ḑ"},
	{referent: g.Rpi, neu: "ẓ", ben: "f", det: "v"},
	{referent: g.Rmx, neu: "c", ben: "č", det: "j"},
	{referent: g.Rrdp, neu: "th", ben: "ph", det: "kh"},
	{referent: g.Robv, neu: "ll", ben: "rr", det: "řř",
		altNeu: "lç", altBen: "rç", alt: "řç"},
	{referent: g.Rpvs, neu: "mm", ben: "nn", det: "ňň",
		altNeu: "mç", altBen: "nç", alt: "ňç"},
}

func TestRefC1_MatchesSourceTable(t *testing.T) {
	for _, row := range refTable {
		for _, c := range []struct {
			effect g.RefEffect
			form   string
		}{
			{g.NEU, row.neu}, {g.BEN, row.ben}, {g.DET, row.det},
		} {
			ref := g.PersonalRef{Referent: row.referent, Effect: c.effect}
			if got := RefC1(ref); got != c.form {
				t.Errorf("RefC1(%v/%v) = %q, want %q", row.referent, c.effect, got, c.form)
			}
			got, ok := DecomposeRefCluster(c.form)
			if !ok || len(got) != 1 || got[0] != ref {
				t.Errorf("DecomposeRefCluster(%q) = %v, %v; want [%v]", c.form, got, ok, ref)
			}
		}
	}
}

// The §4.6 footnote confines the Obv/PVS alternates to §4.6.5
// referential affixes, "to avoid ambiguity with geminated C_A forms".
// Outside that position they must not decode as referents, because
// they collide with the Nomic category affix: "lç" as a referential
// word is 1m with the NOMIC modifier, not Obv/NEU.
func TestRefAlternates_OnlyInAffixPosition(t *testing.T) {
	for _, row := range refTable {
		if row.altNeu == "" {
			continue
		}
		for _, c := range []struct {
			effect g.RefEffect
			form   string
		}{
			{g.NEU, row.altNeu}, {g.BEN, row.altBen}, {g.DET, row.alt},
		} {
			want := g.PersonalRef{Referent: row.referent, Effect: c.effect}
			got, ok := DecomposeRefAffixCs(c.form)
			if !ok || len(got) != 1 || got[0] != want {
				t.Errorf("DecomposeRefAffixCs(%q) = %v, %v; want [%v]", c.form, got, ok, want)
			}
			if _, ok := DecomposeRefCluster(c.form); ok {
				t.Errorf("DecomposeRefCluster(%q) decoded an affix-only alternate form", c.form)
			}
		}
	}
}

// "lç" is the one form that reads differently in the two positions.
func TestLc_ReadsAsNomicOutsideAffixPosition(t *testing.T) {
	cat, refs, ok := DecomposeRefWithCategory("lç")
	if !ok {
		t.Fatal(`DecomposeRefWithCategory("lç") failed`)
	}
	if cat == nil || *cat != g.Nomic {
		t.Errorf("category = %v, want NOM", cat)
	}
	if len(refs) != 1 || refs[0].Referent != g.R1m {
		t.Errorf("refs = %v, want [1m]", refs)
	}
}

// "ļ" is the Agglomerative affix in §4.6, not a referent form. The
// table gives pi/NEU as "ẓ" and lists no alternate for it.
func TestLetterL_IsNotAReferent(t *testing.T) {
	if p, ok := LookupRefC1("ļ"); ok {
		t.Errorf(`LookupRefC1("ļ") = %v; "ļ" is the AGM affix, not a referent`, p)
	}
	cat, refs, ok := DecomposeRefWithCategory("ļl")
	if !ok {
		t.Fatal(`DecomposeRefWithCategory("ļl") failed`)
	}
	if cat == nil || *cat != g.Agglomerative {
		t.Errorf("category = %v, want AGM", cat)
	}
	if len(refs) != 1 || refs[0].Referent != g.R1m {
		t.Errorf("refs = %v, want [1m]", refs)
	}
}

// §4.6 writes Agglomerative and Nomic hyphenated on both sides
// ("-ļ-", "-ç-"), its notation for an affix that may precede or
// follow, but writes Abstract with a leading hyphen only ("-w", "-y").
func TestRefCategoryForms_PlacementFollowsSourceNotation(t *testing.T) {
	want := map[string]struct{ prefix, suffix bool }{
		"ļ": {true, true}, "tļ": {true, true},
		"ç": {true, true}, "x": {true, true},
		"w": {false, true}, "y": {false, true},
	}
	if len(RefCategoryForms) != len(want) {
		t.Fatalf("RefCategoryForms has %d entries, want %d", len(RefCategoryForms), len(want))
	}
	for _, f := range RefCategoryForms {
		w, ok := want[f.Form]
		if !ok {
			t.Errorf("unexpected category form %q", f.Form)
			continue
		}
		if f.Prefix != w.prefix || f.Suffix != w.suffix {
			t.Errorf("%q: prefix=%v suffix=%v, want prefix=%v suffix=%v",
				f.Form, f.Prefix, f.Suffix, w.prefix, w.suffix)
		}
	}
}

func TestDecomposeRefCluster_Chain(t *testing.T) {
	// "sml" is §4.6.1's own example: 'you (sg.) and (s)he and I'.
	got, ok := DecomposeRefCluster("sml")
	if !ok {
		t.Fatal(`DecomposeRefCluster("sml") failed`)
	}
	want := []g.Referent{g.R2m, g.Rma, g.R1m}
	if len(got) != len(want) {
		t.Fatalf("got %v, want %v", got, want)
	}
	for i, w := range want {
		if got[i].Referent != w {
			t.Errorf("position %d: got %v, want %v", i, got[i].Referent, w)
		}
	}
}

// A biconsonantal form wins over two monoconsonantal reads: "ll" is
// Obv/NEU, not 1m+1m.
func TestDecomposeRefCluster_BiconsonantalWins(t *testing.T) {
	got, ok := DecomposeRefCluster("ll")
	if !ok || len(got) != 1 || got[0].Referent != g.Robv {
		t.Errorf(`DecomposeRefCluster("ll") = %v, %v; want [Obv/NEU]`, got, ok)
	}
}

func TestDecomposeRefCluster_Invalid(t *testing.T) {
	for _, s := range []string{"q", "lq", ""} {
		if got, ok := DecomposeRefCluster(s); ok && len(got) > 0 {
			t.Errorf("DecomposeRefCluster(%q) = %v, want failure", s, got)
		}
	}
}

func TestReferentLabels(t *testing.T) {
	for _, r := range g.AllReferents {
		if r.String() == "" || r.Label() == "" {
			t.Errorf("referent %d has an empty abbreviation or label", r)
		}
	}
}
