package render

import (
	"strings"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

func TestFormative_Minimal(t *testing.T) {
	// MinimalFormative("ml"): I=∅, II=S1/PRC ("a"), III="ml", IV=STA/BSC/EXS ("a"),
	// V=[], VI=default ("l"), VII=[], VIII=nil, IX=CaseSlot{THM} ("a").
	// Default Vv elides per §3.2 → short form "wamla".
	f := g.MinimalFormative("ml")
	got := Formative(f)
	want := "wamla"
	if got != want {
		t.Errorf("Formative(minimal \"ml\") = %q, want %q", got, want)
	}
}

func TestFormative_NonDefaultSlots(t *testing.T) {
	// Build a formative with several non-default slots and confirm the
	// rendering composes correctly.
	f := g.MinimalFormative("ml")
	f.Root = g.CrRoot{
		Cluster: "ml",
		Stem:    g.S2, Version: g.PRC,
		SlotIV: g.SlotIV{Function: g.DYN, Specification: g.OBJ, Context: g.EXS},
	}
	f.Final = g.UnframedNominal{Case: g.ERG} // Vc = "o"

	got := Formative(f)
	// Vv=e, Cr=ml, Vr=DYN+OBJ → form 6 series 1 (EXS) = "ö", Ca="l", Vc="o"
	want := "emlölo"
	if got != want {
		t.Errorf("Formative(non-default) = %q, want %q", got, want)
	}
}

func TestFormative_Ultimate(t *testing.T) {
	// Verbal formative: Final is UnframedVerbal{Assertive{INF}}.
	// Ultimate marks the last vowel; default Vv elides → "wamlú".
	f := g.MinimalFormative("ml")
	f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.INF}}
	got := Formative(f)
	want := "wamlú"
	if got != want {
		t.Errorf("Formative(verbal) = %q, want %q", got, want)
	}
}

func TestFormative_WithSlotVIII(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SlotVIII = g.VnCnValence{
		Valence:   g.MNO,
		MoodScope: g.SUB,
	}
	got := Formative(f)
	// §3.8.1.2 Cn→Ca shortcut: default Ca and MNO+non-FAC Pattern-1 Cn
	// collapse the Vn and substitute the Cn for Ca. After the shortcut
	// the body is a-ml-a-hl-a (3 vowels), PEN needs 2, slack 1 → only
	// the default Vv elides, leaving "mlahla".
	want := "mlahla"
	if got != want {
		t.Errorf("Formative(with VIII) = %q, want %q", got, want)
	}
}

func TestFormative_MovedGlottal_Cases37To52(t *testing.T) {
	// §3.9.1 SPECIAL NOTE shifts the Vc glottal-stop for cases 37-52
	// onto an earlier vocalic form. The canonical landing spot is just
	// after Vr, producing "mla'l<Vc>" for a MinimalFormative("ml").
	cases := []struct {
		c    g.Case
		want string
	}{
		// Relational (37-44): Series 1 + glottal.
		{g.PRN, "mla'la"},
		{g.DSP, "mla'lä"},
		{g.COR, "mla'le"},
		{g.CPS, "mla'li"},
		{g.COM, "mla'lëi"},
		{g.UTL, "mla'lö"},
		{g.PRD, "mla'lo"},
		{g.RLT, "mla'lu"},
		// Affinitive (45-52): Series 2 + glottal.
		{g.ACT, "mla'lai"},
		{g.ASI, "mla'lau"},
		{g.ESS, "mla'lei"},
		{g.TRM, "mla'leu"},
		{g.SEL, "mla'lëu"},
		{g.CFM, "mla'lou"},
		{g.DEP, "mla'loi"},
		{g.VOC, "mla'lui"},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.Final = g.UnframedNominal{Case: c.c}
		got := Formative(f)
		if got != c.want {
			t.Errorf("Formative(%v) = %q, want %q", c.c, got, c.want)
		}
	}
}

func TestFormative_NoMovedGlottal_OtherCases(t *testing.T) {
	// Cases 1-36 and 53-68 must NOT have the glottal moved. (53-68 are
	// Spatio-Temporal; their canonical Vc keeps the glottal in place.)
	for _, c := range []g.Case{g.THM, g.INS, g.ABS, g.STM, g.ERG, g.SIT, g.LOC, g.PER} {
		f := g.MinimalFormative("ml")
		f.Final = g.UnframedNominal{Case: c}
		got := Formative(f)
		// We don't assert the exact form, just that there is no
		// leading "'" on a Cs/Ca position (the visible signature of
		// the moved-glottal short form).
		if strings.Contains(got, "a'l") {
			t.Errorf("Formative(%v) = %q has moved-glottal pattern", c, got)
		}
	}
}

func TestFormative_CnCaShortcut_AllVariants(t *testing.T) {
	// All five Pattern-1 non-FAC Cn variants produce the canonical
	// short-form surface when paired with default Ca and MNO Valence.
	cases := []struct {
		mood g.Mood
		want string
	}{
		{g.SUB, "mlahla"},
		{g.ASM, "mlahra"},
		{g.SPC, "mlahma"},
		{g.COU, "mlahna"},
		{g.HYP, "mlahňa"},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		f.SlotVIII = g.VnCnValence{Valence: g.MNO, MoodScope: c.mood}
		got := Formative(f)
		if got != c.want {
			t.Errorf("Formative(MNO/%v) = %q, want %q", c.mood, got, c.want)
		}
	}
}

func TestFormative_WithAffixes(t *testing.T) {
	f := g.MinimalFormative("ml")
	// Slot VII affix: vowel=a, cons=r → "ar"
	f.SlotVII = []g.Affix{{Type: g.Type1Affix, Degree: 1, Consonant: "r"}}
	got := Formative(f)
	// Long: a-ml-a-l-ar-a. Both default Vv and THM Vc elide → "wamlar".
	want := "wamlar"
	if got != want {
		t.Errorf("Formative(with affix) = %q, want %q", got, want)
	}
}

func TestFormative_THMVcElision_KeepsForMinimal(t *testing.T) {
	// MinimalFormative("ml") has 3 syllables in long form (a-ml-a-l-a).
	// PEN needs 2; slack is 1; only one elision fits. Prefer Vv.
	f := g.MinimalFormative("ml")
	got := Formative(f)
	want := "wamla"
	if got != want {
		t.Errorf("Formative(minimal) = %q, want %q", got, want)
	}
}

func TestFormative_AssertiveOBS_MonosyllabicElision(t *testing.T) {
	// §3.10: a monosyllabic body carries implicit ultimate stress with
	// no diacritic. The Assertive/OBS Vk vowel "a" elides whenever the
	// resulting body is monosyllabic — the parser round-trips it via
	// finalFromVc's monosyllabic branch back to Assertive/OBS.
	f := g.MinimalFormative("ml")
	f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
	got := Formative(f)
	want := "waml"
	if got != want {
		t.Errorf("Formative(ultimate-ASR-OBS) = %q, want %q", got, want)
	}
}

func TestFormative_ShortForm_AntepenultKeepsLong(t *testing.T) {
	// Antepenult stress needs ≥3 syllables. Eliding the leading "a" of a
	// 3-syllable long form would leave 2 syllables, breaking the stress.
	// The renderer must keep the long form so the diacritic can land.
	f := g.MinimalFormative("ml")
	f.Final = g.FramedVerbal{Case: g.THM}
	got := Formative(f)
	want := "ámlala"
	if got != want {
		t.Errorf("Formative(antepenult) = %q, want %q", got, want)
	}
}

func TestFormative_ShortForm_NonDefaultVvNoElision(t *testing.T) {
	// Non-default Stem (S2/PRC = "e") never elides; the THM Vc still does.
	f := g.MinimalFormative("ml")
	cr := f.Root.(g.CrRoot)
	cr.Stem = g.S2
	f.Root = cr
	got := Formative(f)
	want := "wemla"
	if got != want {
		t.Errorf("Formative(S2/PRC) = %q, want %q", got, want)
	}
}

func TestFormative_ShortForm_WithSlotI_NoElision(t *testing.T) {
	// Slot I prefix locks the formative into long form for Vv; THM Vc
	// still elides.
	f := g.MinimalFormative("ml")
	t1 := g.Type1
	f.Concat = t1
	got := Formative(f)
	want := "hlamla"
	if got != want {
		t.Errorf("Formative(T1) = %q, want %q", got, want)
	}
}

func TestFormative_ANTPaddingNilSlotIX(t *testing.T) {
	// A Formative with empty Slot IX content (FramedVerbal{THM}) renders
	// Vv+Cr+Vr+Ca = 2 syllables (a-m-a-l). ANT stress needs 3; §5.8.8
	// says pad with default Slot IX → append "a", yielding "amala",
	// then mark ANT → "ámala".
	f := g.Formative{
		Root:   g.DefaultCrRoot("m"),
		SlotVI: g.DefaultSlotVI,
		Final:  g.FramedVerbal{Case: g.THM},
	}
	got := Formative(f)
	want := "ámala"
	if got != want {
		t.Errorf("Formative(ANT, nil SlotIX) = %q, want %q", got, want)
	}
}

func TestFormative_ShortcutW_Series1(t *testing.T) {
	f := g.MinimalFormative("ml")
	got := Formative(f)
	want := "wamla"
	if got != want {
		t.Errorf("Formative(W shortcut series 1) = %q, want %q", got, want)
	}
}

// TestFormative_ShortcutOnlyWhenItShortens pins the rule that decides
// between the §3.2 Cc shortcut and the plain form. The shortcut exists
// to shorten the word by a syllable, and it can only do that when the
// plain form would have to spell out Vv. With default Stem/Version the
// leading Vv elides anyway, so the shortcut buys nothing and the plain
// form wins.
func TestFormative_ShortcutOnlyWhenItShortens(t *testing.T) {
	cases := []struct {
		name  string
		stem  g.Stem
		persp g.Perspective
		kase  g.Case
		want  string
	}{
		// Vv is default and elides, so both forms are two syllables and
		// the same length. The tie goes to the shortcut.
		{"default Vv, series 1", g.S1, g.M_, g.THM, "wamla"},
		// Same tie, but series 2 spells its Ca as a single -r-, making
		// the plain form a rune shorter than "waimla".
		{"default Vv, series 2", g.S1, g.G_, g.THM, "mlara"},
		// Non-default Vv can't elide, so the shortcut saves a syllable.
		{"non-default Vv, series 1", g.S2, g.M_, g.THM, "wemla"},
		{"non-default Vv, series 2", g.S2, g.G_, g.ERG, "weimlo"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			f := g.MinimalFormative("ml")
			cr := f.Root.(g.CrRoot)
			cr.Stem = c.stem
			f.Root = cr
			f.SlotVI = g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL,
				Perspective: c.persp, Extension: g.DEL, Essence: g.NRM}
			f.Final = g.UnframedNominal{Case: c.kase}
			if got := Formative(f); got != c.want {
				t.Errorf("Formative = %q, want %q", got, c.want)
			}
		})
	}
}

func TestFormative_ShortcutY_Series1(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SlotVI = g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL,
		Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}
	got := Formative(f)
	want := "yamla"
	if got != want {
		t.Errorf("Formative(Y shortcut series 1) = %q, want %q", got, want)
	}
}

func TestFormative_ShortcutWithConcat(t *testing.T) {
	f := g.MinimalFormative("ml")
	t1 := g.Type1
	f.Concat = t1
	got := Formative(f)
	want := "hlamla"
	if got != want {
		t.Errorf("Formative(T1+W shortcut) = %q, want %q", got, want)
	}
}
