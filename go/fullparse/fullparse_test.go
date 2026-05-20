package fullparse

import (
	"testing"

	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/render"
)

func TestParseFormative_Minimal(t *testing.T) {
	// "amlala" is the rendering of MinimalFormative("ml").
	f, err := ParseFormative("amlala")
	if err != nil {
		t.Fatalf("ParseFormative(\"amlala\") error: %v", err)
	}
	cr, ok := f.Root.(g.CrRoot)
	if !ok {
		t.Fatalf("Root = %v, want CrRoot", f.Root)
	}
	if cr.Cluster != "ml" {
		t.Errorf("Cluster = %q, want %q", cr.Cluster, "ml")
	}
	if cr.Stem != g.S1 || cr.Version != g.PRC {
		t.Errorf("Stem/Version = %v/%v, want S1/PRC", cr.Stem, cr.Version)
	}
	if cr.SlotIV != g.DefaultSlotIV {
		t.Errorf("SlotIV = %v, want %v", cr.SlotIV, g.DefaultSlotIV)
	}
	if f.SlotVI != g.DefaultSlotVI {
		t.Errorf("SlotVI = %v, want %v", f.SlotVI, g.DefaultSlotVI)
	}
	un, ok := f.Final.(g.UnframedNominal)
	if !ok || un.Case != g.THM {
		t.Errorf("Final = %v, want UnframedNominal{THM}", f.Final)
	}
}

func TestParseFormative_NonDefault(t *testing.T) {
	// "emlölo" = S2/PRC stem, root ml, DYN/OBJ/EXS Vr, default Ca, ERG case.
	f, err := ParseFormative("emlölo")
	if err != nil {
		t.Fatalf("ParseFormative(\"emlölo\") error: %v", err)
	}
	cr, ok := f.Root.(g.CrRoot)
	if !ok {
		t.Fatalf("Root = %v, want CrRoot", f.Root)
	}
	if cr.Stem != g.S2 || cr.Version != g.PRC {
		t.Errorf("Stem/Version = %v/%v, want S2/PRC", cr.Stem, cr.Version)
	}
	if cr.SlotIV != (g.SlotIV{Function: g.DYN, Specification: g.OBJ, Context: g.EXS}) {
		t.Errorf("SlotIV = %v, want (DYN, OBJ, EXS)", cr.SlotIV)
	}
	un, ok := f.Final.(g.UnframedNominal)
	if !ok || un.Case != g.ERG {
		t.Errorf("Final = %v, want UnframedNominal{ERG}", f.Final)
	}
}

func TestParseFormative_Verbal(t *testing.T) {
	// "amlalú" has ultimate stress (acute on the final u), so the
	// trailing vowel is Vk, not Vc. ASR + INF.
	f, err := ParseFormative("amlalú")
	if err != nil {
		t.Fatalf("ParseFormative(\"amlalú\") error: %v", err)
	}
	uv, ok := f.Final.(g.UnframedVerbal)
	if !ok {
		t.Fatalf("Final = %v, want UnframedVerbal", f.Final)
	}
	as, ok := uv.Vk.(g.Assertive)
	if !ok || as.Validation != g.INF {
		t.Errorf("Vk = %v, want Assertive{INF}", uv.Vk)
	}
}

func TestParseFormative_ElidedVc(t *testing.T) {
	// "amlal" has no trailing vowel — Vc elides to THM.
	f, err := ParseFormative("amlal")
	if err != nil {
		t.Fatalf("ParseFormative(\"amlal\") error: %v", err)
	}
	un, ok := f.Final.(g.UnframedNominal)
	if !ok || un.Case != g.THM {
		t.Errorf("Final = %v, want UnframedNominal{THM}", f.Final)
	}
}

func TestRoundTripMinimal(t *testing.T) {
	original := g.MinimalFormative("ml")
	surface := render.Formative(original)
	parsed, err := ParseFormative(surface)
	if err != nil {
		t.Fatalf("round trip error on %q: %v", surface, err)
	}
	if parsed.Root != original.Root || parsed.SlotVI != original.SlotVI {
		t.Errorf("round trip mismatch:\noriginal: %+v\nparsed:   %+v", original, parsed)
	}
	if original.Final != parsed.Final {
		t.Errorf("Final mismatch: original=%v parsed=%v", original.Final, parsed.Final)
	}
}

func TestParseFormative_TooShort(t *testing.T) {
	for _, w := range []string{"", "a", "am"} {
		if _, err := ParseFormative(w); err == nil {
			t.Errorf("ParseFormative(%q) succeeded, want error", w)
		}
	}
}

func TestParseFormative_ConsonantInitial(t *testing.T) {
	// "malal" = Cr=m, Vr=a (STA/BSC/EXS), Ca=l, no trailing Vc.
	// Vv elides to (S1, PRC).
	f, err := ParseFormative("malal")
	if err != nil {
		t.Fatalf("ParseFormative(\"malal\") error: %v", err)
	}
	if cr, ok := f.Root.(g.CrRoot); !ok || cr.Cluster != "m" {
		t.Errorf("Root = %v, want CrRoot{Cluster: m}", f.Root)
	}
	if f.SlotVI != g.DefaultSlotVI {
		t.Errorf("SlotVI = %v, want default", f.SlotVI)
	}
}

func TestParseFormative_MalëuţřaitCanonical(t *testing.T) {
	// "Malëuţřait" is the language's name for itself — the canonical
	// V4 test word. Lowercased: "malëuţřait".
	// Conjuncts: ["m", "a", "l", "ëu", "ţř", "ai", "t"]
	// Cr=m, Vr=a, Ca=l, Vx=ëu/Cs=ţř (affix 1), Vx=ai/Cs=t (affix 2).
	// Stress is on the penultimate syllable "ai" — no accent marks,
	// 3 syllables → Penultimate by default.
	f, err := ParseFormative("malëuţřait")
	if err != nil {
		t.Fatalf("ParseFormative(canonical Maleuţřait) error: %v", err)
	}
	if cr, ok := f.Root.(g.CrRoot); !ok || cr.Cluster != "m" {
		t.Errorf("Root = %v, want CrRoot{Cluster: m}", f.Root)
	}
	if f.SlotVI != g.DefaultSlotVI {
		t.Errorf("Ca → %v, want default", f.SlotVI)
	}
	if len(f.SlotVII) != 2 {
		t.Fatalf("Slot VII = %v, want 2 affixes", f.SlotVII)
	}
	// "ëu" is in type2Degrees (form 5 of series 2), so Type2Affix.
	if f.SlotVII[0] != (g.Affix{Type: g.Type2Affix, Degree: 5, Consonant: "ţř"}) {
		t.Errorf("Affix 1 = %v", f.SlotVII[0])
	}
	if f.SlotVII[1] != (g.Affix{Type: g.Type2Affix, Degree: 1, Consonant: "t"}) {
		t.Errorf("Affix 2 = %v", f.SlotVII[1])
	}
	if un, ok := f.Final.(g.UnframedNominal); !ok || un.Case != g.THM {
		t.Errorf("Final = %v, want UnframedNominal{THM}", f.Final)
	}
}

func TestParseFormative_SlotI_Type1(t *testing.T) {
	// "h" prefix + vowel-initial body = Type 1 concatenation.
	// "hamlala" = h + amlala.
	f, err := ParseFormative("hamlala")
	if err != nil {
		t.Fatalf("ParseFormative(\"hamlala\") error: %v", err)
	}
	if f.Concat == nil || *f.Concat != g.Type1 {
		t.Errorf("SlotI = %v, want Type1", f.Concat)
	}
	if cr, ok := f.Root.(g.CrRoot); !ok || cr.Cluster != "ml" {
		t.Errorf("Root = %v, want CrRoot{Cluster: ml}", f.Root)
	}
}

func TestParseFormative_SlotI_Type2(t *testing.T) {
	// "hw" prefix → Type 2 concatenation.
	f, err := ParseFormative("hwamlala")
	if err != nil {
		t.Fatalf("ParseFormative(\"hwamlala\") error: %v", err)
	}
	if f.Concat == nil || *f.Concat != g.Type2 {
		t.Errorf("SlotI = %v, want Type2", f.Concat)
	}
}

func TestParseFormative_ShortcutW_Series1(t *testing.T) {
	// "waml" = shortcut "w" + Vv "a" (series 1, S1/PRC) + Cr "ml".
	// Shortcut W series 1 = default Ca (UNI/CSL/M/DEL/NRM).
	f, err := ParseFormative("waml")
	if err != nil {
		t.Fatalf("ParseFormative(\"waml\") error: %v", err)
	}
	if cr, ok := f.Root.(g.CrRoot); !ok || cr.Cluster != "ml" {
		t.Errorf("Root = %v, want CrRoot{Cluster: ml}", f.Root)
	}
	if f.SlotVI != g.DefaultSlotVI {
		t.Errorf("SlotVI = %v, want default (W series 1)", f.SlotVI)
	}
}

func TestParseFormative_ShortcutY_Series3(t *testing.T) {
	// "yuml" = shortcut "y" + Vv "u" (series 1 vowel… wait, let me think).
	// Vv vowel "u" is series 1 form 9. So series=1. Shortcut Y series 1
	// = UNI/CSL/M/PRX/NRM (extension PRX).
	f, err := ParseFormative("yuml")
	if err != nil {
		t.Fatalf("ParseFormative(\"yuml\") error: %v", err)
	}
	want := g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.M_, Extension: g.PRX, Essence: g.NRM}
	if f.SlotVI != want {
		t.Errorf("SlotVI = %v, want %v", f.SlotVI, want)
	}
}

func TestParseFormative_ShortcutW_Series2(t *testing.T) {
	// Vv "ai" is series 2 form 1. Shortcut W series 2 = UNI/CSL/G/DEL/NRM.
	f, err := ParseFormative("waiml")
	if err != nil {
		t.Fatalf("ParseFormative(\"waiml\") error: %v", err)
	}
	want := g.SlotVI{Configuration: g.UNI, Affiliation: g.CSL, Perspective: g.G_, Extension: g.DEL, Essence: g.NRM}
	if f.SlotVI != want {
		t.Errorf("SlotVI = %v, want %v", f.SlotVI, want)
	}
}

func TestParseFormative_ShortcutWithConcat(t *testing.T) {
	// "hlaml" = "hl" (Type1 + ShortcutW) + "a" (Vv) + "ml" (Cr).
	f, err := ParseFormative("hlaml")
	if err != nil {
		t.Fatalf("ParseFormative(\"hlaml\") error: %v", err)
	}
	if f.Concat == nil || *f.Concat != g.Type1 {
		t.Errorf("SlotI = %v, want Type1", f.Concat)
	}
	if cr, ok := f.Root.(g.CrRoot); !ok || cr.Cluster != "ml" {
		t.Errorf("Root = %v, want CrRoot{Cluster: ml}", f.Root)
	}
}

func TestParseFormative_ShortcutWithAffix(t *testing.T) {
	// "wamlar" = shortcut W + Vv a + Cr ml + Slot VII affix (a, r) + no Vc.
	f, err := ParseFormative("wamlar")
	if err != nil {
		t.Fatalf("ParseFormative(\"wamlar\") error: %v", err)
	}
	if len(f.SlotVII) != 1 {
		t.Fatalf("SlotVII = %v, want 1 affix", f.SlotVII)
	}
	if f.SlotVII[0] != (g.Affix{Type: g.Type1Affix, Degree: 1, Consonant: "r"}) {
		t.Errorf("Affix = %v", f.SlotVII[0])
	}
	if un, ok := f.Final.(g.UnframedNominal); !ok || un.Case != g.THM {
		t.Errorf("Final = %v, want UnframedNominal{THM}", f.Final)
	}
}

func TestParseFormative_Corpus_Ärmaläwia(t *testing.T) {
	// From agent-4's corpus tests. "ärmaläwi'a" should parse with:
	//   Vv=ä → S1/CPT
	//   Cr=rm
	//   Vr=a → STA/BSC/EXS (default)
	//   Ca=l → default SlotVI
	//   Vn=ä, Cn=w → VnCnAspect{PRS, ...}
	//   Vc=i'a → LOC case
	//   Stress=Penultimate
	f, err := ParseFormative("ärmaläwi'a")
	if err != nil {
		t.Fatalf("ParseFormative(\"ärmaläwi'a\") error: %v", err)
	}
	cr, ok := f.Root.(g.CrRoot)
	if !ok || cr.Cluster != "rm" {
		t.Errorf("Root = %v, want CrRoot{Cluster: rm}", f.Root)
	}
	if cr.Stem != g.S1 || cr.Version != g.CPT {
		t.Errorf("Stem/Version = %v/%v, want S1/CPT", cr.Stem, cr.Version)
	}
	vc, ok := f.SlotVIII.(g.VnCnAspect)
	if !ok {
		t.Fatalf("SlotVIII = %T, want VnCnAspect", f.SlotVIII)
	}
	if vc.Aspect != g.PRS {
		t.Errorf("Aspect = %v, want PRS", vc.Aspect)
	}
	if un, ok := f.Final.(g.UnframedNominal); !ok || un.Case != g.LOC {
		t.Errorf("Final = %v, want UnframedNominal{LOC}", f.Final)
	}
}

func TestParseFormative_SentencePrefix_Plain(t *testing.T) {
	// "çamlala" = ç + amlala (sentence prefix + plain formative).
	f, err := ParseFormative("çamlala")
	if err != nil {
		t.Fatalf("ParseFormative(\"çamlala\") error: %v", err)
	}
	if !f.SentenceStarter {
		t.Error("SentenceStarter = false, want true")
	}
	if cr, ok := f.Root.(g.CrRoot); !ok || cr.Cluster != "ml" {
		t.Errorf("Root = %v, want CrRoot{Cluster: ml}", f.Root)
	}
}

func TestParseFormative_SentencePrefix_çë(t *testing.T) {
	// "çëmlala" = ç + ë (default Vv) + mlala; strip both, parse as "mlala".
	// mlala has 4 conjuncts ["ml", "a", "l", "a"]: consonant-initial,
	// Cr=ml, Vr=a, Ca=l, Vc=a.
	f, err := ParseFormative("çëmlala")
	if err != nil {
		t.Fatalf("ParseFormative(\"çëmlala\") error: %v", err)
	}
	if !f.SentenceStarter {
		t.Error("SentenceStarter = false, want true")
	}
	if cr, ok := f.Root.(g.CrRoot); !ok || cr.Cluster != "ml" {
		t.Errorf("Root = %v, want CrRoot{Cluster: ml}", f.Root)
	}
}

func TestParseFormative_SentencePrefix_çç(t *testing.T) {
	// "ççaml" = ç + ç + aml; the inner ç becomes y. Result is "yaml":
	// shortcut Y + Vv "a" + Cr "ml".
	f, err := ParseFormative("ççaml")
	if err != nil {
		t.Fatalf("ParseFormative(\"ççaml\") error: %v", err)
	}
	if !f.SentenceStarter {
		t.Error("SentenceStarter = false, want true")
	}
}

func TestParseFormative_NoSentencePrefix(t *testing.T) {
	f, err := ParseFormative("amlala")
	if err != nil {
		t.Fatal(err)
	}
	if f.SentenceStarter {
		t.Error("SentenceStarter = true, want false")
	}
}

func TestParseFormative_GlottalCaseAlone(t *testing.T) {
	// "amlali'a" = vowel-initial minimal formative with LOC case via
	// glottalized Vc. No Slot VIII.
	f, err := ParseFormative("amlali'a")
	if err != nil {
		t.Fatalf("ParseFormative(\"amlali'a\") error: %v", err)
	}
	if un, ok := f.Final.(g.UnframedNominal); !ok || un.Case != g.LOC {
		t.Errorf("Final = %v, want UnframedNominal{LOC}", f.Final)
	}
}

func TestRoundTrip_ConsonantInitial(t *testing.T) {
	// Build a minimal consonant-initial formative (Vv = default elided).
	// Note: render always emits a Vv vowel, so a round-trip from a
	// consonant-initial-style Formative goes through render and comes
	// back as vowel-initial. We instead start from a parsed consonant-
	// initial word and check the slot values match expectations.
	parsed, err := ParseFormative("malal")
	if err != nil {
		t.Fatal(err)
	}
	// The Render layer always emits Vv, so the round-trip surface is
	// the vowel-initial form. Re-parsing should give the same slot values.
	surface := "a" + "m" + "a" + "l" // = render of {S1,PRC; m; defaults}
	if surface != "amal" {
		t.Fatalf("surface synthesis error: %s", surface)
	}
	reparsed, err := ParseFormative(surface)
	if err != nil {
		t.Fatal(err)
	}
	if reparsed.Root != parsed.Root || reparsed.SlotVI != parsed.SlotVI {
		t.Errorf("re-parse differs from consonant-initial parse")
	}
}

func TestParseFormative_WithSlotVIIAffix(t *testing.T) {
	// "amlalara" = Vv=a, Cr=ml, Vr=a, Ca=l, then Slot VII affix (a, r),
	// then Vc=a. The affix "r" is not a valid Cn, so it stays as affix.
	f, err := ParseFormative("amlalara")
	if err != nil {
		t.Fatalf("ParseFormative(\"amlalara\") error: %v", err)
	}
	want := []g.Affix{{Type: g.Type1Affix, Degree: 1, Consonant: "r"}}
	if len(f.SlotVII) != 1 || f.SlotVII[0] != want[0] {
		t.Errorf("SlotVII = %v, want %v", f.SlotVII, want)
	}
	if un, ok := f.Final.(g.UnframedNominal); !ok || un.Case != g.THM {
		t.Errorf("Final = %v, want UnframedNominal{THM}", f.Final)
	}
	if f.SlotVIII != nil {
		t.Errorf("SlotVIII = %v, want nil", f.SlotVIII)
	}
}

func TestParseFormative_WithSlotVIII(t *testing.T) {
	// "amlalahla" = Vv=a, Cr=ml, Vr=a, Ca=l, Vn=a, Cn=hl, Vc=a.
	// "hl" is a valid Pattern-1 Cn → Slot VIII = VnCnValence(MNO, SUB).
	// Stress is penultimate, so Mood (SUB) is reinterpreted as the
	// parallel CaseScope (CCA).
	f, err := ParseFormative("amlalahla")
	if err != nil {
		t.Fatalf("ParseFormative(\"amlalahla\") error: %v", err)
	}
	vc, ok := f.SlotVIII.(g.VnCnValence)
	if !ok {
		t.Fatalf("SlotVIII = %T %v, want VnCnValence", f.SlotVIII, f.SlotVIII)
	}
	if vc.Valence != g.MNO {
		t.Errorf("Valence = %v, want MNO", vc.Valence)
	}
	// MoodScope encodes the 6-value Cn pattern as a Mood; CCA = SUB.
	if vc.MoodScope != g.SUB {
		t.Errorf("MoodScope = %v, want SUB (CCA)", vc.MoodScope)
	}
	// SlotVII should be empty.
	if f.SlotVII != nil {
		t.Errorf("SlotVII = %v, want nil", f.SlotVII)
	}
	if un, ok := f.Final.(g.UnframedNominal); !ok || un.Case != g.THM {
		t.Errorf("Final = %v, want UnframedNominal{THM}", f.Final)
	}
}

func TestParseFormative_SlotVIIIVerbal(t *testing.T) {
	// Same shape as above but ultimate stress on the final vowel.
	// "amlalahlá" → Final = UnframedVerbal{Assertive{OBS}}, SlotVIII MoodVal.
	f, err := ParseFormative("amlalahlá")
	if err != nil {
		t.Fatalf("ParseFormative(\"amlalahlá\") error: %v", err)
	}
	if _, ok := f.Final.(g.UnframedVerbal); !ok {
		t.Errorf("Final = %v, want UnframedVerbal", f.Final)
	}
	vc, ok := f.SlotVIII.(g.VnCnValence)
	if !ok {
		t.Fatalf("SlotVIII = %T, want VnCnValence", f.SlotVIII)
	}
	if vc.MoodScope != g.SUB {
		t.Errorf("MoodScope = %v, want SUB", vc.MoodScope)
	}
}

func TestRoundTrip_WithSlotVII(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SlotVII = []g.Affix{{Type: g.Type1Affix, Degree: 1, Consonant: "r"}}
	surface := render.Formative(f)
	parsed, err := ParseFormative(surface)
	if err != nil {
		t.Fatalf("round trip %q failed: %v", surface, err)
	}
	if len(parsed.SlotVII) != 1 || parsed.SlotVII[0] != f.SlotVII[0] {
		t.Errorf("SlotVII round-trip: got %v, want %v", parsed.SlotVII, f.SlotVII)
	}
}

func TestRoundTrip_WithSlotVIII(t *testing.T) {
	// Build a formative with VnCnValence + nominal stress so render
	// emits a CaseScope-pattern Cn; the parser must round-trip the
	// CaseScopeVal variant.
	f := g.MinimalFormative("ml")
	f.SlotVIII = g.VnCnValence{
		Valence: g.MNO,
		MoodScope: g.SUB,
	}
	surface := render.Formative(f)
	parsed, err := ParseFormative(surface)
	if err != nil {
		t.Fatalf("round trip %q failed: %v", surface, err)
	}
	if parsed.SlotVIII != f.SlotVIII {
		t.Errorf("SlotVIII round-trip: got %v, want %v", parsed.SlotVIII, f.SlotVIII)
	}
}

func TestParseFormative_RoundTripVariants(t *testing.T) {
	cases := []struct {
		name string
		f    g.Formative
	}{
		{"S2/PRC, ERG", func() g.Formative {
			f := g.MinimalFormative("ml")
			cr := f.Root.(g.CrRoot)
			cr.Stem = g.S2
			f.Root = cr
			f.Final = g.UnframedNominal{Case: g.ERG}
			return f
		}()},
		{"DYN/CTE/FNC, ABS", func() g.Formative {
			f := g.MinimalFormative("t")
			cr := f.Root.(g.CrRoot)
			cr.SlotIV = g.SlotIV{Function: g.DYN, Specification: g.CTE, Context: g.FNC}
			f.Root = cr
			f.Final = g.UnframedNominal{Case: g.ABS}
			return f
		}()},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			surface := render.Formative(c.f)
			parsed, err := ParseFormative(surface)
			if err != nil {
				t.Fatalf("round trip error on %q: %v", surface, err)
			}
			if parsed.Root != c.f.Root {
				t.Errorf("Root: got %v, want %v", parsed.Root, c.f.Root)
			}
			if parsed.Final != c.f.Final {
				t.Errorf("Final: got %v, want %v", parsed.Final, c.f.Final)
			}
		})
	}
}
