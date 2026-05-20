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
	if f.SlotII != g.DefaultSlotII {
		t.Errorf("SlotII = %v, want %v", f.SlotII, g.DefaultSlotII)
	}
	if f.SlotIII != "ml" {
		t.Errorf("SlotIII = %q, want %q", f.SlotIII, "ml")
	}
	if f.SlotIV != g.DefaultSlotIV {
		t.Errorf("SlotIV = %v, want %v", f.SlotIV, g.DefaultSlotIV)
	}
	if f.SlotVI != g.DefaultSlotVI {
		t.Errorf("SlotVI = %v, want %v", f.SlotVI, g.DefaultSlotVI)
	}
	cs, ok := f.SlotIX.(g.CaseSlot)
	if !ok || cs.Case != g.THM {
		t.Errorf("SlotIX = %v, want CaseSlot{THM}", f.SlotIX)
	}
	if f.Stress != g.Penultimate {
		t.Errorf("Stress = %v, want Penultimate", f.Stress)
	}
}

func TestParseFormative_NonDefault(t *testing.T) {
	// "emlölo" = S2/PRC stem, root ml, DYN/OBJ/EXS Vr, default Ca, ERG case.
	f, err := ParseFormative("emlölo")
	if err != nil {
		t.Fatalf("ParseFormative(\"emlölo\") error: %v", err)
	}
	if f.SlotII != (g.SlotII{Stem: g.S2, Version: g.PRC}) {
		t.Errorf("SlotII = %v, want (S2, PRC)", f.SlotII)
	}
	if f.SlotIV != (g.SlotIV{Function: g.DYN, Specification: g.OBJ, Context: g.EXS}) {
		t.Errorf("SlotIV = %v, want (DYN, OBJ, EXS)", f.SlotIV)
	}
	cs, ok := f.SlotIX.(g.CaseSlot)
	if !ok || cs.Case != g.ERG {
		t.Errorf("SlotIX = %v, want CaseSlot{ERG}", f.SlotIX)
	}
}

func TestParseFormative_Verbal(t *testing.T) {
	// "amlalú" has ultimate stress (acute on the final u), so the
	// trailing vowel is Vk, not Vc. ASR + INF.
	f, err := ParseFormative("amlalú")
	if err != nil {
		t.Fatalf("ParseFormative(\"amlalú\") error: %v", err)
	}
	if f.Stress != g.Ultimate {
		t.Errorf("Stress = %v, want Ultimate", f.Stress)
	}
	as, ok := f.SlotIX.(g.Assertive)
	if !ok || as.Validation != g.INF {
		t.Errorf("SlotIX = %v, want Assertive{INF}", f.SlotIX)
	}
}

func TestParseFormative_ElidedVc(t *testing.T) {
	// "amlal" has no trailing vowel — Vc elides to THM.
	f, err := ParseFormative("amlal")
	if err != nil {
		t.Fatalf("ParseFormative(\"amlal\") error: %v", err)
	}
	cs, ok := f.SlotIX.(g.CaseSlot)
	if !ok || cs.Case != g.THM {
		t.Errorf("SlotIX = %v, want CaseSlot{THM}", f.SlotIX)
	}
}

func TestRoundTripMinimal(t *testing.T) {
	original := g.MinimalFormative("ml")
	surface := render.Formative(original)
	parsed, err := ParseFormative(surface)
	if err != nil {
		t.Fatalf("round trip error on %q: %v", surface, err)
	}
	if parsed.SlotII != original.SlotII ||
		parsed.SlotIII != original.SlotIII ||
		parsed.SlotIV != original.SlotIV ||
		parsed.SlotVI != original.SlotVI {
		t.Errorf("round trip mismatch:\noriginal: %+v\nparsed:   %+v", original, parsed)
	}
	cs1, ok1 := original.SlotIX.(g.CaseSlot)
	cs2, ok2 := parsed.SlotIX.(g.CaseSlot)
	if !ok1 || !ok2 || cs1 != cs2 {
		t.Errorf("SlotIX mismatch: original=%v parsed=%v", original.SlotIX, parsed.SlotIX)
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
	if f.SlotII != g.DefaultSlotII {
		t.Errorf("SlotII = %v, want default (S1, PRC)", f.SlotII)
	}
	if f.SlotIII != "m" {
		t.Errorf("SlotIII = %q, want \"m\"", f.SlotIII)
	}
	if f.SlotIV != g.DefaultSlotIV {
		t.Errorf("SlotIV = %v, want default", f.SlotIV)
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
	if f.SlotIII != "m" {
		t.Errorf("Cr = %q, want \"m\"", f.SlotIII)
	}
	if f.SlotIV != g.DefaultSlotIV {
		t.Errorf("Vr → %v, want default (STA, BSC, EXS)", f.SlotIV)
	}
	if f.SlotVI != g.DefaultSlotVI {
		t.Errorf("Ca → %v, want default", f.SlotVI)
	}
	if len(f.SlotVII) != 2 {
		t.Fatalf("Slot VII = %v, want 2 affixes", f.SlotVII)
	}
	// "ëu" is in type2Degrees (form 5 of series 2), so Type2Affix.
	if f.SlotVII[0] != (g.Affix{Vowel: "ëu", Consonant: "ţř", Type: g.Type2Affix}) {
		t.Errorf("Affix 1 = %v", f.SlotVII[0])
	}
	if f.SlotVII[1] != (g.Affix{Vowel: "ai", Consonant: "t", Type: g.Type2Affix}) {
		t.Errorf("Affix 2 = %v", f.SlotVII[1])
	}
	if cs, ok := f.SlotIX.(g.CaseSlot); !ok || cs.Case != g.THM {
		t.Errorf("Slot IX = %v, want CaseSlot{THM}", f.SlotIX)
	}
	if f.Stress != g.Penultimate {
		t.Errorf("Stress = %v, want Penultimate", f.Stress)
	}
}

func TestParseFormative_SlotI_Type1(t *testing.T) {
	// "h" prefix + vowel-initial body = Type 1 concatenation.
	// "hamlala" = h + amlala.
	f, err := ParseFormative("hamlala")
	if err != nil {
		t.Fatalf("ParseFormative(\"hamlala\") error: %v", err)
	}
	if f.SlotI == nil || *f.SlotI != g.Type1 {
		t.Errorf("SlotI = %v, want Type1", f.SlotI)
	}
	if f.SlotIII != "ml" {
		t.Errorf("Cr = %q, want \"ml\"", f.SlotIII)
	}
}

func TestParseFormative_SlotI_Type2(t *testing.T) {
	// "hw" prefix → Type 2 concatenation.
	f, err := ParseFormative("hwamlala")
	if err != nil {
		t.Fatalf("ParseFormative(\"hwamlala\") error: %v", err)
	}
	if f.SlotI == nil || *f.SlotI != g.Type2 {
		t.Errorf("SlotI = %v, want Type2", f.SlotI)
	}
}

func TestParseFormative_ShortcutW_Series1(t *testing.T) {
	// "waml" = shortcut "w" + Vv "a" (series 1, S1/PRC) + Cr "ml".
	// Shortcut W series 1 = default Ca (UNI/CSL/M/DEL/NRM).
	f, err := ParseFormative("waml")
	if err != nil {
		t.Fatalf("ParseFormative(\"waml\") error: %v", err)
	}
	if f.SlotIShortcut == nil || *f.SlotIShortcut != g.ShortcutW {
		t.Errorf("SlotIShortcut = %v, want ShortcutW", f.SlotIShortcut)
	}
	if f.SlotII != g.DefaultSlotII {
		t.Errorf("SlotII = %v, want default", f.SlotII)
	}
	if f.SlotIII != "ml" {
		t.Errorf("Cr = %q, want \"ml\"", f.SlotIII)
	}
	if f.SlotIV != g.DefaultSlotIV {
		t.Errorf("SlotIV = %v, want default (shortcuts elide Vr)", f.SlotIV)
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
	if f.SlotIShortcut == nil || *f.SlotIShortcut != g.ShortcutY {
		t.Errorf("SlotIShortcut = %v, want ShortcutY", f.SlotIShortcut)
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
	if f.SlotI == nil || *f.SlotI != g.Type1 {
		t.Errorf("SlotI = %v, want Type1", f.SlotI)
	}
	if f.SlotIShortcut == nil || *f.SlotIShortcut != g.ShortcutW {
		t.Errorf("SlotIShortcut = %v, want ShortcutW", f.SlotIShortcut)
	}
	if f.SlotIII != "ml" {
		t.Errorf("Cr = %q, want \"ml\"", f.SlotIII)
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
	if f.SlotVII[0] != (g.Affix{Vowel: "a", Consonant: "r", Type: g.Type1Affix}) {
		t.Errorf("Affix = %v", f.SlotVII[0])
	}
	cs, ok := f.SlotIX.(g.CaseSlot)
	if !ok || cs.Case != g.THM {
		t.Errorf("SlotIX = %v, want CaseSlot{THM}", f.SlotIX)
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
	if f.SlotII != (g.SlotII{Stem: g.S1, Version: g.CPT}) {
		t.Errorf("SlotII = %v, want (S1, CPT)", f.SlotII)
	}
	if f.SlotIII != "rm" {
		t.Errorf("Cr = %q, want \"rm\"", f.SlotIII)
	}
	vc, ok := f.SlotVIII.(g.VnCnAspect)
	if !ok {
		t.Fatalf("SlotVIII = %T, want VnCnAspect", f.SlotVIII)
	}
	if vc.Aspect != g.PRS {
		t.Errorf("Aspect = %v, want PRS", vc.Aspect)
	}
	cs, ok := f.SlotIX.(g.CaseSlot)
	if !ok || cs.Case != g.LOC {
		t.Errorf("SlotIX = %v, want CaseSlot{LOC}", f.SlotIX)
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
	if f.SlotIII != "ml" {
		t.Errorf("Cr = %q, want \"ml\"", f.SlotIII)
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
	if f.SlotIII != "ml" {
		t.Errorf("Cr = %q, want \"ml\"", f.SlotIII)
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
	if f.SlotIShortcut == nil || *f.SlotIShortcut != g.ShortcutY {
		t.Errorf("SlotIShortcut = %v, want ShortcutY", f.SlotIShortcut)
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
	cs, ok := f.SlotIX.(g.CaseSlot)
	if !ok || cs.Case != g.LOC {
		t.Errorf("SlotIX = %v, want CaseSlot{LOC}", f.SlotIX)
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
	if reparsed.SlotII != parsed.SlotII ||
		reparsed.SlotIII != parsed.SlotIII ||
		reparsed.SlotIV != parsed.SlotIV ||
		reparsed.SlotVI != parsed.SlotVI {
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
	want := []g.Affix{{Vowel: "a", Consonant: "r", Type: g.Type1Affix}}
	if len(f.SlotVII) != 1 || f.SlotVII[0] != want[0] {
		t.Errorf("SlotVII = %v, want %v", f.SlotVII, want)
	}
	cs, ok := f.SlotIX.(g.CaseSlot)
	if !ok || cs.Case != g.THM {
		t.Errorf("SlotIX = %v, want CaseSlot{THM}", f.SlotIX)
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
	// Penultimate stress = nominal → CaseScopeVal.
	csv, ok := vc.MS.(g.CaseScopeVal)
	if !ok || csv.CaseScope != g.CCA {
		t.Errorf("MS = %v, want CaseScopeVal{CCA}", vc.MS)
	}
	// SlotVII should be empty.
	if f.SlotVII != nil {
		t.Errorf("SlotVII = %v, want nil", f.SlotVII)
	}
	cs, ok := f.SlotIX.(g.CaseSlot)
	if !ok || cs.Case != g.THM {
		t.Errorf("SlotIX = %v, want CaseSlot{THM}", f.SlotIX)
	}
}

func TestParseFormative_SlotVIIIVerbal(t *testing.T) {
	// Same shape as above but ultimate stress on the final vowel.
	// "amlalahlá" → stress Ultimate. SlotIX is Vk, SlotVIII MoodVal.
	f, err := ParseFormative("amlalahlá")
	if err != nil {
		t.Fatalf("ParseFormative(\"amlalahlá\") error: %v", err)
	}
	if f.Stress != g.Ultimate {
		t.Errorf("Stress = %v, want Ultimate", f.Stress)
	}
	vc, ok := f.SlotVIII.(g.VnCnValence)
	if !ok {
		t.Fatalf("SlotVIII = %T, want VnCnValence", f.SlotVIII)
	}
	mv, ok := vc.MS.(g.MoodVal)
	if !ok || mv.Mood != g.SUB {
		t.Errorf("MS = %v, want MoodVal{SUB}", vc.MS)
	}
}

func TestRoundTrip_WithSlotVII(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.SlotVII = []g.Affix{{Vowel: "a", Consonant: "r", Type: g.Type1Affix}}
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
		MS:      g.CaseScopeVal{CaseScope: g.CCA},
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
			f.SlotII = g.SlotII{Stem: g.S2, Version: g.PRC}
			f.SlotIX = g.CaseSlot{Case: g.ERG}
			return f
		}()},
		{"DYN/CTE/FNC, ABS", func() g.Formative {
			f := g.MinimalFormative("t")
			f.SlotIV = g.SlotIV{Function: g.DYN, Specification: g.CTE, Context: g.FNC}
			f.SlotIX = g.CaseSlot{Case: g.ABS}
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
			if parsed.SlotII != c.f.SlotII {
				t.Errorf("SlotII: got %v, want %v", parsed.SlotII, c.f.SlotII)
			}
			if parsed.SlotIV != c.f.SlotIV {
				t.Errorf("SlotIV: got %v, want %v", parsed.SlotIV, c.f.SlotIV)
			}
			if parsed.SlotIX != c.f.SlotIX {
				t.Errorf("SlotIX: got %v, want %v", parsed.SlotIX, c.f.SlotIX)
			}
		})
	}
}
