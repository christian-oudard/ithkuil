package tokenize

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/phonology"
)

// A §4.8 parsing adjunct declares the stress of the word after it.
// That is phonology, not grammar — it says how the next word is said,
// not what it means — so Tokenize consumes it and it never appears as
// a word of its own.
func TestTokenize_ParsingAdjunctIsConsumed(t *testing.T) {
	// "amlala" carries penultimate stress unmarked; 'e' declares
	// ultimate, which is written with an acute.
	toks := Tokenize("'e' amlala")
	if len(toks) != 1 {
		t.Fatalf("Tokenize returned %d words, want 1: %+v", len(toks), toks)
	}
	if got := toks[0].Romanization(); got != "amlalá" {
		t.Errorf("declared stress not applied: got %q, want %q", got, "amlalá")
	}
}

// Every vowel §2.3 ¶5 lists names a stress.
func TestParsingAdjunct_Vowels(t *testing.T) {
	for _, c := range []struct {
		in   string
		want phonology.Stress
	}{
		{"'a'", phonology.Monosyllabic},
		{"'e'", phonology.Ultimate},
		{"'o'", phonology.Penultimate},
		{"'u'", phonology.Antepenultimate},
	} {
		got, ok := phonology.ParsingAdjunct(c.in)
		if !ok || got != c.want {
			t.Errorf("ParsingAdjunct(%q) = %v, %v; want %v", c.in, got, ok, c.want)
		}
	}
	if _, ok := phonology.ParsingAdjunct("'i'"); ok {
		t.Error("'i' is not one of the four vowels §2.3 ¶5 lists")
	}
}

// The adjunct reverses correctly because its content is the stress,
// and stress is fully expressible with the diacritics. Reading a word
// written with an adjunct and writing it back gives the same word with
// the stress marked, which is the canonical spelling of the same
// grammar rather than a loss.
func TestParsingAdjunct_ReversesAsADiacritic(t *testing.T) {
	viaAdjunct := Tokenize("'e' amlala")
	viaDiacritic := Tokenize("amlalá")
	if len(viaAdjunct) != 1 || len(viaDiacritic) != 1 {
		t.Fatalf("expected one word each, got %d and %d", len(viaAdjunct), len(viaDiacritic))
	}
	a, aok := viaAdjunct[0].(FormativeWord)
	b, bok := viaDiacritic[0].(FormativeWord)
	if !aok || !bok {
		t.Fatalf("expected formatives, got %T and %T", viaAdjunct[0], viaDiacritic[0])
	}
	if a.Formative.Final != b.Formative.Final {
		t.Errorf("the two spellings disagree on stress: %v vs %v",
			a.Formative.Final, b.Formative.Final)
	}
}

func TestClassifyWord_Bias(t *testing.T) {
	w := ClassifyWord("řřx")
	b, ok := w.(BiasWord)
	if !ok {
		t.Fatalf("ClassifyWord(\"řřx\") = %T, want BiasWord", w)
	}
	if b.Bias != g.DOL {
		t.Errorf("Bias = %v, want DOL", b.Bias)
	}
}

func TestClassifyWord_RegisterOpen(t *testing.T) {
	w := ClassifyWord("ha")
	r, ok := w.(RegisterStartWord)
	if !ok {
		t.Fatalf("ClassifyWord(\"ha\") = %T, want RegisterStartWord", w)
	}
	if r.Register != g.DSV {
		t.Errorf("Register = %v, want DSV", r.Register)
	}
}

func TestClassifyWord_RegisterClose(t *testing.T) {
	w := ClassifyWord("hai")
	r, ok := w.(RegisterEndWord)
	if !ok {
		t.Fatalf("ClassifyWord(\"hai\") = %T, want RegisterEndWord", w)
	}
	if r.Register != g.DSV {
		t.Errorf("Register = %v, want DSV", r.Register)
	}
}

func TestClassifyWord_Carrier(t *testing.T) {
	w := ClassifyWord("hla")
	c, ok := w.(CarrierWord)
	if !ok {
		t.Fatalf("ClassifyWord(\"hla\") = %T, want CarrierWord", w)
	}
	if c.Carrier.Type != g.Carrier || c.Carrier.Case != g.THM {
		t.Errorf("Carrier = %v, want {Carrier, THM}", c.Carrier)
	}
}

func TestClassifyWord_Modular(t *testing.T) {
	// "ah" = Vn "a" + Cn "h" → modular.
	w := ClassifyWord("ah")
	m, ok := w.(ModularWord)
	if !ok {
		t.Fatalf("ClassifyWord(\"ah\") = %T, want ModularWord", w)
	}
	if len(m.Modular.Content) != 1 {
		t.Fatalf("Modular.Content = %v, want one entry", m.Modular.Content)
	}
	if _, ok := m.Modular.Content[0].(g.VnCnValence); !ok {
		t.Errorf("Modular.Content[0] = %T, want VnCnValence", m.Modular.Content[0])
	}
}

func TestClassifyWord_Formative(t *testing.T) {
	w := ClassifyWord("malëuţřait")
	f, ok := w.(FormativeWord)
	if !ok {
		t.Fatalf("ClassifyWord(\"malëuţřait\") = %T, want FormativeWord", w)
	}
	cr, ok := f.Formative.Root.(g.CrRoot)
	if !ok || cr.Cluster != "m" {
		t.Errorf("Root = %v, want CrRoot{Cluster:m}", f.Formative.Root)
	}
}

func TestClassifyWord_ReferentialWithCase(t *testing.T) {
	// "lü" = R1m + DAT case.
	w := ClassifyWord("lü")
	r, ok := w.(ReferentialWord)
	if !ok {
		t.Fatalf("ClassifyWord(\"lü\") = %T, want ReferentialWord", w)
	}
	refs, ok := g.HeadRefs(r.Referential.Head)
	if !ok || len(refs) != 1 || refs[0].Referent.String() != "1m" {
		t.Errorf("refs = %v, want [{1m, NEU}]", refs)
	}
	if r.Referential.Case.String() != "DAT" {
		t.Errorf("Case = %v, want DAT", r.Referential.Case)
	}
}

// §4.6.1 leaves V_C1 unparenthesized in its slot table and gives
// "(ë)C(C)-V" as the shape to look for, so a referential always carries
// a case. A bare consonant cluster is not one — nor is it a word at
// all, having no vowel to pronounce.
func TestClassifyWord_BareClusterIsNotAReferential(t *testing.T) {
	for _, w := range []string{"l", "sml"} {
		if got := ClassifyWord(w); !isUnknown(got) {
			t.Errorf("ClassifyWord(%q) = %T, want UnknownWord", w, got)
		}
	}
}

func isUnknown(t WordToken) bool {
	_, ok := t.(UnknownWord)
	return ok
}

func TestClassifyWord_Concatenated(t *testing.T) {
	// Per §3.1.1, the concatenated formative comes FIRST in written
	// order with a Cc marker, and the parent comes LAST without one.
	// "hamlala-amlala" = Type1-concat "hamlala" (h prefix) + parent
	// "amlala".
	w := ClassifyWord("hamlala-amlala")
	cf, ok := w.(ConcatenatedFormativeWord)
	if !ok {
		t.Fatalf("ClassifyWord(\"hamlala-amlala\") = %T, want ConcatenatedFormativeWord", w)
	}
	if cf.Chain.Length() != 2 {
		t.Errorf("chain length = %d, want 2", cf.Chain.Length())
	}
}

func TestClassifyWord_NotAChain(t *testing.T) {
	// A single hyphen with no real formative on one side falls through
	// to UnknownWord (or whatever else might match).
	w := ClassifyWord("amlala-")
	if _, ok := w.(ConcatenatedFormativeWord); ok {
		t.Errorf("ClassifyWord(\"amlala-\") = %T, should not be concat chain", w)
	}
}

func TestClassifyWord_Unknown(t *testing.T) {
	w := ClassifyWord("xyzzy")
	if _, ok := w.(UnknownWord); !ok {
		t.Errorf("ClassifyWord(\"xyzzy\") = %T, want UnknownWord", w)
	}
}

// TestClassifyWord_IthkuilGlossCorpus is a smoke test seeded with
// example words from the Kotlin IthkuilGloss test suite (WordTests.kt).
// We assert classifier types only — our gloss differs from
// theirs so a literal port isn't useful, but the classifier should at
// least agree on what kind of word each input is.
//
// Known divergences (not asserted here):
//   - "hrei" — was a standalone Mood/Case-Scope adjunct in earlier
//     spec versions. **Eliminated in v1.3** (replaced by the MCS
//     affix), so we're correct to reject it; Kotlin tracks v0.19.0
//     and still accepts it.
func TestClassifyWord_IthkuilGlossCorpus(t *testing.T) {
	type want int
	const (
		formative want = iota
		concatenated
		ref
		combref
		modular
		bias
		registerStart
		carrier
		singleAffix
		multiAffix
	)
	cases := []struct {
		word string
		kind want
	}{
		{"yužgrá", formative},    // S3 verbal, **žgr** root
		{"eolaleici", formative}, // S2 PRC nominal, **l** root
		{"khe", ref},             // Rdp/DET referential, ABS
		{"lalu", formative},      // basic **l** + IND
		{"ha", registerStart},    // DSV register open
		{"pļļ", bias},            // CMD "Funny!"
		{"çalal", formative},     // sentence prefix + **l**
		{"çëlal", formative},     // sentence prefix (ëi form) + **l**
		{"ççala", formative},     // sentence prefix + y shortcut
		{"çwala", formative},     // sentence prefix + w shortcut
		{"ihnú", modular},        // RCP.COU modular
		{"lala'a", formative},    // glottalized PRN
		{"wala'ana", formative},  // w-prefix, **l** + affix
		{"ëilal", formative},     // Cs-root (D1, **l**)
		{"oërmölá", formative},   // Cs-root CPT.DYN
		{"oërmoulá", formative},  // Cs-root CPT.DYN.FNC
		{"lála'a", formative},    // PRN + ANT (framed)
		{"hna", carrier},         // Naming carrier (cf. TestTokenize_CarrierForeign)
		{"ţnaxeka", combref},     // [mi.BEN+2p] combination referential
		{"ţnaxekka", formative},  // same shape but kk geminate → formative
		{"äst", singleAffix},     // affixual adjunct **st**/2₁
		{"are", singleAffix},     // V-C-V with VIIDom scope
		{"xaheitr", multiAffix},  // multi-affix with Cz=h
		{"xa'heitr", multiAffix}, // multi-affix with Cz='h (VSub)
		{"layá", ref},            // 1m-THM-THM\RPV (Vc2 + RPV stress)
		{"miyüs", ref},           // ma-AFF-DAT-2m (Vc2 + C2)
		{"adni'lö", formative},   // dn root + UTL with moved-glottal Vc
		{"la'la", formative},     // l root + PRN with moved-glottal Vc
		{"hamlala-amlala", concatenated},
		{"çëhamala-lala", concatenated},
	}
	typeName := func(w WordToken) string {
		switch w.(type) {
		case FormativeWord:
			return "FormativeWord"
		case ConcatenatedFormativeWord:
			return "ConcatenatedFormativeWord"
		case ReferentialWord:
			return "ReferentialWord"
		case CombinationRefWord:
			return "CombinationRefWord"
		case ModularWord:
			return "ModularWord"
		case SingleAffixWord:
			return "SingleAffixWord"
		case MultipleAffixWord:
			return "MultipleAffixWord"
		case BiasWord:
			return "BiasWord"
		case RegisterStartWord:
			return "RegisterStartWord"
		case CarrierWord:
			return "CarrierWord"
		case UnknownWord:
			return "UnknownWord"
		}
		return "?"
	}
	for _, c := range cases {
		w := ClassifyWord(c.word)
		matched := false
		switch c.kind {
		case formative:
			_, matched = w.(FormativeWord)
		case concatenated:
			_, matched = w.(ConcatenatedFormativeWord)
		case ref:
			_, matched = w.(ReferentialWord)
		case combref:
			_, matched = w.(CombinationRefWord)
		case modular:
			_, matched = w.(ModularWord)
		case bias:
			_, matched = w.(BiasWord)
		case registerStart:
			_, matched = w.(RegisterStartWord)
		case carrier:
			_, matched = w.(CarrierWord)
		case singleAffix:
			_, matched = w.(SingleAffixWord)
		case multiAffix:
			_, matched = w.(MultipleAffixWord)
		}
		if !matched {
			t.Errorf("ClassifyWord(%q) = %s, want kind %d", c.word, typeName(w), c.kind)
		}
	}
}

func TestTokenize_CarrierForeign(t *testing.T) {
	// "hna John malá" — "hna" is a Naming carrier (NAM + THM), so "John"
	// is foreign text. "malá" continues the sentence as a regular formative.
	tokens := Tokenize("hna John malá")
	if len(tokens) != 3 {
		t.Fatalf("got %d tokens, want 3", len(tokens))
	}
	if _, ok := tokens[0].(CarrierWord); !ok {
		t.Errorf("token 0 = %T, want CarrierWord", tokens[0])
	}
	fw, ok := tokens[1].(ForeignWord)
	if !ok {
		t.Fatalf("token 1 = %T, want ForeignWord", tokens[1])
	}
	if fw.Text != "John" {
		t.Errorf("ForeignWord.Text = %q, want \"John\"", fw.Text)
	}
	// malá should NOT be foreign — carrier only scopes one word.
	if _, isForeign := tokens[2].(ForeignWord); isForeign {
		t.Errorf("token 2 should not be ForeignWord; carrier only scopes one")
	}
}

func TestTokenize_Sentence(t *testing.T) {
	// Three non-interacting tokens: formative + bias + register.
	// (A carrier-led sentence is exercised by TestTokenize_CarrierForeign.)
	tokens := Tokenize("malëuţřait řřx ha")
	if len(tokens) != 3 {
		t.Fatalf("got %d tokens, want 3", len(tokens))
	}
	if _, ok := tokens[0].(FormativeWord); !ok {
		t.Errorf("token 0 = %T, want FormativeWord", tokens[0])
	}
	if _, ok := tokens[1].(BiasWord); !ok {
		t.Errorf("token 1 = %T, want BiasWord", tokens[1])
	}
	if _, ok := tokens[2].(RegisterStartWord); !ok {
		t.Errorf("token 2 = %T, want RegisterStartWord", tokens[2])
	}
}

func TestTokenize_ModularMarksMood(t *testing.T) {
	// Modular Cn is shared between Mood and Case-Scope; the surrounding
	// formative's verbal/nominal status disambiguates (§3.8.1).
	cases := []struct {
		sentence string
		wantNil  bool
		wantMood bool
	}{
		// Verbal next formative (ultimate stress) → MarksMood=true.
		{"ah amlalú", false, true},
		// Nominal next formative (penultimate stress) → MarksMood=false.
		{"ah amlala", false, false},
		// Framed-verbal (antepenultimate stress) → also CaseScope per §3.8.1.
		{"ah ámlala", false, false},
		// No following formative → MarksMood=nil (default to Mood).
		{"ah řřx", true, false},
		{"ah", true, false},
	}
	for _, c := range cases {
		toks := Tokenize(c.sentence)
		mw, ok := toks[0].(ModularWord)
		if !ok {
			t.Fatalf("Tokenize(%q)[0] = %T, want ModularWord", c.sentence, toks[0])
		}
		if c.wantNil {
			if mw.MarksMood != nil {
				t.Errorf("Tokenize(%q): MarksMood = %v, want nil", c.sentence, *mw.MarksMood)
			}
			continue
		}
		if mw.MarksMood == nil {
			t.Errorf("Tokenize(%q): MarksMood = nil, want %v", c.sentence, c.wantMood)
			continue
		}
		if *mw.MarksMood != c.wantMood {
			t.Errorf("Tokenize(%q): MarksMood = %v, want %v",
				c.sentence, *mw.MarksMood, c.wantMood)
		}
	}
}

func TestTokenize_Empty(t *testing.T) {
	if tokens := Tokenize(""); len(tokens) != 0 {
		t.Errorf("Tokenize(\"\") = %v, want empty", tokens)
	}
	if tokens := Tokenize("   "); len(tokens) != 0 {
		t.Errorf("Tokenize(spaces) = %v, want empty", tokens)
	}
}

func TestRomanization(t *testing.T) {
	// Every word token preserves its original romanization.
	cases := []string{"malëuţřait", "řřx", "ha", "hai", "hla", "ah"}
	for _, w := range cases {
		tok := ClassifyWord(w)
		if tok.Romanization() != w {
			t.Errorf("token Romanization() = %q, want %q", tok.Romanization(), w)
		}
	}
}

// §4.6.2 puts the Slot 3 Specification consonant directly after the V_C,
// so a V_C in cases 37-68 is not word-final and takes its glottal-stop in
// the §1.7 Rule 1 position, after the vowel-form. SplitConjuncts then
// groups that glottal with the Spec consonant that follows it, which is
// why the case lookup has to put it back.
func TestCombinationRef_Rule1Glottal(t *testing.T) {
	cases := []struct {
		word string
		want g.Case
	}{
		// Rule 1, after the vowel-form.
		{"sa'xinļ", g.PRN},   // a+' , case 37
		{"mmia'xinļ", g.LOC}, // ia -> i'a, case 53
		// Rule 3's epenthetic spelling of the same slot, which arrives
		// merged into the vowel conjunct instead.
		{"mma'oxinļ", g.CNR}, // a'o, case 61
		// No glottal at all: the plain case. ie is series-3 form 2.
		{"mmiexinļ", g.PUR},
	}
	for _, c := range cases {
		w := ClassifyWord(c.word)
		cr, ok := w.(CombinationRefWord)
		if !ok {
			t.Errorf("ClassifyWord(%q) = %T, want CombinationRefWord", c.word, w)
			continue
		}
		if cr.Combination.Case != c.want {
			t.Errorf("%s: case = %v, want %v", c.word, cr.Combination.Case, c.want)
		}
	}
}

// §4.6.1 Slot 3 is w/y + V_C2, so a glottal-stop on V_C1 lands on the
// front of that w or y rather than word-finally. fo'we'is is the
// grammar's own example and carries one glottal in each case slot, one
// per §1.7 placement: V_C1 takes Rule 1 ("o'") and V_C2 takes Rule 3
// ("e'i").
func TestReferential_Rule1Glottal(t *testing.T) {
	cases := []struct {
		word        string
		want, want2 g.Case
	}{
		{"lai'wiš", g.ACT, g.AFF},  // ai+' , case 45
		{"fo'we'is", g.PRD, g.ESS}, // o+' , case 43; ei+' -> e'i, case 47
	}
	for _, c := range cases {
		w := ClassifyWord(c.word)
		r, ok := w.(ReferentialWord)
		if !ok {
			t.Errorf("ClassifyWord(%q) = %T, want ReferentialWord", c.word, w)
			continue
		}
		if r.Referential.Case != c.want {
			t.Errorf("%s: V_C1 = %v, want %v", c.word, r.Referential.Case, c.want)
		}
		second := r.Referential.Second
		if second == nil || second.Case != c.want2 {
			t.Errorf("%s: V_C2 = %v, want %v", c.word, second, c.want2)
		}
	}
	// Without the glottal it is a different case, not the same word.
	if r, ok := ClassifyWord("laiwiš").(ReferentialWord); !ok || r.Referential.Case != g.POS {
		t.Errorf("laiwiš: V_C1 = %v, want POS", r.Referential.Case)
	}
}
