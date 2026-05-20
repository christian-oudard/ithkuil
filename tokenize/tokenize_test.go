package tokenize

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

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
	if c.Carrier.Type != g.Carrier || c.Carrier.Vc != "a" {
		t.Errorf("Carrier = %v, want {Carrier, a}", c.Carrier)
	}
}

func TestClassifyWord_Modular(t *testing.T) {
	// "ah" = Vn "a" + Cn "h" → modular.
	w := ClassifyWord("ah")
	m, ok := w.(ModularWord)
	if !ok {
		t.Fatalf("ClassifyWord(\"ah\") = %T, want ModularWord", w)
	}
	if m.Modular.Vn != "a" || m.Modular.Cn != "h" {
		t.Errorf("Modular = %v, want {a, h}", m.Modular)
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
	if len(r.Refs) != 1 || r.Refs[0].Referent.String() != "1m" {
		t.Errorf("refs = %v, want [{1m, NEU}]", r.Refs)
	}
	if r.Case == nil {
		t.Fatal("Case = nil, want DAT")
	}
	if r.Case.String() != "DAT" {
		t.Errorf("Case = %v, want DAT", *r.Case)
	}
}

func TestClassifyWord_ReferentialWithoutCase(t *testing.T) {
	// Plain "l" — single-conjunct, no case.
	w := ClassifyWord("l")
	r, ok := w.(ReferentialWord)
	if !ok {
		t.Fatalf("ClassifyWord(\"l\") = %T, want ReferentialWord", w)
	}
	if r.Case != nil {
		t.Errorf("Case = %v, want nil", *r.Case)
	}
}

func TestClassifyWord_Concatenated(t *testing.T) {
	// "amlala-hamlala" — head "amlala" + Type1-concat dependent
	// "hamlala". The "h" prefix gives the dependent Slot I = Type1.
	w := ClassifyWord("amlala-hamlala")
	cf, ok := w.(ConcatenatedFormativeWord)
	if !ok {
		t.Fatalf("ClassifyWord(\"amlala-hamlala\") = %T, want ConcatenatedFormativeWord", w)
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
// We assert classifier types only — our gloss surface differs from
// theirs so a literal port isn't useful, but the classifier should at
// least agree on what kind of word each input is.
//
// Known divergences (not asserted here): "adni'lö", "la'la", "layá",
// "miyüs", "äst", "hrei", "ţnaxekka", and "çëhamala-lala" (sentence
// prefix on a concat chain) each round-trip differently from Kotlin
// and would each need spec/lexicon investigation.
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
	)
	cases := []struct {
		word string
		kind want
	}{
		{"yužgrá", formative},        // S3 verbal, **žgr** root
		{"eolaleici", formative},     // S2 PRC nominal, **l** root
		{"khe", ref},                 // Rdp/DET referential, ABS
		{"lalu", formative},          // basic **l** + IND
		{"ha", registerStart},        // DSV register open
		{"pļļ", bias},                // CMD "Funny!"
		{"çalal", formative},         // sentence prefix + **l**
		{"çëlal", formative},         // sentence prefix (ëi form) + **l**
		{"ççala", formative},         // sentence prefix + y shortcut
		{"çwala", formative},         // sentence prefix + w shortcut
		{"ihnú", modular},            // RCP.COU modular
		{"lala'a", formative},        // glottalized PRN
		{"wala'ana", formative},      // w-prefix, **l** + affix
		{"ëilal", formative},         // Cs-root (D1, **l**)
		{"oërmölá", formative},       // Cs-root CPT.DYN
		{"oërmoulá", formative},      // Cs-root CPT.DYN.FNC
		{"lála'a", formative},        // PRN + ANT (framed)
		{"hnas", carrier},            // Naming carrier (cf. TestTokenize_CarrierForeign)
		{"ţnaxeka", combref},         // [mi.BEN+2p] combination referential
		{"ţnaxekka", formative},      // same shape but kk geminate → formative
		{"amlala-hamlala", concatenated},
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
		}
		if !matched {
			t.Errorf("ClassifyWord(%q) = %s, want kind %d", c.word, typeName(w), c.kind)
		}
	}
}

func TestTokenize_CarrierForeign(t *testing.T) {
	// "hnas John malá" — "hnas" is a Naming carrier, so "John" is
	// foreign text. "malá" should still gloss normally.
	tokens := Tokenize("hnas John malá")
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

func TestSurface(t *testing.T) {
	// Every word token preserves its original surface text.
	cases := []string{"malëuţřait", "řřx", "ha", "hai", "hla", "ah"}
	for _, w := range cases {
		tok := ClassifyWord(w)
		if tok.Surface() != w {
			t.Errorf("token Surface() = %q, want %q", tok.Surface(), w)
		}
	}
}
