package tokenize

import (
	"testing"

	g "github.com/coudard/ithkuil/go/grammar"
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
	if f.Formative.SlotIII != "m" {
		t.Errorf("Cr = %q, want \"m\"", f.Formative.SlotIII)
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
