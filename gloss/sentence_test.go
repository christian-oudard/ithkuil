package gloss

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/tokenize"
)

func TestSentence_MixedTokens(t *testing.T) {
	// Mixed: bias, formative, register opener.
	out := Sentence("řřx malëuţřait ha")
	if len(out) != 3 {
		t.Fatalf("got %d glosses, want 3", len(out))
	}
	if out[0] != "DOL(Ow! Ouch!)" {
		t.Errorf("token 0 = %q, want \"DOL(Ow! Ouch!)\"", out[0])
	}
	if !strings.HasPrefix(out[1], "-m-") {
		t.Errorf("token 1 = %q, want formative gloss starting with -m-", out[1])
	}
	if out[2] != "REG-DSV" {
		t.Errorf("token 2 = %q, want \"REG-DSV\"", out[2])
	}
}

func TestSentence_Empty(t *testing.T) {
	if out := Sentence(""); len(out) != 0 {
		t.Errorf("Sentence(\"\") = %v, want empty", out)
	}
}

func TestToken_Carrier(t *testing.T) {
	tok := tokenize.ClassifyWord("hla")
	got := (&Glosser{}).Token(tok)
	if got != "CARR-Carrier(a)" {
		t.Errorf("Token(\"hla\") = %q, want \"CARR-Carrier(a)\"", got)
	}
}

func TestToken_Modular(t *testing.T) {
	// "ah" = Vn "a" (MNO valence) + Cn "h" (FAC mood). Both default,
	// so the inner gloss is empty → "MOD".
	tok := tokenize.ClassifyWord("ah")
	got := (&Glosser{}).Token(tok)
	if got != "MOD" {
		t.Errorf("Token(\"ah\") = %q, want \"MOD\"", got)
	}
}

func TestToken_Modular_NonDefault(t *testing.T) {
	// "ähl" = Vn "ä" (PRL valence) + Cn "hl" (SUB mood).
	tok := tokenize.ClassifyWord("ähl")
	got := (&Glosser{}).Token(tok)
	if got != "MOD(PRL.SUB)" {
		t.Errorf("Token(\"ähl\") = %q, want \"MOD(PRL.SUB)\"", got)
	}
}

func TestToken_Modular_AspectPattern2(t *testing.T) {
	// "ehňw" = Vn "e" (HAB aspect) + Cn "hňw" (Pattern 2 HYP).
	// Pattern 2 → CaseScopeVal{CCV} initial parse.
	tok := tokenize.ClassifyWord("ehňw")
	got := (&Glosser{}).Token(tok)
	if got != "MOD(HAB.CCV)" {
		t.Errorf("Token(\"ehňw\") = %q, want \"MOD(HAB.CCV)\"", got)
	}
}

func TestToken_RegisterEnd(t *testing.T) {
	tok := tokenize.ClassifyWord("hai")
	got := (&Glosser{}).Token(tok)
	if got != "REG-DSV-END" {
		t.Errorf("Token(\"hai\") = %q, want \"REG-DSV-END\"", got)
	}
}

func TestToken_Referential(t *testing.T) {
	// "l" is the C1 for R1m/NEU (the speaker, "I").
	tok := tokenize.ClassifyWord("l")
	got := (&Glosser{}).Token(tok)
	if got != "REF[1m]" {
		t.Errorf("Token(\"l\") = %q, want \"REF[1m]\"", got)
	}
	// "r" is R1m/BEN — effect shown.
	tok = tokenize.ClassifyWord("r")
	got = (&Glosser{}).Token(tok)
	if got != "REF[1m/BEN]" {
		t.Errorf("Token(\"r\") = %q, want \"REF[1m/BEN]\"", got)
	}
}

func TestToken_ReferentialWithCase(t *testing.T) {
	// "lü" = R1m + DAT.
	tok := tokenize.ClassifyWord("lü")
	got := (&Glosser{}).Token(tok)
	if got != "REF[1m]-DAT" {
		t.Errorf("Token(\"lü\") = %q, want \"REF[1m]-DAT\"", got)
	}
}

func TestSentence_CarrierForeign(t *testing.T) {
	// "hnas John malá" — John passes through; carrier glosses; malá glosses.
	out := Sentence("hnas John malá")
	if len(out) != 3 {
		t.Fatalf("got %d, want 3", len(out))
	}
	if out[1] != "John" {
		t.Errorf("foreign word gloss = %q, want \"John\"", out[1])
	}
}

func TestToken_Unknown(t *testing.T) {
	// "qpqp" has only non-Ithkuil "q"s plus a non-referential "p"
	// arrangement; nothing claims it.
	tok := tokenize.ClassifyWord("qpqp")
	got := (&Glosser{}).Token(tok)
	if got != "?qpqp" {
		t.Errorf("Token(\"qpqp\") = %q, want \"?qpqp\"", got)
	}
}
