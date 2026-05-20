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

func TestToken_SingleAffixWord(t *testing.T) {
	tok := tokenize.ClassifyWord("are")
	got := (&Glosser{}).Token(tok)
	if !strings.HasPrefix(got, "AFFIX[") {
		t.Errorf("Token(are) = %q, want AFFIX[...]", got)
	}
}

func TestToken_MultipleAffixWord(t *testing.T) {
	tok := tokenize.ClassifyWord("xaheitr")
	got := (&Glosser{}).Token(tok)
	if !strings.HasPrefix(got, "AFFIXES[") {
		t.Errorf("Token(xaheitr) = %q, want AFFIXES[...]", got)
	}
}

func TestToken_CombinationRef(t *testing.T) {
	tok := tokenize.ClassifyWord("ţnaxeka")
	got := (&Glosser{}).Token(tok)
	if !strings.Contains(got, "REF[") || !strings.Contains(got, ".x") {
		t.Errorf("Token(ţnaxeka) = %q, want REF[...]-...x", got)
	}
}

func TestToken_CombinationRef_WithCarrier(t *testing.T) {
	tok := tokenize.ClassifyWord("ahlax")
	got := (&Glosser{}).Token(tok)
	if !strings.Contains(got, "CARR[") {
		t.Errorf("Token(ahlax) = %q, want CARR[...]", got)
	}
}

func TestToken_Ref_WithCarrier(t *testing.T) {
	tok := tokenize.ClassifyWord("üohla")
	got := (&Glosser{}).Token(tok)
	if !strings.Contains(got, "CARR[") {
		t.Errorf("Token(üohla) = %q, want CARR[...]", got)
	}
}

func TestToken_Ref_RpvAndCase2(t *testing.T) {
	tok := tokenize.ClassifyWord("layá")
	got := (&Glosser{}).Token(tok)
	if !strings.Contains(got, "\\RPV") {
		t.Errorf("Token(layá) = %q, want \\RPV suffix", got)
	}
}

func TestToken_Concatenated(t *testing.T) {
	tok := tokenize.ClassifyWord("hamlala-amlala")
	got := (&Glosser{}).Token(tok)
	if !strings.Contains(got, " >> ") {
		t.Errorf("Token(hamlala-amlala) = %q, want \" >> \" separator", got)
	}
}

func TestFormative_CsRoot(t *testing.T) {
	tok := tokenize.ClassifyWord("oërmölá").(tokenize.FormativeWord)
	got := (&Glosser{}).Formative(tok.Formative)
	if got == "" {
		t.Fatal("Formative of oërmölá returned empty")
	}
	// CsRoot should mention DYN (function) per the test corpus and use
	// the (Cs)/degree shape.
	if !strings.Contains(got, "(") || !strings.Contains(got, "/") {
		t.Errorf("CsRoot gloss = %q; expected (Cs)/degree shape", got)
	}
}

func TestFormative_RefRoot(t *testing.T) {
	tok := tokenize.ClassifyWord("ealali").(tokenize.FormativeWord)
	got := (&Glosser{}).Formative(tok.Formative)
	if got == "" {
		t.Fatal("Formative of ealali returned empty")
	}
	// RefRoot gloss has the "-(refs)-" shape from §4.6.4 decomposition.
	if !strings.Contains(got, "(1m)") {
		t.Errorf("RefRoot gloss = %q, want \"(1m)\" segment", got)
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
