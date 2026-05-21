package gloss

import (
	"strings"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/referentials"
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
	if !strings.Contains(got, "REF[") || !strings.Contains(got, ".BSC") {
		t.Errorf("Token(ţnaxeka) = %q, want REF[...].BSC", got)
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

func TestFormative_NumberRoot(t *testing.T) {
	// Number formatives should gloss with their decoded integer value
	// rather than a lexicon meaning.
	cases := []struct {
		word    string
		wantVal string
	}{
		{"ksalirsa", "'42'"},     // 2 + TNX/4 = 42
		{"cpalörs", "'66'"},      // 6 + TNX/6 = 66
		{"gzalui", "'100'"},      // power root for 100
		{"wapcui", "'10000'"},    // W-shortcut power root for 10000
	}
	for _, c := range cases {
		tok := tokenize.ClassifyWord(c.word).(tokenize.FormativeWord)
		got := (&Glosser{}).Formative(tok.Formative)
		if !strings.Contains(got, c.wantVal) {
			t.Errorf("gloss(%q) = %q, want substring %q", c.word, got, c.wantVal)
		}
	}
}

func TestFormative_NumberWithSPT(t *testing.T) {
	// A number formative carrying the SPT (Specified Points in
	// Calendrical Time, §6) affix should gloss with both the integer
	// value and the SPT degree's calendar label.
	cases := []struct {
		word    string
		wantVal string
	}{
		{"wučkerw", "'8th hour'"},   // 8 + SPT/3 → "8th hour"
		{"wucpirw", "'6th weekday'"}, // 6 + SPT/4 → "6th weekday"
		{"wustarsëirw", "'15th day'"}, // 5 + TNX/1 + SPT/5 → "15th day"
		{"wuzorw", "'3th month'"},    // 3 + SPT/7 → "3th month"
	}
	for _, c := range cases {
		tok, ok := tokenize.ClassifyWord(c.word).(tokenize.FormativeWord)
		if !ok {
			t.Errorf("ClassifyWord(%q) is not a FormativeWord: %T", c.word, tokenize.ClassifyWord(c.word))
			continue
		}
		got := (&Glosser{}).Formative(tok.Formative)
		if !strings.Contains(got, c.wantVal) {
			t.Errorf("gloss(%q) = %q, want substring %q", c.word, got, c.wantVal)
		}
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

func TestToken_FramedVerbalCase(t *testing.T) {
	// Construct a FramedVerbal with a non-THM case so finalSlotIX
	// renders the case (not the default-elided form).
	f := g.MinimalFormative("ml")
	f.Final = g.FramedVerbal{Case: g.ERG}
	got := (&Glosser{}).Formative(f)
	if !strings.Contains(got, "ERG") {
		t.Errorf("Framed verbal ERG gloss = %q, want ERG", got)
	}
	if !strings.Contains(got, "ANT") {
		t.Errorf("Framed verbal gloss = %q, want ANT tag", got)
	}
}

func TestToken_AllStems(t *testing.T) {
	// Drive stemIndex through every variant. Use lexicon-backed
	// glossing to actually consult the stem index.
	lex := loadLex(t)
	for _, stem := range []g.Stem{g.S0, g.S1, g.S2, g.S3} {
		f := g.MinimalFormative("ml")
		cr := f.Root.(g.CrRoot)
		cr.Stem = stem
		f.Root = cr
		got := (&Glosser{Lex: lex}).Formative(f)
		if got == "" {
			t.Errorf("stem %v: empty gloss", stem)
		}
	}
}

func TestAffix_TypeSubscripts(t *testing.T) {
	// Build single-affix-adjunct tokens with each affix Type so
	// affixTypeSubscript runs all branches.
	cases := []struct {
		name string
		t    g.AffixType
	}{
		{"a", g.Type1Affix},
		{"ai", g.Type2Affix},
		{"ia", g.Type3Affix},
	}
	for _, c := range cases {
		a := g.SingleAffixAdjunct{
			Affix: g.Affix{Type: c.t, Degree: 1, Consonant: "r"},
			Scope: g.ScopeVDom,
		}
		got := (&Glosser{}).Token(tokenize.SingleAffixWord{Text: c.name + "r", Affix: a})
		if got == "" {
			t.Errorf("SingleAffix Type=%s: empty gloss", c.t)
		}
	}
}

func TestVkTag_AllVariants(t *testing.T) {
	for _, val := range g.AllValidations {
		f := g.MinimalFormative("ml")
		f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: val}}
		got := (&Glosser{}).Formative(f)
		want := "ASR"
		if val != g.OBS {
			want = "ASR/" + val.String()
		}
		if !strings.Contains(got, want) {
			t.Errorf("ASR/%s gloss = %q, want substring %q", val, got, want)
		}
	}
	for _, vk := range g.AllVk[1:] {
		f := g.MinimalFormative("ml")
		f.Final = g.UnframedVerbal{Vk: vk}
		got := (&Glosser{}).Formative(f)
		if !strings.Contains(got, vk.Tag()) {
			t.Errorf("Vk=%T (%s) gloss = %q, want substring %q", vk, vk.Tag(), got, vk.Tag())
		}
	}
}

func TestFormative_SlotVI_AllNonDefaults(t *testing.T) {
	// Walk each Slot VI sub-field non-default so slotVI's per-field
	// emit branches all fire.
	for _, mod := range []func(*g.SlotVI){
		func(s *g.SlotVI) { s.Configuration = g.DPX },
		func(s *g.SlotVI) { s.Affiliation = g.ASO },
		func(s *g.SlotVI) { s.Perspective = g.G_ },
		func(s *g.SlotVI) { s.Extension = g.PRX },
		func(s *g.SlotVI) { s.Essence = g.RPV },
	} {
		f := g.MinimalFormative("ml")
		s := g.DefaultSlotVI
		mod(&s)
		f.SlotVI = s
		got := (&Glosser{}).Formative(f)
		if got == "" {
			t.Errorf("non-default Ca %+v: empty gloss", s)
		}
	}
}

func TestFormative_AllFinalTags(t *testing.T) {
	// Cover finalTag's three branches.
	for _, fin := range []g.Final{
		g.UnframedNominal{Case: g.THM},
		g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}},
		g.FramedVerbal{Case: g.THM},
	} {
		f := g.MinimalFormative("ml")
		f.Final = fin
		_ = (&Glosser{}).Formative(f)
	}
}

func TestFormative_TypeAndType2Concat(t *testing.T) {
	t2 := g.Type2
	f := g.MinimalFormative("ml")
	f.Concat = &t2
	got := (&Glosser{}).Formative(f)
	if !strings.Contains(got, "T2") {
		t.Errorf("Type2 concat gloss = %q, want T2", got)
	}
}

func TestAffixes_Type3RefShortcut(t *testing.T) {
	// §4.6.5: lone Type-3 affix with referential Cs renders as (refs)/deg.
	f := g.MinimalFormative("ml")
	f.SlotVII = []g.Affix{{Type: g.Type3Affix, Degree: 5, Consonant: "l"}}
	got := (&Glosser{}).Formative(f)
	if !strings.Contains(got, "(1m)/5") {
		t.Errorf("Type-3 ref shortcut gloss = %q, want (1m)/5", got)
	}
}

func TestAffixes_MultipleType1(t *testing.T) {
	// Two regular Slot VII affixes: hyphenated list, not the §4.6.5 path.
	f := g.MinimalFormative("ml")
	f.SlotVII = []g.Affix{
		{Type: g.Type1Affix, Degree: 1, Consonant: "r"},
		{Type: g.Type1Affix, Degree: 2, Consonant: "n"},
	}
	got := (&Glosser{}).Formative(f)
	// Should contain both affix labels with "-" separator.
	if !strings.Contains(got, "-") {
		t.Errorf("two affixes gloss = %q, want hyphen separator", got)
	}
}

func TestCsRootLabel_WithLex(t *testing.T) {
	lex := loadLex(t)
	f := g.MinimalFormative("ml")
	f.Root = g.CsRoot{Cs: "r", Degree: 5, Version: g.PRC, Function: g.STA, Context: g.EXS}
	got := (&Glosser{Lex: lex}).Formative(f)
	if !strings.Contains(got, "/5") {
		t.Errorf("Cs-root gloss = %q, want degree /5", got)
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

func TestFormative_NilRootPanics(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("Formative(nil Root) did not panic")
		}
	}()
	(&Glosser{}).Formative(g.Formative{Final: g.UnframedNominal{Case: g.THM}})
}

func TestFormative_NilFinalPanics(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("Formative(nil Final) did not panic")
		}
	}()
	(&Glosser{}).Formative(g.Formative{Root: g.CrRoot{Cluster: "m"}})
}

func TestRootPrefix_RefRootNonDefaultVersion(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.Root = g.RefRoot{C1: "l", Version: g.CPT, SlotIV: g.DefaultSlotIV}
	got := (&Glosser{}).Formative(f)
	if !strings.Contains(got, "CPT") {
		t.Errorf("RefRoot CPT gloss = %q, want CPT prefix", got)
	}
}

func TestRootSuffix_CsRootNonDefaultContext(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.Root = g.CsRoot{Cs: "r", Degree: 5, Version: g.PRC, Function: g.STA, Context: g.FNC}
	got := (&Glosser{}).Formative(f)
	if !strings.Contains(got, "FNC") {
		t.Errorf("CsRoot FNC gloss = %q, want FNC", got)
	}
}

func TestCrRootLabel_EmptyCluster(t *testing.T) {
	// CrRoot with empty cluster — exercises the empty-cluster early
	// return inside crRootLabel.
	f := g.MinimalFormative("ml")
	cr := f.Root.(g.CrRoot)
	cr.Cluster = ""
	f.Root = cr
	got := (&Glosser{}).Formative(f)
	// Just exercise the path; don't assert specific output.
	_ = got
}

func TestBiasLabel_EmptyExpression(t *testing.T) {
	// A bias variant whose expression-table lookup returns "" should
	// fall through to plain b.String(). Use a zero-value Bias which
	// has no surface expression.
	out := (&Glosser{}).biasLabel(g.Bias(0))
	if out == "" {
		t.Error("biasLabel(zero) returned empty")
	}
}

func TestModularLabel_AllDefault(t *testing.T) {
	// An all-default modular adjunct (no Content, default scope/reach)
	// glosses as bare "MOD".
	m := g.ModularAdjunct{}
	got := (&Glosser{}).modularLabel(m, nil)
	if got != "MOD" {
		t.Errorf("modularLabel(default) = %q, want %q", got, "MOD")
	}
}

func TestRefLabel_FullShape(t *testing.T) {
	// Build a ReferentialWord covering Case2, Category, RefB, RpvEssence.
	thm := g.THM
	erg := g.ERG
	cat := referentials.Nomic
	r := tokenize.ReferentialWord{
		Refs: []referentials.PersonalRef{
			{Referent: referentials.R1m, Effect: referentials.BEN},
		},
		Category:   &cat,
		Case:       &thm,
		Case2:      &erg,
		RefB:       []referentials.PersonalRef{{Referent: referentials.R2m, Effect: referentials.NEU}},
		RpvEssence: true,
	}
	out := (&Glosser{}).refLabel(r)
	if !strings.Contains(out, "ERG") || !strings.Contains(out, "RPV") {
		t.Errorf("refLabel full shape = %q, missing ERG or RPV", out)
	}
}

func TestCombinationRefLabel_WithAffixes(t *testing.T) {
	thm := g.THM
	c := tokenize.CombinationRefWord{
		Refs: []referentials.PersonalRef{
			{Referent: referentials.R1m, Effect: referentials.BEN},
		},
		Case:    thm,
		Spec:    g.BSC,
		Affixes: []g.Affix{{Type: g.Type1Affix, Degree: 1, Consonant: "r"}},
		Case2:   &thm,
	}
	out := (&Glosser{}).combinationRefLabel(c)
	if !strings.Contains(out, "r/1") {
		t.Errorf("combinationRefLabel = %q, want affix \"r/1\"", out)
	}
}

func TestAffixLabel_WithLexicon(t *testing.T) {
	lex := loadLex(t)
	gl := &Glosser{Lex: lex}
	// A known affix should resolve to its abbreviation.
	if got := gl.affixLabel("r"); got == "r" {
		t.Errorf("affixLabel(r) with lex = %q, expected abbreviation", got)
	}
	// An unknown affix should pass through.
	if got := gl.affixLabel("zzznonexistent"); got != "zzznonexistent" {
		t.Errorf("affixLabel(unknown) = %q, want passthrough", got)
	}
}

func TestFinalSlotIX_NilFinal(t *testing.T) {
	if got := finalSlotIX(nil); got != "" {
		t.Errorf("finalSlotIX(nil) = %q, want empty", got)
	}
}

func TestFinalTag_NilFinal(t *testing.T) {
	if got := finalTag(nil); got != "" {
		t.Errorf("finalTag(nil) = %q, want empty", got)
	}
}

func TestSlotI_UnknownConcat(t *testing.T) {
	// A non-Type1/Type2 concat status returns empty. We can't construct
	// one via the public enum, but we can pass an invalid pointer value.
	bad := g.ConcatenationStatus(99)
	if got := slotI(&bad); got != "" {
		t.Errorf("slotI(unknown) = %q, want empty", got)
	}
}
