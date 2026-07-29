package compose

import (
	"path/filepath"
	"testing"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// canonicalGlosser returns a Glosser configured for canonical/input
// mode — the same one whose output we expect ParseToken to invert.
func canonicalGlosser(t *testing.T) *gloss.Glosser {
	t.Helper()
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	return &gloss.Glosser{Lex: lex, Canonical: true}
}

func TestParseToken_Bias(t *testing.T) {
	gl := canonicalGlosser(t)
	for _, b := range g.AllBiases {
		want := tokenize.BiasWord{Text: b.String(), Bias: b}
		s := gl.Token(want)
		got, err := ParseToken(s, nil)
		if err != nil {
			t.Errorf("Bias %s: ParseToken(%q) err: %v", b, s, err)
			continue
		}
		bw, ok := got.(tokenize.BiasWord)
		if !ok || bw.Bias != b {
			t.Errorf("Bias %s: got %T %+v, want BiasWord with %s", b, got, got, b)
		}
	}
}

func TestParseToken_RegisterStart(t *testing.T) {
	gl := canonicalGlosser(t)
	for _, r := range g.AllRegisters {
		if r == g.END {
			continue // END is only used for register-end tokens
		}
		want := tokenize.RegisterStartWord{Text: r.String(), Register: r}
		s := gl.Token(want)
		got, err := ParseToken(s, nil)
		if err != nil {
			t.Errorf("Register start %s: ParseToken(%q) err: %v", r, s, err)
			continue
		}
		rw, ok := got.(tokenize.RegisterStartWord)
		if !ok || rw.Register != r {
			t.Errorf("Register start %s: got %T %+v", r, got, got)
		}
	}
}

func TestParseToken_RegisterEnd(t *testing.T) {
	gl := canonicalGlosser(t)
	for _, r := range g.AllRegisters {
		want := tokenize.RegisterEndWord{Text: r.String() + "_END", Register: r}
		s := gl.Token(want)
		got, err := ParseToken(s, nil)
		if err != nil {
			t.Errorf("Register end %s: ParseToken(%q) err: %v", r, s, err)
			continue
		}
		rw, ok := got.(tokenize.RegisterEndWord)
		if !ok || rw.Register != r {
			t.Errorf("Register end %s: got %T %+v", r, got, got)
		}
	}
}

func TestParseToken_SingleAffix(t *testing.T) {
	gl := canonicalGlosser(t)
	lex, _ := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	cases := []struct {
		atype g.AffixType
		deg   int
		cs    string
		scope g.AffixScope
	}{
		{g.Type1Affix, 1, "r", g.ScopeVDom},
		{g.Type2Affix, 5, "kt", g.ScopeVIIDom},
		{g.Type3Affix, 9, "tk", g.ScopeFormative},
	}
	for _, c := range cases {
		want := tokenize.SingleAffixWord{
			Affix: g.SingleAffixAdjunct{
				Affix: g.Affix{Type: c.atype, Degree: c.deg, Consonant: c.cs},
				Scope: c.scope,
			},
		}
		s := gl.Token(want)
		got, err := ParseToken(s, lex)
		if err != nil {
			t.Errorf("Single %+v: ParseToken(%q) err: %v", c, s, err)
			continue
		}
		sw, ok := got.(tokenize.SingleAffixWord)
		if !ok || sw.Affix.Scope != c.scope || sw.Affix.Affix.Type != c.atype ||
			sw.Affix.Affix.Degree != c.deg || sw.Affix.Affix.Consonant != c.cs {
			t.Errorf("Single %+v: got %T %+v", c, got, got)
		}
	}
}

func TestParseToken_MultiAffix(t *testing.T) {
	gl := canonicalGlosser(t)
	lex, _ := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	first := g.Affix{Type: g.Type1Affix, Degree: 3, Consonant: "r"}
	rest := []g.Affix{
		{Type: g.Type2Affix, Degree: 5, Consonant: "kt"},
		{Type: g.Type1Affix, Degree: 7, Consonant: "tk"},
	}
	want := tokenize.MultipleAffixWord{
		Affixes: g.MultipleAffixAdjunct{
			First:      first,
			Rest:       rest,
			FirstScope: g.ScopeVSub,
			RestScope:  g.ScopeVIIDom,
		},
	}
	s := gl.Token(want)
	got, err := ParseToken(s, lex)
	if err != nil {
		t.Fatalf("ParseToken(%q): %v", s, err)
	}
	mw, ok := got.(tokenize.MultipleAffixWord)
	if !ok {
		t.Fatalf("got %T, want MultipleAffixWord", got)
	}
	if mw.Affixes.First != first {
		t.Errorf("First = %+v, want %+v", mw.Affixes.First, first)
	}
	if len(mw.Affixes.Rest) != len(rest) {
		t.Fatalf("Rest len = %d, want %d", len(mw.Affixes.Rest), len(rest))
	}
	for i, a := range rest {
		if mw.Affixes.Rest[i] != a {
			t.Errorf("Rest[%d] = %+v, want %+v", i, mw.Affixes.Rest[i], a)
		}
	}
	if mw.Affixes.FirstScope != g.ScopeVSub || mw.Affixes.RestScope != g.ScopeVIIDom {
		t.Errorf("scopes = (%v,%v), want (VSub,VIIDom)",
			mw.Affixes.FirstScope, mw.Affixes.RestScope)
	}
}

func TestParseToken_Referential(t *testing.T) {
	gl := canonicalGlosser(t)
	nomicCat := g.Nomic
	cases := []tokenize.ReferentialWord{
		// Plain: single referent + case
		{Referential: g.Referential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
			Case: g.THM,
		}},
		// Effect
		{Referential: g.Referential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R2m, Effect: g.BEN}}},
			Case: g.ERG,
		}},
		// A second case stacked onto the head
		{Referential: g.Referential{
			Head:   g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
			Case:   g.THM,
			Second: &g.SecondReferent{Case: g.ERG},
		}},
		// A second referent carrying its own case
		{Referential: g.Referential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
			Case: g.THM,
			Second: &g.SecondReferent{
				Case: g.IND,
				Refs: []g.PersonalRef{{Referent: g.R2m, Effect: g.NEU}},
			},
		}},
		// RpvEssence
		{Referential: g.Referential{
			Head:       g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
			Case:       g.DAT,
			RpvEssence: true,
		}},
		// A category modifier on the head
		{Referential: g.Referential{
			Head: g.PersonalHead{
				Refs:     []g.PersonalRef{{Referent: g.Rma, Effect: g.NEU}},
				Category: &nomicCat,
			},
			Case: g.ERG,
		}},
	}
	for i, want := range cases {
		s := gl.Token(want)
		got, err := ParseToken(s, nil)
		if err != nil {
			t.Errorf("case %d %q: %v", i, s, err)
			continue
		}
		rw, ok := got.(tokenize.ReferentialWord)
		if !ok {
			t.Errorf("case %d %q: got %T", i, s, got)
			continue
		}
		// Re-gloss; equal canonical strings = same data.
		got2 := gl.Token(rw)
		if got2 != s {
			t.Errorf("case %d: round-trip differs\n  first:  %s\n  second: %s", i, s, got2)
		}
	}
}

func TestParseToken_Modular(t *testing.T) {
	gl := canonicalGlosser(t)
	// All-default: empty body, MNO/FAC.
	allDefault := tokenize.ModularWord{
		Modular: g.ModularAdjunct{Scope: g.ModularScopeDefault},
	}
	// Typed: aspect + mood.
	typed := tokenize.ModularWord{
		Modular: g.ModularAdjunct{
			Content: []g.SlotVIII{g.VnCnAspect{Aspect: g.RTR, MoodScope: g.SUB}},
		},
	}
	// Scoped to parent only.
	scoped := tokenize.ModularWord{
		Modular: g.ModularAdjunct{
			Scope:   g.ModularScopeParent,
			Content: []g.SlotVIII{g.VnCnValence{Valence: g.PRL, MoodScope: g.HYP}},
		},
	}
	// With reach scope (V_H §4.3 Slot 4).
	reachCases := []g.ModularReach{
		g.ModularReachCaseMood,
		g.ModularReachCaseMoodIll,
		g.ModularReachFormative,
		g.ModularReachAdjacent,
	}
	var reachWords []tokenize.ModularWord
	for _, r := range reachCases {
		reachWords = append(reachWords, tokenize.ModularWord{
			Modular: g.ModularAdjunct{
				Reach:   r,
				Content: []g.SlotVIII{g.VnCnAspect{Aspect: g.HAB, MoodScope: g.FAC}},
			},
		})
	}
	all := append([]tokenize.ModularWord{allDefault, typed, scoped}, reachWords...)
	for _, want := range all {
		s := gl.Token(want)
		got, err := ParseToken(s, nil)
		if err != nil {
			t.Errorf("ParseToken(%q): %v", s, err)
			continue
		}
		got2 := gl.Token(got)
		if got2 != s {
			t.Errorf("round-trip differs\n  first:  %s\n  second: %s", s, got2)
		}
	}
}

func TestParseToken_MultiReferential(t *testing.T) {
	gl := canonicalGlosser(t)
	want := tokenize.ReferentialWord{Referential: g.Referential{
		Head: g.PersonalHead{Refs: []g.PersonalRef{
			{Referent: g.R1m, Effect: g.NEU},
			{Referent: g.R2p, Effect: g.BEN},
		}},
		Case: g.ERG,
	}}
	s := gl.Token(want)
	got, err := ParseToken(s, nil)
	if err != nil {
		t.Fatalf("ParseToken(%q): %v", s, err)
	}
	rw, ok := got.(tokenize.ReferentialWord)
	if !ok {
		t.Fatalf("got %T", got)
	}
	refs, _ := g.HeadRefs(rw.Referential.Head)
	if len(refs) != 2 || refs[0].Referent != g.R1m ||
		refs[1].Referent != g.R2p || refs[1].Effect != g.BEN {
		t.Errorf("Refs = %+v", refs)
	}
	got2 := gl.Token(rw)
	if got2 != s {
		t.Errorf("round-trip differs\n  first:  %s\n  second: %s", s, got2)
	}
}

func TestParseToken_CarrierHeadedReferential(t *testing.T) {
	gl := canonicalGlosser(t)
	want := tokenize.ReferentialWord{Referential: g.Referential{
		Head:   g.SuppletiveHead{Type: g.Quotative},
		Case:   g.ERG,
		Second: &g.SecondReferent{Case: g.DAT},
	}}
	s := gl.Token(want)
	got, err := ParseToken(s, nil)
	if err != nil {
		t.Fatalf("ParseToken(%q): %v", s, err)
	}
	rw, ok := got.(tokenize.ReferentialWord)
	if !ok {
		t.Fatalf("got %T (input %q)", got, s)
	}
	head, ok := rw.Referential.Head.(g.SuppletiveHead)
	if !ok || head.Type != g.Quotative {
		t.Errorf("Head = %+v", rw.Referential.Head)
	}
	if rw.Referential.Case != g.ERG {
		t.Errorf("Case = %+v", rw.Referential.Case)
	}
	if rw.Referential.Second == nil || rw.Referential.Second.Case != g.DAT {
		t.Errorf("Second = %+v", rw.Referential.Second)
	}
	got2 := gl.Token(rw)
	if got2 != s {
		t.Errorf("round-trip differs\n  first:  %s\n  second: %s", s, got2)
	}
}

func TestParseToken_CombinationRef(t *testing.T) {
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	gl := &gloss.Glosser{Lex: lex, Canonical: true}
	dative := g.DAT
	want := tokenize.CombinationRefWord{Combination: g.CombinationReferential{
		Head:    g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
		Case:    g.ERG,
		Spec:    g.BSC,
		Affixes: []g.Affix{{Type: g.Type1Affix, Degree: 3, Consonant: "r"}},
		Case2:   &dative,
	}}
	s := gl.Token(want)
	got, err := ParseToken(s, lex)
	if err != nil {
		t.Fatalf("ParseToken(%q): %v", s, err)
	}
	cw, ok := got.(tokenize.CombinationRefWord)
	if !ok {
		t.Fatalf("got %T, want CombinationRefWord", got)
	}
	comb := cw.Combination
	refs, _ := g.HeadRefs(comb.Head)
	if comb.Case != g.ERG || comb.Spec != g.BSC || len(refs) != 1 {
		t.Errorf("got %+v", comb)
	}
	// §4.6.2 puts affixes and a stacked case after the Specification.
	// Both used to be dropped on the way back, silently.
	if len(comb.Affixes) != 1 || comb.Affixes[0].Consonant != "r" ||
		comb.Affixes[0].Degree != 3 {
		t.Errorf("affixes = %+v, want one r/3", comb.Affixes)
	}
	if comb.Case2 == nil || *comb.Case2 != g.DAT {
		t.Errorf("Case2 = %+v, want DAT", comb.Case2)
	}
}

func TestParseToken_ForeignWord(t *testing.T) {
	gl := canonicalGlosser(t)
	for _, name := range []string{"John", "Emily", "Beethoven", "naïve"} {
		want := tokenize.ForeignWord{Text: name}
		s := gl.Token(want)
		got, err := ParseToken(s, nil)
		if err != nil {
			t.Errorf("ForeignWord %q: ParseToken(%q) err: %v", name, s, err)
			continue
		}
		fw, ok := got.(tokenize.ForeignWord)
		if !ok || fw.Text != name {
			t.Errorf("ForeignWord %q: got %T %+v", name, got, got)
		}
	}
}

func TestParseToken_CarrierAdjunct(t *testing.T) {
	gl := canonicalGlosser(t)
	cases := []struct {
		ct g.CarrierType
		c  g.Case
	}{
		{g.Carrier, g.THM},
		{g.Quotative, g.ERG},
		{g.Naming, g.ABS},
		{g.Phrasal, g.DAT},
	}
	for _, c := range cases {
		want := tokenize.CarrierWord{
			Carrier: g.CarrierAdjunct{Type: c.ct, Case: c.c},
		}
		s := gl.Token(want)
		got, err := ParseToken(s, nil)
		if err != nil {
			t.Errorf("Carrier %v %v: ParseToken(%q) err: %v", c.ct, c.c, s, err)
			continue
		}
		cw, ok := got.(tokenize.CarrierWord)
		if !ok || cw.Carrier.Type != c.ct || cw.Carrier.Case != c.c {
			t.Errorf("Carrier %v %v: got %T %+v", c.ct, c.c, got, got)
		}
	}
}
