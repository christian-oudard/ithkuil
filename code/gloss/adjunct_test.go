package gloss

import (
	"path/filepath"
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
)

// canonicalGlosser returns a Glosser configured for canonical/input
// mode — the same one whose output we expect ParseToken to invert.
func canonicalGlosser(t *testing.T) *Glosser {
	t.Helper()
	lex, err := lexicon.Load(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Fatalf("load lex: %v", err)
	}
	return &Glosser{Lex: lex, Canonical: true}
}

func TestParseToken_Bias(t *testing.T) {
	gl := canonicalGlosser(t)
	for _, b := range g.AllBiases {
		want := b
		s := gl.Token(want)
		got, err := ParseWord(s, nil)
		if err != nil {
			t.Errorf("Bias %s: ParseWord(%q) err: %v", b, s, err)
			continue
		}
		bw, ok := got.(g.Bias)
		if !ok || bw != b {
			t.Errorf("Bias %s: got %T %+v", b, got, got)
		}
	}
}

func TestParseToken_RegisterStart(t *testing.T) {
	gl := canonicalGlosser(t)
	for _, r := range g.AllRegisters {
		if r == g.END {
			continue // END is only used for register-end tokens
		}
		want := g.RegisterMarker{Register: r}
		s := gl.Token(want)
		got, err := ParseWord(s, nil)
		if err != nil {
			t.Errorf("Register start %s: ParseWord(%q) err: %v", r, s, err)
			continue
		}
		rw, ok := got.(g.RegisterMarker)
		if !ok || rw.Register != r {
			t.Errorf("Register start %s: got %T %+v", r, got, got)
		}
	}
}

func TestParseToken_RegisterEnd(t *testing.T) {
	gl := canonicalGlosser(t)
	for _, r := range g.AllRegisters {
		want := g.RegisterMarker{Register: r, End: true}
		s := gl.Token(want)
		got, err := ParseWord(s, nil)
		if err != nil {
			t.Errorf("Register end %s: ParseWord(%q) err: %v", r, s, err)
			continue
		}
		rw, ok := got.(g.RegisterMarker)
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
		want := g.SingleAffixAdjunct{
			Affix: g.Affix{Type: c.atype, Degree: c.deg, Consonant: c.cs},
			Scope: c.scope,
		}
		s := gl.Token(want)
		got, err := ParseWord(s, lex)
		if err != nil {
			t.Errorf("Single %+v: ParseWord(%q) err: %v", c, s, err)
			continue
		}
		sw, ok := got.(g.SingleAffixAdjunct)
		if !ok || sw.Scope != c.scope || sw.Affix.Type != c.atype ||
			sw.Affix.Degree != c.deg || sw.Affix.Consonant != c.cs {
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
	want := g.MultipleAffixAdjunct{
		First:      first,
		Rest:       rest,
		FirstScope: g.ScopeVSub,
		RestScope:  g.ScopeVIIDom,
	}
	s := gl.Token(want)
	got, err := ParseWord(s, lex)
	if err != nil {
		t.Fatalf("ParseWord(%q): %v", s, err)
	}
	mw, ok := got.(g.MultipleAffixAdjunct)
	if !ok {
		t.Fatalf("got %T, want a MultipleAffixAdjunct", got)
	}
	if mw.First != first {
		t.Errorf("First = %+v, want %+v", mw.First, first)
	}
	if len(mw.Rest) != len(rest) {
		t.Fatalf("Rest len = %d, want %d", len(mw.Rest), len(rest))
	}
	for i, a := range rest {
		if mw.Rest[i] != a {
			t.Errorf("Rest[%d] = %+v, want %+v", i, mw.Rest[i], a)
		}
	}
	if mw.FirstScope != g.ScopeVSub || mw.RestScope != g.ScopeVIIDom {
		t.Errorf("scopes = (%v,%v), want (VSub,VIIDom)",
			mw.FirstScope, mw.RestScope)
	}
}

func TestParseToken_Referential(t *testing.T) {
	gl := canonicalGlosser(t)
	nomicCat := g.Nomic
	cases := []g.Referential{
		// Plain: single referent + case
		g.Referential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
			Case: g.THM,
		},
		// Effect
		g.Referential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R2m, Effect: g.BEN}}},
			Case: g.ERG,
		},
		// A second case stacked onto the head
		g.Referential{
			Head:   g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
			Case:   g.THM,
			Second: &g.SecondReferent{Case: g.ERG},
		},
		// A second referent carrying its own case
		g.Referential{
			Head: g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
			Case: g.THM,
			Second: &g.SecondReferent{
				Case: g.IND,
				Refs: []g.PersonalRef{{Referent: g.R2m, Effect: g.NEU}},
			},
		},
		// RpvEssence
		g.Referential{
			Head:       g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
			Case:       g.DAT,
			RpvEssence: true,
		},
		// A category modifier on the head
		g.Referential{
			Head: g.PersonalHead{
				Refs:     []g.PersonalRef{{Referent: g.Rma, Effect: g.NEU}},
				Category: &nomicCat,
			},
			Case: g.ERG,
		},
	}
	for i, want := range cases {
		s := gl.Token(want)
		got, err := ParseWord(s, nil)
		if err != nil {
			t.Errorf("case %d %q: %v", i, s, err)
			continue
		}
		rw, ok := got.(g.Referential)
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
	allDefault := g.ModularAdjunct{Scope: g.ModularScopeDefault}
	// Typed: aspect + mood.
	typed := g.ModularAdjunct{
		Content: []g.SlotVIII{g.VnCnAspect{Aspect: g.RTR, MoodScope: g.SUB}},
	}
	// Scoped to parent only.
	scoped := g.ModularAdjunct{
		Scope:   g.ModularScopeParent,
		Content: []g.SlotVIII{g.VnCnValence{Valence: g.PRL, MoodScope: g.HYP}},
	}
	// With reach scope (V_H §4.3 Slot 4).
	reachCases := []g.ModularReach{
		g.ModularReachCaseMood,
		g.ModularReachCaseMoodIll,
		g.ModularReachFormative,
		g.ModularReachAdjacent,
	}
	var reachWords []g.ModularAdjunct
	for _, r := range reachCases {
		reachWords = append(reachWords, g.ModularAdjunct{
			Reach:   r,
			Content: []g.SlotVIII{g.VnCnAspect{Aspect: g.HAB, MoodScope: g.FAC}},
		})
	}
	all := append([]g.ModularAdjunct{allDefault, typed, scoped}, reachWords...)
	for _, want := range all {
		s := gl.Token(want)
		got, err := ParseWord(s, nil)
		if err != nil {
			t.Errorf("ParseWord(%q): %v", s, err)
			continue
		}
		got2 := gl.Token(got)
		if got2 != s {
			t.Errorf("round-trip differs\n  first:  %s\n  second: %s", s, got2)
		}
	}
}

// TestParseToken_ModularMultiPair covers §4.3's Slots 2, 3 and 4 all
// filled. The glosser writes the values hyphen-joined and
// parseModularToken splits on the hyphen to read them back, but
// looksLikeModular tested the body for a single dot, so a token with
// more than one entry never reached the parser that handles it and
// fell through to the formative path: Quijada's own "uhlaini" glossed
// to "PTI.SUB-RSM-PRG" and came back "no root in ...".
func TestParseToken_ModularMultiPair(t *testing.T) {
	gl := canonicalGlosser(t)
	want := g.ModularAdjunct{
		Content: []g.SlotVIII{
			g.VnCnValence{Valence: g.PTI, MoodScope: g.SUB},
			g.VnCnAspect{Aspect: g.RSM, MoodScope: g.FAC},
			g.VnCnAspect{Aspect: g.PRG, MoodScope: g.FAC},
		},
	}
	s := gl.Token(want)
	got, err := ParseWord(s, nil)
	if err != nil {
		t.Fatalf("ParseWord(%q): %v", s, err)
	}
	if got2 := gl.Token(got); got2 != s {
		t.Errorf("round-trip differs\n  first:  %s\n  second: %s", s, got2)
	}
}

// TestParseToken_ModularLoneAspect covers §4.3's Slot 4 filled alone,
// whose canonical gloss is a bare abbreviation. Bias and register are
// tried before the modular check, so a bare abbreviation that reaches
// it is one neither claimed — but the check required a scope or reach
// tail, so "RTR" (the adjunct written "a") went to the formative
// parser and came back "no root".
func TestParseToken_ModularLoneAspect(t *testing.T) {
	gl := canonicalGlosser(t)
	for _, asp := range []g.Aspect{g.RTR, g.PRS} {
		want := g.ModularAdjunct{
			Content: []g.SlotVIII{g.VnCnAspect{Aspect: asp, MoodScope: g.FAC}},
		}
		s := gl.Token(want)
		got, err := ParseWord(s, nil)
		if err != nil {
			t.Errorf("ParseWord(%q): %v", s, err)
			continue
		}
		if got2 := gl.Token(got); got2 != s {
			t.Errorf("round-trip differs\n  first:  %s\n  second: %s", s, got2)
		}
	}
}

func TestParseToken_MultiReferential(t *testing.T) {
	gl := canonicalGlosser(t)
	want := g.Referential{
		Head: g.PersonalHead{Refs: []g.PersonalRef{
			{Referent: g.R1m, Effect: g.NEU},
			{Referent: g.R2p, Effect: g.BEN},
		}},
		Case: g.ERG,
	}
	s := gl.Token(want)
	got, err := ParseWord(s, nil)
	if err != nil {
		t.Fatalf("ParseWord(%q): %v", s, err)
	}
	rw, ok := got.(g.Referential)
	if !ok {
		t.Fatalf("got %T", got)
	}
	refs, _ := g.HeadRefs(rw.Head)
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
	want := g.Referential{
		Head:   g.SuppletiveHead{Type: g.Quotative},
		Case:   g.ERG,
		Second: &g.SecondReferent{Case: g.DAT},
	}
	s := gl.Token(want)
	got, err := ParseWord(s, nil)
	if err != nil {
		t.Fatalf("ParseWord(%q): %v", s, err)
	}
	rw, ok := got.(g.Referential)
	if !ok {
		t.Fatalf("got %T (input %q)", got, s)
	}
	head, ok := rw.Head.(g.SuppletiveHead)
	if !ok || head.Type != g.Quotative {
		t.Errorf("Head = %+v", rw.Head)
	}
	if rw.Case != g.ERG {
		t.Errorf("Case = %+v", rw.Case)
	}
	if rw.Second == nil || rw.Second.Case != g.DAT {
		t.Errorf("Second = %+v", rw.Second)
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
	gl := &Glosser{Lex: lex, Canonical: true}
	dative := g.DAT
	want := g.CombinationReferential{
		Head:    g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
		Case:    g.ERG,
		Spec:    g.BSC,
		Affixes: []g.Affix{{Type: g.Type1Affix, Degree: 3, Consonant: "r"}},
		Case2:   &dative,
	}
	s := gl.Token(want)
	got, err := ParseWord(s, lex)
	if err != nil {
		t.Fatalf("ParseWord(%q): %v", s, err)
	}
	cw, ok := got.(g.CombinationReferential)
	if !ok {
		t.Fatalf("got %T, want CombinationRefWord", got)
	}
	comb := cw
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
		want := g.Foreign{Text: name}
		s := gl.Token(want)
		got, err := ParseWord(s, nil)
		if err != nil {
			t.Errorf("ForeignWord %q: ParseWord(%q) err: %v", name, s, err)
			continue
		}
		fw, ok := got.(g.Foreign)
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
		want := g.CarrierAdjunct{Type: c.ct, Case: c.c}
		s := gl.Token(want)
		got, err := ParseWord(s, nil)
		if err != nil {
			t.Errorf("Carrier %v %v: ParseWord(%q) err: %v", c.ct, c.c, s, err)
			continue
		}
		cw, ok := got.(g.CarrierAdjunct)
		if !ok || cw.Type != c.ct || cw.Case != c.c {
			t.Errorf("Carrier %v %v: got %T %+v", c.ct, c.c, got, got)
		}
	}
}
