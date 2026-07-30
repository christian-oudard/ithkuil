package gloss

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

func TestApplyFlag_Stem(t *testing.T) {
	f := g.MinimalFormative("ml")
	if _, err := ApplyFlag(&f, "S2"); err != nil {
		t.Fatalf("ApplyFlag S2: %v", err)
	}
	cr, ok := f.Root.(g.CrRoot)
	if !ok || cr.Stem != g.S2 {
		t.Errorf("Stem = %v, want S2", cr.Stem)
	}
}

func TestApplyFlag_Version(t *testing.T) {
	f := g.MinimalFormative("ml")
	if _, err := ApplyFlag(&f, "CPT"); err != nil {
		t.Fatalf("ApplyFlag CPT: %v", err)
	}
	cr := f.Root.(g.CrRoot)
	if cr.Version != g.CPT {
		t.Errorf("Version = %v, want CPT", cr.Version)
	}
}

func TestApplyFlag_Case(t *testing.T) {
	f := g.MinimalFormative("ml")
	if _, err := ApplyFlag(&f, "ERG"); err != nil {
		t.Fatalf("ApplyFlag ERG: %v", err)
	}
	un, ok := f.Final.(g.UnframedNominal)
	if !ok || un.Case != g.ERG {
		t.Errorf("Final = %v, want UnframedNominal{ERG}", f.Final)
	}
}

func TestApplyFlag_Illocution(t *testing.T) {
	f := g.MinimalFormative("ml")
	if _, err := ApplyFlag(&f, "DIR"); err != nil {
		t.Fatalf("ApplyFlag DIR: %v", err)
	}
	uv, ok := f.Final.(g.UnframedVerbal)
	if !ok {
		t.Fatalf("Final = %v, want UnframedVerbal{DIR}", f.Final)
	}
	if _, ok := uv.Vk.(g.Directive); !ok {
		t.Errorf("Vk = %v, want Directive", uv.Vk)
	}
}

func TestApplyFlag_Stress(t *testing.T) {
	f := g.MinimalFormative("ml")
	// PEN is the default, so applying it should be a no-op error or noop.
	// ULT, ANT, MON change Final.
	if _, err := ApplyFlag(&f, "ULT"); err != nil {
		t.Fatalf("ApplyFlag ULT: %v", err)
	}
	if _, ok := f.Final.(g.UnframedVerbal); !ok {
		t.Errorf("ULT didn't produce UnframedVerbal: %v", f.Final)
	}
}

func TestApplyFlag_UnknownReturnsError(t *testing.T) {
	f := g.MinimalFormative("ml")
	if _, err := ApplyFlag(&f, "QQQ"); err == nil {
		t.Error("ApplyFlag(QQQ) returned nil error")
	}
}

func TestApplyFlag_StemOnNonCrErrors(t *testing.T) {
	// Build a CsRoot formative and try applying S2.
	f := g.MinimalFormative("ml")
	f.Root = g.CsRoot{Cs: "ml", Degree: 5, Version: g.PRC, Function: g.STA, Context: g.EXS}
	_, err := ApplyFlag(&f, "S2")
	if err == nil {
		t.Error("ApplyFlag S2 on CsRoot didn't error")
	}
}

func TestApplyFlag_AllIllocutions(t *testing.T) {
	// Each illocution flag should produce an UnframedVerbal Final
	// with the matching Vk variant.
	cases := []struct {
		flag string
		ok   bool
	}{
		{"ASR", true}, {"DIR", true}, {"DEC", true},
		{"IRG", true}, {"VER", true}, {"ADM", true},
		{"POT", true}, {"HOR", true}, {"CNJ", true},
	}
	for _, c := range cases {
		f := g.MinimalFormative("ml")
		_, err := ApplyFlag(&f, c.flag)
		if (err == nil) != c.ok {
			t.Errorf("ApplyFlag(%s): err=%v, want ok=%v", c.flag, err, c.ok)
			continue
		}
		if c.ok {
			if _, ok := f.Final.(g.UnframedVerbal); !ok {
				t.Errorf("ApplyFlag(%s) Final = %T, want UnframedVerbal", c.flag, f.Final)
			}
		}
	}
}

func TestApplyFlag_FunctionAndSpecification(t *testing.T) {
	f := g.MinimalFormative("ml")
	if _, err := ApplyFlag(&f, "DYN"); err != nil {
		t.Fatalf("ApplyFlag DYN: %v", err)
	}
	cr := f.Root.(g.CrRoot)
	if cr.SlotIV.Function != g.DYN {
		t.Errorf("Function = %v, want DYN", cr.SlotIV.Function)
	}
	if _, err := ApplyFlag(&f, "OBJ"); err != nil {
		t.Fatalf("ApplyFlag OBJ: %v", err)
	}
	cr = f.Root.(g.CrRoot)
	if cr.SlotIV.Specification != g.OBJ {
		t.Errorf("Specification = %v, want OBJ", cr.SlotIV.Specification)
	}
}

func TestApplyFlag_Context(t *testing.T) {
	f := g.MinimalFormative("ml")
	if _, err := ApplyFlag(&f, "FNC"); err != nil {
		t.Fatalf("ApplyFlag FNC: %v", err)
	}
	cr := f.Root.(g.CrRoot)
	if cr.SlotIV.Context != g.FNC {
		t.Errorf("Context = %v, want FNC", cr.SlotIV.Context)
	}
}

func TestApplyFlag_StressVariants(t *testing.T) {
	cases := []string{"MON", "PEN", "ULT", "ANT"}
	for _, s := range cases {
		f := g.MinimalFormative("ml")
		if _, err := ApplyFlag(&f, s); err != nil {
			t.Errorf("ApplyFlag(%s): %v", s, err)
		}
	}
}

func TestApplyFlag_VersionOnCsAndRefRoots(t *testing.T) {
	for _, root := range []g.Root{
		g.CsRoot{Cs: "r", Degree: 5, Version: g.PRC, Function: g.STA, Context: g.EXS},
		g.RefRoot{Refs: []g.PersonalRef{{Referent: g.R1m}}, Version: g.PRC, SlotIV: g.DefaultSlotIV},
	} {
		f := g.MinimalFormative("ml")
		f.Root = root
		if _, err := ApplyFlag(&f, "CPT"); err != nil {
			t.Errorf("ApplyFlag CPT on %T: %v", root, err)
		}
		switch r := f.Root.(type) {
		case g.CsRoot:
			if r.Version != g.CPT {
				t.Errorf("CsRoot.Version = %v, want CPT", r.Version)
			}
		case g.RefRoot:
			if r.Version != g.CPT {
				t.Errorf("RefRoot.Version = %v, want CPT", r.Version)
			}
		}
	}
}

func TestApplyFlag_FunctionOnCsAndRefRoots(t *testing.T) {
	for _, root := range []g.Root{
		g.CsRoot{Cs: "r", Degree: 5, Version: g.PRC, Function: g.STA, Context: g.EXS},
		g.RefRoot{Refs: []g.PersonalRef{{Referent: g.R1m}}, Version: g.PRC, SlotIV: g.DefaultSlotIV},
	} {
		f := g.MinimalFormative("ml")
		f.Root = root
		if _, err := ApplyFlag(&f, "DYN"); err != nil {
			t.Errorf("ApplyFlag DYN on %T: %v", root, err)
		}
	}
}

func TestApplyFlag_SpecOnNonCrRefErrors(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.Root = g.CsRoot{Cs: "r", Degree: 5, Version: g.PRC, Function: g.STA, Context: g.EXS}
	if _, err := ApplyFlag(&f, "OBJ"); err == nil {
		t.Error("ApplyFlag OBJ on CsRoot didn't error")
	}
}

func TestApplyFlag_SpecOnRefRoot(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.Root = g.RefRoot{Refs: []g.PersonalRef{{Referent: g.R1m}}, Version: g.PRC, SlotIV: g.DefaultSlotIV}
	if _, err := ApplyFlag(&f, "OBJ"); err != nil {
		t.Errorf("ApplyFlag OBJ on RefRoot: %v", err)
	}
	rr := f.Root.(g.RefRoot)
	if rr.SlotIV.Specification != g.OBJ {
		t.Errorf("RefRoot.SlotIV.Specification = %v, want OBJ", rr.SlotIV.Specification)
	}
}

func TestApplyFlag_ContextOnCsAndRef(t *testing.T) {
	for _, root := range []g.Root{
		g.CsRoot{Cs: "r", Degree: 5, Version: g.PRC, Function: g.STA, Context: g.EXS},
		g.RefRoot{Refs: []g.PersonalRef{{Referent: g.R1m}}, Version: g.PRC, SlotIV: g.DefaultSlotIV},
	} {
		f := g.MinimalFormative("ml")
		f.Root = root
		if _, err := ApplyFlag(&f, "RPS"); err != nil {
			t.Errorf("ApplyFlag RPS on %T: %v", root, err)
		}
	}
}

func TestApplyFlag_CaseOnFramedVerbal(t *testing.T) {
	f := g.MinimalFormative("ml")
	f.Final = g.FramedVerbal{Case: g.THM}
	if _, err := ApplyFlag(&f, "ERG"); err != nil {
		t.Fatalf("ApplyFlag ERG: %v", err)
	}
	fv, ok := f.Final.(g.FramedVerbal)
	if !ok || fv.Case != g.ERG {
		t.Errorf("Final = %v, want FramedVerbal{ERG}", f.Final)
	}
}

func TestApplyFlag_MoodOnEachSlotVIIIVariant(t *testing.T) {
	// Each Mood flag should rewrite the MoodScope of whatever SlotVIII
	// variant is already present.
	variants := []g.SlotVIII{
		g.VnCnValence{Valence: g.MNO, MoodScope: g.FAC},
		g.VnCnAspect{Aspect: g.RTR, MoodScope: g.FAC},
		g.VnCnPhase{Phase: g.PCT, MoodScope: g.FAC},
		g.VnCnEffect{Effect: g.BEN1, MoodScope: g.FAC},
		g.VnCnLevel{Level: g.MIN, MoodScope: g.FAC},
	}
	for _, v := range variants {
		f := g.MinimalFormative("ml")
		f.SlotVIII = v
		if _, err := ApplyFlag(&f, "SUB"); err != nil {
			t.Errorf("ApplyFlag SUB on %T: %v", v, err)
		}
		if got := g.SlotVIIIMoodScope(f.SlotVIII); got != g.SUB {
			t.Errorf("after SUB on %T: MoodScope = %v, want SUB", v, got)
		}
	}
	// Applying a mood when SlotVIII is nil → defaults to a MNO valence.
	f := g.MinimalFormative("ml")
	if _, err := ApplyFlag(&f, "SUB"); err != nil {
		t.Fatalf("ApplyFlag SUB nil: %v", err)
	}
	if v, ok := f.SlotVIII.(g.VnCnValence); !ok || v.MoodScope != g.SUB {
		t.Errorf("ApplyFlag SUB nil: SlotVIII = %v, want VnCnValence{MNO,SUB}", f.SlotVIII)
	}
}

func TestApplyFlag_AspectValenceFlags(t *testing.T) {
	f := g.MinimalFormative("ml")
	if _, err := ApplyFlag(&f, "RTR"); err != nil {
		t.Fatalf("ApplyFlag RTR: %v", err)
	}
	if _, ok := f.SlotVIII.(g.VnCnAspect); !ok {
		t.Errorf("SlotVIII = %v, want VnCnAspect", f.SlotVIII)
	}
	f = g.MinimalFormative("ml")
	if _, err := ApplyFlag(&f, "PRL"); err != nil {
		t.Fatalf("ApplyFlag PRL: %v", err)
	}
	if _, ok := f.SlotVIII.(g.VnCnValence); !ok {
		t.Errorf("SlotVIII = %v, want VnCnValence", f.SlotVIII)
	}
}

func TestApplyFlag_ULTPreservesExistingVerbal(t *testing.T) {
	// Already verbal — applying ULT should be a no-op.
	f := g.MinimalFormative("ml")
	f.Final = g.UnframedVerbal{Vk: g.Directive{}}
	if _, err := ApplyFlag(&f, "ULT"); err != nil {
		t.Fatalf("ApplyFlag ULT: %v", err)
	}
	uv := f.Final.(g.UnframedVerbal)
	if _, ok := uv.Vk.(g.Directive); !ok {
		t.Errorf("ULT overwrote existing UnframedVerbal Vk: %v", uv.Vk)
	}
}

func TestCurrentCase_FramedAndVerbal(t *testing.T) {
	// Apply PEN to a FramedVerbal Final — currentCase should pull the case
	// out of the FramedVerbal and carry it over.
	f := g.MinimalFormative("ml")
	f.Final = g.FramedVerbal{Case: g.ERG}
	if _, err := ApplyFlag(&f, "PEN"); err != nil {
		t.Fatalf("ApplyFlag PEN: %v", err)
	}
	un, ok := f.Final.(g.UnframedNominal)
	if !ok || un.Case != g.ERG {
		t.Errorf("after PEN on FramedVerbal: Final = %v, want UnframedNominal{ERG}", f.Final)
	}
	// Verbal Final → currentCase returns THM.
	f = g.MinimalFormative("ml")
	f.Final = g.UnframedVerbal{Vk: g.Directive{}}
	if _, err := ApplyFlag(&f, "PEN"); err != nil {
		t.Fatalf("ApplyFlag PEN on verbal: %v", err)
	}
	un = f.Final.(g.UnframedNominal)
	if un.Case != g.THM {
		t.Errorf("PEN after verbal: Case = %v, want THM", un.Case)
	}
}

func TestApplyFlag_ManyCaseValues(t *testing.T) {
	// ApplyFlag for any Case abbrev should set the Final's case.
	for _, c := range []g.Case{g.THM, g.INS, g.ERG, g.DAT, g.GEN, g.LOC} {
		f := g.MinimalFormative("ml")
		if _, err := ApplyFlag(&f, c.String()); err != nil {
			t.Errorf("ApplyFlag(%s): %v", c.String(), err)
			continue
		}
		switch v := f.Final.(type) {
		case g.UnframedNominal:
			if v.Case != c {
				t.Errorf("%s: nominal case = %v, want %v", c.String(), v.Case, c)
			}
		case g.FramedVerbal:
			if v.Case != c {
				t.Errorf("%s: framed case = %v, want %v", c.String(), v.Case, c)
			}
		}
	}
}
