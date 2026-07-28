package grammar

import "testing"

func TestMinimalFormative(t *testing.T) {
	f := MinimalFormative("ml")
	cr, ok := f.Root.(CrRoot)
	if !ok {
		t.Fatalf("Root = %v, want CrRoot", f.Root)
	}
	if cr.Cluster != "ml" {
		t.Errorf("Cluster = %q, want %q", cr.Cluster, "ml")
	}
	if cr.Stem != S1 || cr.Version != PRC {
		t.Errorf("Stem/Version = %v/%v, want S1/PRC", cr.Stem, cr.Version)
	}
	if cr.SlotIV != DefaultSlotIV {
		t.Errorf("SlotIV = %v, want %v", cr.SlotIV, DefaultSlotIV)
	}
	if f.SlotVI != DefaultSlotVI {
		t.Errorf("SlotVI = %v, want %v", f.SlotVI, DefaultSlotVI)
	}
	un, ok := f.Final.(UnframedNominal)
	if !ok || un.Case != THM {
		t.Errorf("Final = %v, want UnframedNominal{THM}", f.Final)
	}
	if f.Concat != ConcatNone {
		t.Errorf("Concat = %v, want None", f.Concat)
	}
	if f.SlotV != nil {
		t.Errorf("SlotV = %v, want nil", f.SlotV)
	}
	if f.SlotVII != nil {
		t.Errorf("SlotVII = %v, want nil", f.SlotVII)
	}
	if f.SlotVIII != nil {
		t.Errorf("SlotVIII = %v, want nil", f.SlotVIII)
	}
}

func TestFinalVariants(t *testing.T) {
	// All three Final variants should satisfy the interface.
	var f Final = UnframedNominal{Case: ERG}
	if un, ok := f.(UnframedNominal); !ok || un.Case != ERG {
		t.Errorf("UnframedNominal mismatch: %v", f)
	}
	f = FramedVerbal{Case: THM}
	if fv, ok := f.(FramedVerbal); !ok || fv.Case != THM {
		t.Errorf("FramedVerbal mismatch: %v", f)
	}
	f = UnframedVerbal{Vk: Assertive{Validation: INF}}
	uv, ok := f.(UnframedVerbal)
	if !ok {
		t.Fatalf("UnframedVerbal mismatch: %v", f)
	}
	as, ok := uv.Vk.(Assertive)
	if !ok || as.Validation != INF {
		t.Errorf("UnframedVerbal Vk = %v, want Assertive{INF}", uv.Vk)
	}
}

func TestRootVariants(t *testing.T) {
	// All three Root variants implement the Root interface.
	var r Root = DefaultCrRoot("ml")
	if cr, ok := r.(CrRoot); !ok || cr.Cluster != "ml" {
		t.Errorf("CrRoot mismatch: %v", r)
	}
	r = CsRoot{Cs: "n", Degree: 4, Version: PRC, Function: STA, Context: EXS}
	if cs, ok := r.(CsRoot); !ok || cs.Cs != "n" || cs.Degree != 4 {
		t.Errorf("CsRoot mismatch: %v", r)
	}
	r = RefRoot{C1: "l", Version: PRC, SlotIV: DefaultSlotIV}
	if rr, ok := r.(RefRoot); !ok || rr.C1 != "l" {
		t.Errorf("RefRoot mismatch: %v", r)
	}
}
