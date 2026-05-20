package grammar

import "testing"

func TestMinimalFormative(t *testing.T) {
	f := MinimalFormative("ml")
	if f.SlotIII != "ml" {
		t.Errorf("SlotIII = %q, want %q", f.SlotIII, Root("ml"))
	}
	if f.SlotII != DefaultSlotII {
		t.Errorf("SlotII = %v, want %v", f.SlotII, DefaultSlotII)
	}
	if f.SlotIV != DefaultSlotIV {
		t.Errorf("SlotIV = %v, want %v", f.SlotIV, DefaultSlotIV)
	}
	if f.SlotVI != DefaultSlotVI {
		t.Errorf("SlotVI = %v, want %v", f.SlotVI, DefaultSlotVI)
	}
	if f.Stress != Penultimate {
		t.Errorf("Stress = %v, want Penultimate", f.Stress)
	}
	cs, ok := f.SlotIX.(CaseSlot)
	if !ok || cs.Case != THM {
		t.Errorf("SlotIX = %v, want CaseSlot{THM}", f.SlotIX)
	}
	if f.SlotI != nil {
		t.Errorf("SlotI = %v, want nil", f.SlotI)
	}
	if f.SlotIShortcut != nil {
		t.Errorf("SlotIShortcut = %v, want nil", f.SlotIShortcut)
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

func TestSlotIXVariants(t *testing.T) {
	// Both variants should satisfy SlotIX.
	var s SlotIX = CaseSlot{Case: ERG}
	if cs, ok := s.(CaseSlot); !ok || cs.Case != ERG {
		t.Errorf("CaseSlot mismatch: %v", s)
	}
	s = Directive{}
	if _, ok := s.(Directive); !ok {
		t.Errorf("Directive mismatch: %v", s)
	}
	s = Assertive{Validation: INF}
	if as, ok := s.(Assertive); !ok || as.Validation != INF {
		t.Errorf("Assertive mismatch: %v", s)
	}
}

func TestRootString(t *testing.T) {
	if got := Root("ml").String(); got != "ml" {
		t.Errorf("Root.String() = %q, want \"ml\"", got)
	}
}
