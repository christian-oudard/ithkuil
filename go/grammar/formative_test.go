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
	un, ok := f.Final.(UnframedNominal)
	if !ok || un.Case != THM {
		t.Errorf("Final = %v, want UnframedNominal{THM}", f.Final)
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

func TestRootString(t *testing.T) {
	if got := Root("ml").String(); got != "ml" {
		t.Errorf("Root.String() = %q, want \"ml\"", got)
	}
}
