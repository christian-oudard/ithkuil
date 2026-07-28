package grammar

import "testing"

func TestSlotVIEnumCounts(t *testing.T) {
	if len(AllConfigurations) != 20 {
		t.Errorf("want 20 configurations, got %d", len(AllConfigurations))
	}
	if len(AllAffiliations) != 4 {
		t.Errorf("want 4 affiliations, got %d", len(AllAffiliations))
	}
	if len(AllPerspectives) != 4 {
		t.Errorf("want 4 perspectives, got %d", len(AllPerspectives))
	}
	if len(AllExtensions) != 6 {
		t.Errorf("want 6 extensions, got %d", len(AllExtensions))
	}
	if len(AllEssences) != 2 {
		t.Errorf("want 2 essences, got %d", len(AllEssences))
	}
}

func TestSlotVINonUniplexCount(t *testing.T) {
	// Sanity check the spec count: 19 non-uniplex configurations.
	n := 0
	for _, c := range AllConfigurations {
		if c != UPX {
			n++
		}
	}
	if n != 19 {
		t.Errorf("want 19 non-uniplex configs, got %d", n)
	}
}

func TestDefaultSlotVI(t *testing.T) {
	want := SlotVI{UPX, CSL, M_, DEL, NRM}
	if DefaultSlotVI != want {
		t.Errorf("DefaultSlotVI = %v, want %v", DefaultSlotVI, want)
	}
}
