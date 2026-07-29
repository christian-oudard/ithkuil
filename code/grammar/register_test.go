package grammar

import "testing"

func TestRegisterCount(t *testing.T) {
	if len(AllRegisters) != 7 {
		t.Errorf("AllRegisters = %d, want 7", len(AllRegisters))
	}
}
