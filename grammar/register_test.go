package grammar

import "testing"

func TestRegisterCount(t *testing.T) {
	if len(AllRegisters) != 7 {
		t.Errorf("AllRegisters = %d, want 7", len(AllRegisters))
	}
}

func TestRegisterInitialForms(t *testing.T) {
	cases := []struct {
		r    Register
		want string
	}{
		{NRR, ""},
		{DSV, "ha"},
		{PNT, "he"},
		{SPF, "hi"},
		{EXM, "ho"},
		{CGT, "hu"},
		{END, ""},
	}
	for _, c := range cases {
		if got := RegisterInitialForm(c.r); got != c.want {
			t.Errorf("RegisterInitialForm(%s) = %q, want %q", c.r, got, c.want)
		}
	}
}

func TestRegisterFinalForms(t *testing.T) {
	cases := []struct {
		r    Register
		want string
	}{
		{NRR, ""},
		{DSV, "hai"},
		{PNT, "hei"},
		{SPF, "hiu"},
		{EXM, "hoi"},
		{CGT, "hui"},
		{END, "hüi"},
	}
	for _, c := range cases {
		if got := RegisterFinalForm(c.r); got != c.want {
			t.Errorf("RegisterFinalForm(%s) = %q, want %q", c.r, got, c.want)
		}
	}
}
