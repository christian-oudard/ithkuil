package parse

import "github.com/christian-oudard/ithkuil/grammar"

// registerByInitial maps the opening adjunct romanization to its Register.
var registerByInitial = func() map[string]grammar.Register {
	m := make(map[string]grammar.Register, len(grammar.AllRegisters))
	for _, r := range grammar.AllRegisters {
		f := grammar.RegisterInitialForm(r)
		if f != "" {
			m[f] = r
		}
	}
	return m
}()

// registerByFinal maps the closing adjunct romanization to its Register.
var registerByFinal = func() map[string]grammar.Register {
	m := make(map[string]grammar.Register, len(grammar.AllRegisters))
	for _, r := range grammar.AllRegisters {
		f := grammar.RegisterFinalForm(r)
		if f != "" {
			m[f] = r
		}
	}
	return m
}()

// ParseRegister decodes a register-opening adjunct (ha, he, hi, ho, hu).
// Returns (NRR, false) for inputs that don't match any opening form.
func ParseRegister(s string) (grammar.Register, bool) {
	r, ok := registerByInitial[s]
	return r, ok
}

// ParseRegisterFinal decodes a register-closing adjunct (hai, hei,
// hiu, hoi, hui, hüi). hüi closes any open register/carrier (END).
func ParseRegisterFinal(s string) (grammar.Register, bool) {
	r, ok := registerByFinal[s]
	return r, ok
}
