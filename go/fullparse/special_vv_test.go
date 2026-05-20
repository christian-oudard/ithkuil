package fullparse

import (
	"testing"

	g "github.com/coudard/ithkuil/go/grammar"
)

// Cs-root formatives put an affix Cs in the Cr slot and encode the
// degree in Vr. The Haskell test corpus pins these shapes.

func TestCsRoot_ëilal(t *testing.T) {
	// ëi → (S1, PRC, STA). cr="l". Vr="a" → degree 1, EXS.
	f, err := ParseFormative("ëilal")
	if err != nil {
		t.Fatalf("ParseFormative(\"ëilal\") error: %v", err)
	}
	if f.SlotIII != "l" {
		t.Errorf("Cr = %q, want \"l\"", f.SlotIII)
	}
	if f.CsRootDegree == nil || *f.CsRootDegree != 1 {
		t.Errorf("CsRootDegree = %v, want 1", f.CsRootDegree)
	}
	if f.SlotII != (g.SlotII{Stem: g.S1, Version: g.PRC}) {
		t.Errorf("SlotII = %v, want (S1, PRC)", f.SlotII)
	}
	if f.SlotIV != (g.SlotIV{Function: g.STA, Specification: g.BSC, Context: g.EXS}) {
		t.Errorf("SlotIV = %v, want (STA, BSC, EXS)", f.SlotIV)
	}
}

func TestCsRoot_oërmölá(t *testing.T) {
	// oë → (S1, CPT, DYN). cr="rm". Vr="ö" → degree 6, EXS.
	// Ultimate stress on the final á → Vk = ASR/OBS.
	f, err := ParseFormative("oërmölá")
	if err != nil {
		t.Fatalf("ParseFormative(\"oërmölá\") error: %v", err)
	}
	if f.SlotIII != "rm" {
		t.Errorf("Cr = %q, want \"rm\"", f.SlotIII)
	}
	if f.CsRootDegree == nil || *f.CsRootDegree != 6 {
		t.Errorf("CsRootDegree = %v, want 6", f.CsRootDegree)
	}
	if f.SlotII != (g.SlotII{Stem: g.S1, Version: g.CPT}) {
		t.Errorf("SlotII = %v, want (S1, CPT)", f.SlotII)
	}
	if f.SlotIV != (g.SlotIV{Function: g.DYN, Specification: g.BSC, Context: g.EXS}) {
		t.Errorf("SlotIV = %v, want (DYN, BSC, EXS)", f.SlotIV)
	}
	uv, ok := f.Final.(g.UnframedVerbal)
	if !ok {
		t.Fatalf("Final = %v, want UnframedVerbal", f.Final)
	}
	as, ok := uv.Vk.(g.Assertive)
	if !ok || as.Validation != g.OBS {
		t.Errorf("Vk = %v, want Assertive{OBS}", uv.Vk)
	}
}

func TestCsRoot_oërmoulá_FNC(t *testing.T) {
	// Vr="ou" → series 2 form 6 → degree 6, FNC.
	f, err := ParseFormative("oërmoulá")
	if err != nil {
		t.Fatalf("ParseFormative(\"oërmoulá\") error: %v", err)
	}
	if f.CsRootDegree == nil || *f.CsRootDegree != 6 {
		t.Errorf("CsRootDegree = %v, want 6", f.CsRootDegree)
	}
	if f.SlotIV.Context != g.FNC {
		t.Errorf("Context = %v, want FNC", f.SlotIV.Context)
	}
}

func TestCsRoot_DegreeZero(t *testing.T) {
	// Vr="ae" → degree 0, EXS.
	f, err := ParseFormative("ëilael")
	if err != nil {
		t.Fatalf("ParseFormative(\"ëilael\") error: %v", err)
	}
	if f.CsRootDegree == nil || *f.CsRootDegree != 0 {
		t.Errorf("CsRootDegree = %v, want 0", f.CsRootDegree)
	}
}

func TestRefRoot_ealali(t *testing.T) {
	// ea → (S1, CPT). cr="l" (referential C1).
	// CsRootDegree must remain nil — this is a ref-root, not a Cs-root.
	f, err := ParseFormative("ealali")
	if err != nil {
		t.Fatalf("ParseFormative(\"ealali\") error: %v", err)
	}
	if f.SlotII != (g.SlotII{Stem: g.S1, Version: g.CPT}) {
		t.Errorf("SlotII = %v, want (S1, CPT)", f.SlotII)
	}
	if f.SlotIII != "l" {
		t.Errorf("Cr = %q, want \"l\"", f.SlotIII)
	}
	if f.CsRootDegree != nil {
		t.Errorf("CsRootDegree = %v, want nil (ref-root, not Cs-root)", *f.CsRootDegree)
	}
}

func TestRefRoot_aelali(t *testing.T) {
	// ae → (S1, PRC).
	f, err := ParseFormative("aelali")
	if err != nil {
		t.Fatalf("ParseFormative(\"aelali\") error: %v", err)
	}
	if f.SlotII != (g.SlotII{Stem: g.S1, Version: g.PRC}) {
		t.Errorf("SlotII = %v, want (S1, PRC)", f.SlotII)
	}
	if f.CsRootDegree != nil {
		t.Errorf("CsRootDegree = %v, want nil", *f.CsRootDegree)
	}
}

func TestSpecialVv_NotShortcut(t *testing.T) {
	// Shortcuts can't combine with Cs-root. "yëilal" — y prefix
	// (shortcut) + ëi as Vv. ParseSlotII rejects ëi (form 5 reserved),
	// so the shortcut path bails out with an error.
	_, err := ParseFormative("yëilal")
	if err == nil {
		t.Error("expected error: shortcuts can't use Cs-root Vv")
	}
}
