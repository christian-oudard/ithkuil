package roman

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/parse"
)

// Cs-root formatives put an affix Cs in the Cr slot and encode the
// degree in Vr. The grammar exposes them as g.CsRoot.

func TestCsRoot_Processual_Static_Degree1(t *testing.T) {
	// ëi → (PRC, STA). cs="l". Vr="a" → degree 1, EXS.
	f, err := ParseFormative("ëilal")
	if err != nil {
		t.Fatalf("Formative(\"ëilal\") error: %v", err)
	}
	cs, ok := f.Root.(g.CsRoot)
	if !ok {
		t.Fatalf("Root = %v, want CsRoot", f.Root)
	}
	if cs.Cs != "l" {
		t.Errorf("Cs = %q, want \"l\"", cs.Cs)
	}
	if cs.Degree != 1 {
		t.Errorf("Degree = %d, want 1", cs.Degree)
	}
	if cs.Version != g.PRC || cs.Function != g.STA || cs.Context != g.EXS {
		t.Errorf("Version/Function/Context = %v/%v/%v, want PRC/STA/EXS",
			cs.Version, cs.Function, cs.Context)
	}
}

func TestCsRoot_Completive_Dynamic_Degree6_Assertive(t *testing.T) {
	// oë → (CPT, DYN). cs="rm". Vr="ö" → degree 6, EXS.
	// Ultimate stress on the final á → Vk = ASR/OBS.
	f, err := ParseFormative("oërmölá")
	if err != nil {
		t.Fatalf("Formative(\"oërmölá\") error: %v", err)
	}
	cs, ok := f.Root.(g.CsRoot)
	if !ok {
		t.Fatalf("Root = %v, want CsRoot", f.Root)
	}
	if cs.Cs != "rm" || cs.Degree != 6 {
		t.Errorf("CsRoot{Cs:%q, Degree:%d}, want {rm, 6}", cs.Cs, cs.Degree)
	}
	if cs.Version != g.CPT || cs.Function != g.DYN || cs.Context != g.EXS {
		t.Errorf("Version/Function/Context = %v/%v/%v, want CPT/DYN/EXS",
			cs.Version, cs.Function, cs.Context)
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

func TestCsRoot_Degree6_Functional(t *testing.T) {
	// Vr="ou" → series 2 form 6 → degree 6, FNC.
	f, err := ParseFormative("oërmoulá")
	if err != nil {
		t.Fatalf("Formative(\"oërmoulá\") error: %v", err)
	}
	cs, ok := f.Root.(g.CsRoot)
	if !ok {
		t.Fatalf("Root = %v, want CsRoot", f.Root)
	}
	if cs.Degree != 6 || cs.Context != g.FNC {
		t.Errorf("Degree/Context = %d/%v, want 6/FNC", cs.Degree, cs.Context)
	}
}

func TestCsRoot_DegreeZero(t *testing.T) {
	// Vr="ae" → degree 0, EXS.
	f, err := ParseFormative("ëilael")
	if err != nil {
		t.Fatalf("Formative(\"ëilael\") error: %v", err)
	}
	cs, ok := f.Root.(g.CsRoot)
	if !ok || cs.Degree != 0 {
		t.Errorf("Root = %v, want CsRoot{Degree:0}", f.Root)
	}
}

func TestRefRoot_ealali(t *testing.T) {
	// ea → (CPT). C1="l".
	f, err := ParseFormative("ealali")
	if err != nil {
		t.Fatalf("Formative(\"ealali\") error: %v", err)
	}
	rr, ok := f.Root.(g.RefRoot)
	if !ok {
		t.Fatalf("Root = %v, want RefRoot", f.Root)
	}
	if parse.RefCluster(rr.Refs) != "l" {
		t.Errorf("C1 = %q, want \"l\"", parse.RefCluster(rr.Refs))
	}
	if rr.Version != g.CPT {
		t.Errorf("Version = %v, want CPT", rr.Version)
	}
}

func TestRefRoot_aelali(t *testing.T) {
	// ae → (PRC).
	f, err := ParseFormative("aelali")
	if err != nil {
		t.Fatalf("Formative(\"aelali\") error: %v", err)
	}
	rr, ok := f.Root.(g.RefRoot)
	if !ok || rr.Version != g.PRC {
		t.Errorf("Root = %v, want RefRoot{Version:PRC}", f.Root)
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
