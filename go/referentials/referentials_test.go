package referentials

import (
	"reflect"
	"testing"
)

func TestReferentCount(t *testing.T) {
	if len(AllReferents) != 11 {
		t.Errorf("AllReferents = %d, want 11", len(AllReferents))
	}
}

func TestRefC1RoundTrip(t *testing.T) {
	// Every (Referent, Effect) pair should produce a non-empty form
	// that reverse-looks-up to the same pair.
	for _, r := range AllReferents {
		for _, e := range AllEffects {
			pr := PersonalRef{Referent: r, Effect: e}
			form := RefC1(pr)
			if form == "" {
				t.Errorf("RefC1(%v, %v) is empty", r, e)
				continue
			}
			back, ok := LookupRefC1(form)
			if !ok || back != pr {
				t.Errorf("round trip %v: %q → %v (ok=%v)", pr, form, back, ok)
			}
		}
	}
}

func TestLookupRefC1_AlternateForm(t *testing.T) {
	// "ļ" is an alternate form for pi.NEU (alongside "ẓ").
	got, ok := LookupRefC1("ļ")
	if !ok {
		t.Fatal("LookupRefC1(\"ļ\") failed")
	}
	want := PersonalRef{Rpi, NEU}
	if got != want {
		t.Errorf("LookupRefC1(\"ļ\") = %v, want %v", got, want)
	}
}

func TestDecomposeRefCluster_Single(t *testing.T) {
	got, ok := DecomposeRefCluster("l")
	if !ok || len(got) != 1 || got[0] != (PersonalRef{R1m, NEU}) {
		t.Errorf("decompose(\"l\") = %v ok=%v, want [{R1m,NEU}]", got, ok)
	}
}

func TestDecomposeRefCluster_Bi(t *testing.T) {
	// "ll" should resolve as Robv/NEU (biconsonantal), not two l's.
	got, ok := DecomposeRefCluster("ll")
	if !ok || len(got) != 1 || got[0] != (PersonalRef{Robv, NEU}) {
		t.Errorf("decompose(\"ll\") = %v ok=%v, want [{Robv,NEU}]", got, ok)
	}
}

func TestDecomposeRefCluster_Chain(t *testing.T) {
	// "ls" = R1m/NEU + R2m/NEU
	got, ok := DecomposeRefCluster("ls")
	want := []PersonalRef{{R1m, NEU}, {R2m, NEU}}
	if !ok || !reflect.DeepEqual(got, want) {
		t.Errorf("decompose(\"ls\") = %v ok=%v, want %v", got, ok, want)
	}
}

func TestDecomposeRefCluster_BiThenMono(t *testing.T) {
	// "lls" = Robv/NEU then R2m/NEU
	got, ok := DecomposeRefCluster("lls")
	want := []PersonalRef{{Robv, NEU}, {R2m, NEU}}
	if !ok || !reflect.DeepEqual(got, want) {
		t.Errorf("decompose(\"lls\") = %v ok=%v, want %v", got, ok, want)
	}
}

func TestDecomposeRefCluster_Invalid(t *testing.T) {
	if _, ok := DecomposeRefCluster("xyzzz"); ok {
		t.Error("decompose(\"xyzzz\") returned ok=true, want false")
	}
}

func TestDecomposeRefWithCategory_Plain(t *testing.T) {
	cat, refs, ok := DecomposeRefWithCategory("l")
	if !ok {
		t.Fatal("decompose(\"l\") failed")
	}
	if cat != nil {
		t.Errorf("plain decompose returned category %v, want nil", *cat)
	}
	if len(refs) != 1 || refs[0] != (PersonalRef{R1m, NEU}) {
		t.Errorf("refs = %v, want [{R1m,NEU}]", refs)
	}
}

func TestDecomposeRefWithCategory_PrefixAGM(t *testing.T) {
	cat, refs, ok := DecomposeRefWithCategory("tļl")
	if !ok {
		t.Fatal("decompose(\"tļl\") failed")
	}
	if cat == nil || *cat != Agglomerative {
		t.Errorf("category = %v, want Agglomerative", cat)
	}
	if len(refs) != 1 || refs[0] != (PersonalRef{R1m, NEU}) {
		t.Errorf("refs = %v, want [{R1m,NEU}]", refs)
	}
}

func TestDecomposeRefWithCategory_SuffixNOM(t *testing.T) {
	cat, refs, ok := DecomposeRefWithCategory("lx")
	if !ok {
		t.Fatal("decompose(\"lx\") failed")
	}
	if cat == nil || *cat != Nomic {
		t.Errorf("category = %v, want Nomic", cat)
	}
	if len(refs) != 1 || refs[0] != (PersonalRef{R1m, NEU}) {
		t.Errorf("refs = %v, want [{R1m,NEU}]", refs)
	}
}

func TestReferentLabels(t *testing.T) {
	cases := []struct {
		r    Referent
		ab   string
		full string
	}{
		{R1m, "1m", "I"},
		{R2m, "2m", "you(sg.)"},
		{Rrdp, "Rdp", "aforementioned"},
		{Rpvs, "PVS", "whatever"},
	}
	for _, c := range cases {
		if c.r.String() != c.ab {
			t.Errorf("%v.String() = %q, want %q", c.r, c.r.String(), c.ab)
		}
		if c.r.Label() != c.full {
			t.Errorf("%v.Label() = %q, want %q", c.r, c.r.Label(), c.full)
		}
	}
}
