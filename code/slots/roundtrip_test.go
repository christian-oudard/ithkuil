package slots

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// FormativeCorpus is defined in corpus.go so the same test set can
// be consumed by higher-layer tests (fullparse, compose) that need
// to exercise their round-trip invariants on the same well-known
// words.

func TestLayout_RoundTrip(t *testing.T) {
	for _, w := range FormativeCorpus {
		l, err := Parse(w)
		if err != nil {
			t.Errorf("Parse(%q): %v", w, err)
			continue
		}
		got := Render(l)
		if got != w {
			t.Errorf("Render(Parse(%q)) = %q", w, got)
		}
	}
}

// TestLayout_SentencePrefix_csFamily covers §5.8.8: the modern cs-
// family of sentence-juncture prefixes (cs-/cse-/csw-/cscs-) is
// stripped silently by the parser the same as §3.2 ç(ë)-/çç-, since
// both are purely prosodic. csw/cscs rewrite to w/y so any shortcut
// Cc the prefix obscured remains visible to downstream parsing.
func TestLayout_SentencePrefix_csFamily(t *testing.T) {
	cases := []struct {
		in       string
		wantBody string
	}{
		{"csalal", "alal"},       // cs- before vowel
		{"cseamlala", "amlala"},  // cse- before consonant body
		{"cswamlala", "wamlala"}, // csw- = cs- + w-Cc shortcut
		{"cscsalal", "yalal"},    // cscs- = cs- + y-Cc shortcut
	}
	for _, c := range cases {
		if body := stripSentencePrefix(c.in); body != c.wantBody {
			t.Errorf("stripSentencePrefix(%q) = %q, want %q", c.in, body, c.wantBody)
		}
	}
	// Bare cs- before a consonant other than w must NOT be treated as
	// a sentence prefix per §5.8.8 (cse- is required there).
	if body := stripSentencePrefix("csmalal"); body != "csmalal" {
		t.Errorf("stripSentencePrefix(%q) = %q, want unchanged", "csmalal", body)
	}
}

// A Cr ending in a glide puts §1.6's footnote in play: Slot IV's Vr is
// Series 3 in RPS Context, so it sits directly after the y- or w- and
// dissimilates. Render has to apply that and Parse has to read it back,
// or the two arms disagree about a form the corpus attests (yuä, wöë).
func TestRoundTrip_GlideDissimilation(t *testing.T) {
	specs := []struct {
		fn   g.Function
		sp   g.Specification
		want string
	}{
		{g.STA, g.BSC, "lyuäla"}, // form 1: ia -> uä after y
		{g.STA, g.OBJ, "lyüëla"}, // form 4: iö -> üë
		{g.DYN, g.BSC, "lyuala"}, // form 9: ua is u-initial, unchanged
	}
	for _, s := range specs {
		f := g.MinimalFormative("ly")
		r := f.Root.(g.CrRoot)
		r.SlotIV = g.SlotIV{Function: s.fn, Specification: s.sp, Context: g.RPS}
		f.Root = r
		got := Render(FromGrammar(f))
		if got != s.want {
			t.Errorf("%v/%v render = %q, want %q", s.fn, s.sp, got, s.want)
		}
		l, err := Parse(got)
		if err != nil {
			t.Errorf("%q: %v", got, err)
			continue
		}
		back, _ := ToGrammar(l)
		if again := Render(FromGrammar(back)); again != got {
			t.Errorf("%q round-tripped to %q", got, again)
		}
	}
}
