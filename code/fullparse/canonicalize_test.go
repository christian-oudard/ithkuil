package fullparse_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/fullparse"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/render"
)

// TestCanonicalize asserts that a non-canonical input surface is
// parsed correctly and re-renders as the canonical equivalent. The
// parser accepts every spec-legal surface form, but the renderer
// only emits the canonical one. So fullparse(non-canonical) →
// render is *not* the identity for these inputs — by design.
//
// Each pair is (input_we_accept, canonical_output_we_produce).
func TestCanonicalize(t *testing.T) {
	pairs := []struct {
		in, want string
	}{
		// The Cc shortcut ties on syllables and length here, so it buys
		// nothing and the plain spelling stays put.
		{"mlala", "mlala"},
		{"malëuţřait", "malëuţřait"},
		// Long-form Slot VIII Mood → Cn→Ca shortcut canonical.
		{"amlalahla", "mlahla"},
		// Default Vv "a" emitted instead of elided.
		{"amlala", "mlala"},
		// §3.9.1 long-form Vc-glottal (cases 37-52). The Cc shortcut
		// elides Vr, leaving §3.9.1 nowhere to move the glottal to, so
		// the shortcut form stays three syllables while the plain form
		// drops to two. The plain form wins.
		{"mlala'a", "mla'la"},
	}
	for _, p := range pairs {
		t.Run(p.in, func(t *testing.T) {
			f, err := fullparse.Formative(p.in)
			if err != nil {
				t.Fatalf("Formative(%q): %v", p.in, err)
			}
			got := render.Formative(f)
			if got != p.want {
				t.Errorf("canonicalize(%q) = %q, want %q", p.in, got, p.want)
			}
		})
	}
}

// TestCanonicalize_LeadingVvNeedsLegalInitialCluster checks that the
// default Vv only elides when the root cluster can actually start a
// word. Eliding it moves Cr into word-initial position, where §3.1 and
// §3.2 permit a narrower set of clusters than medial position does.
func TestCanonicalize_LeadingVvNeedsLegalInitialCluster(t *testing.T) {
	pairs := []struct {
		in, want, why string
	}{
		// m- takes a following liquid (§3.2.8), so the Vv can go.
		{"amlala", "mlala", "ml- is a legal word-initial cluster"},
		// Word-initial r- takes only -w or -y (§3.2.9), so it can't.
		{"ardvilëilḑá", "ardvilëilḑá", "rdv- is not"},
		// §3.4 admits no tetra-conjunct whose tri-prefix is "kţg".
		{"akţgyiva", "akţgyiv", "kţgy- is not (the trailing THM Vc still elides)"},
	}
	for _, p := range pairs {
		f, err := fullparse.Formative(p.in)
		if err != nil {
			t.Fatalf("Formative(%q): %v", p.in, err)
		}
		if got := render.Formative(f); got != p.want {
			t.Errorf("render(%q) = %q, want %q (%s)", p.in, got, p.want, p.why)
		}
	}
}

// TestCanonicalize_LeadingVvKeptForSentencePrefixRoots checks that a
// root beginning with ç- or cs- keeps its Vv. Those are the §1.3.2
// sentence-juncture markers, which the parser strips, so exposing one
// at the start of a word would cost the root its first consonant on
// the way back in.
func TestCanonicalize_LeadingVvKeptForSentencePrefixRoots(t *testing.T) {
	for _, in := range []string{"açmuliwá", "açpulúgmö"} {
		f, err := fullparse.Formative(in)
		if err != nil {
			t.Fatalf("Formative(%q): %v", in, err)
		}
		out := render.Formative(f)
		back, err := fullparse.Formative(out)
		if err != nil {
			t.Fatalf("re-parse of %q: %v", out, err)
		}
		cr, ok := f.Root.(g.CrRoot)
		if !ok {
			t.Fatalf("%q: root is %T, want CrRoot", in, f.Root)
		}
		backCr, ok := back.Root.(g.CrRoot)
		if !ok || backCr.Cluster != cr.Cluster {
			t.Errorf("%q rendered as %q, which re-parses with root %v, want %q",
				in, out, back.Root, cr.Cluster)
		}
	}
}
