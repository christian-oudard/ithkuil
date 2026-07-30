package gloss_test

import (
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
)

// A modular adjunct's Scope and Reach are written as trailing "-{...}"
// markers, and both suffixes were reached only by the corpus sweeps.
// A corpus covers whichever values its words happen to use, which is
// not the same as covering the enum: it exercised the two that appear
// in the spec examples and nothing pinned the rest.

func modular(scope g.ModularScope, reach g.ModularReach) g.ModularAdjunct {
	return g.ModularAdjunct{
		Scope:   scope,
		Reach:   reach,
		Content: []g.SlotVIII{g.VnCnAspect{Aspect: g.RTR, MoodScope: g.FAC}},
	}
}

// TestModularScopeSuffix_EveryValue walks all three scopes. Default is
// the one that must print nothing: it means no Slot-1 prefix was
// written, so a marker would claim a distinction the word does not
// make.
func TestModularScopeSuffix_EveryValue(t *testing.T) {
	gl := &gloss.Glosser{Canonical: true}
	for _, c := range []struct {
		scope g.ModularScope
		want  string
	}{
		{g.ModularScopeDefault, ""},
		{g.ModularScopeParent, "-{" + g.ModularScopeParent.String() + "}"},
		{g.ModularScopeConcat, "-{" + g.ModularScopeConcat.String() + "}"},
	} {
		t.Run(c.scope.String(), func(t *testing.T) {
			got := gl.Word(modular(c.scope, g.ModularReachNone), nil, 0)
			if c.want == "" {
				if strings.Contains(got, "-{") {
					t.Errorf("default scope printed a marker: %q", got)
				}
				return
			}
			if !strings.HasSuffix(got, c.want) {
				t.Errorf("gloss = %q, want it to end %q", got, c.want)
			}
		})
	}
}

// TestModularReachSuffix_EveryValue walks all five reaches. None is
// the absent-V_H case and prints nothing, for the same reason.
func TestModularReachSuffix_EveryValue(t *testing.T) {
	gl := &gloss.Glosser{Canonical: true}
	for _, reach := range g.AllModularReaches {
		t.Run(reach.String(), func(t *testing.T) {
			got := gl.Word(modular(g.ModularScopeDefault, reach), nil, 0)
			marker := "-{" + reach.String() + "}"
			if reach == g.ModularReachNone {
				if strings.Contains(got, "-{") {
					t.Errorf("ModularReachNone printed a marker: %q", got)
				}
				return
			}
			if !strings.Contains(got, marker) {
				t.Errorf("gloss = %q, want it to contain %q", got, marker)
			}
		})
	}
}

// TestModularSuffixes_Combine checks the two markers do not swallow
// each other when both are set, which is the only interaction between
// them and the case a single-value test cannot reach.
func TestModularSuffixes_Combine(t *testing.T) {
	gl := &gloss.Glosser{Canonical: true}
	got := gl.Word(modular(g.ModularScopeParent, g.ModularReachAdjacent), nil, 0)
	for _, want := range []string{
		"-{" + g.ModularScopeParent.String() + "}",
		"-{" + g.ModularReachAdjacent.String() + "}",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("gloss %q is missing %q", got, want)
		}
	}
}
