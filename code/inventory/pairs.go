package inventory

import (
	"reflect"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// Combination is one formative carrying two grammatical values at once,
// named by the coordinates they occupy.
//
// Samples varies one value off a fixed baseline, which is what makes a
// failure legible: exactly one thing changed, so exactly one thing was
// lost. That is also its limit. A defect that only appears when two
// slots are filled together is invisible to it, and the Slot V against
// Slot VII collapse was exactly one — the same affix in either slot
// means different things, an all-default C_A elides, and with nothing
// between them the two spellings fell together. A person found that,
// not a sweep.
type Combination struct {
	A, B  string // "Axis/Abbrev" coordinates
	Word  g.Formative
	Marks int // how many of the two are marked rather than default
}

// Pairs returns one formative per combination of values drawn from two
// different axes.
//
// Axes that interfere are skipped, and which those are is derived
// rather than listed: two edits belong to independent dimensions
// exactly when applying them in either order gives the same formative.
// Phase and Aspect both write Slot VIII, so the second wins and the
// pair says nothing about either; Case and Illocution both write Slot
// IX. A hand-kept exclusion list would be one more thing to keep
// current, and would go stale the first time a field moved.
func Pairs() []Combination {
	axes := Axes()
	var out []Combination
	for i, a := range axes {
		for _, b := range axes[i+1:] {
			if !independent(a, b) {
				continue
			}
			base := nominal()
			if a.Verbal || b.Verbal {
				base = verbal()
			}
			for _, va := range a.Values {
				for _, vb := range b.Values {
					f := base
					va.Apply(&f)
					vb.Apply(&f)
					marks := 0
					for _, v := range []Value{va, vb} {
						if !v.Default {
							marks++
						}
					}
					out = append(out, Combination{
						A:     a.Name + "/" + va.Abbrev,
						B:     b.Name + "/" + vb.Abbrev,
						Word:  f,
						Marks: marks,
					})
				}
			}
		}
	}
	return out
}

// independent reports whether two axes edit disjoint parts of a
// formative, by checking that their first non-default values commute.
// The defaults are no help: two of them commute whatever they write,
// since neither writes anything.
func independent(a, b Axis) bool {
	va, aok := firstMarked(a)
	vb, bok := firstMarked(b)
	if !aok || !bok {
		return false
	}
	base := nominal()
	if a.Verbal || b.Verbal {
		base = verbal()
	}
	ab, ba := base, base
	va.Apply(&ab)
	vb.Apply(&ab)
	vb.Apply(&ba)
	va.Apply(&ba)
	return reflect.DeepEqual(ab, ba)
}

func firstMarked(a Axis) (Value, bool) {
	for _, v := range a.Values {
		if !v.Default {
			return v, true
		}
	}
	return Value{}, false
}
