package roman

import (
	"sort"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
)

// Every worked-example section from the morphology document, checked
// for what ParseWord reads. The words, and the snapshot of which ones
// fall through, live in corpus/morphology_examples.txt, which also
// carries their provenance and the notes on individual entries.
//
// Each section asserts its snapshot exactly. When the reader grows to
// handle a previously-unknown form the test fails, and you remove that
// word from the file's Unknown column; when a regression breaks a form
// that used to read, it fails the other way. Either direction is
// interesting.
func TestMorphologyCorpus(t *testing.T) {
	sections := corpus.MorphologySections()
	if len(sections) == 0 {
		t.Fatal("no sections in morphology_examples.txt")
	}
	for _, s := range sections {
		t.Run(s.Name, func(t *testing.T) {
			var got []string
			for _, w := range s.Words {
				if _, err := ParseWord(w); err != nil {
					got = append(got, w)
				}
			}
			want := append([]string(nil), s.Unknown...)
			sort.Strings(got)
			sort.Strings(want)
			if !equalStringSlice(got, want) {
				t.Errorf("%s: unclassified set drifted\n  got:  %v\n  want: %v",
					s.Name, got, want)
			}
		})
	}
}

func equalStringSlice(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
