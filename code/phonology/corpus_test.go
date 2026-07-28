package phonology_test

import (
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/phonology"
)

// Every word in the official example set was written by Quijada, so
// anything the validator rejects here is the validator being wrong
// about the phonotactics, not the corpus being wrong about Ithkuil.
//
// This caught four over-rejections at once: §2.9 read as symmetric
// (weščayá, žžjádu'u), §2.5 read across the fricative/affricate
// boundary (arţtudëužči'a), §3 applied to word-initial geminates that
// §3's own opening line hands to §6 (rrala and its paradigm), and
// concatenation chains validated as a single hyphenated word rather
// than link by link (hlellwoehú-alxwädé).
func TestCorpus_EveryWordValidates(t *testing.T) {
	words := corpus.Words()
	if len(words) < 500 {
		t.Fatalf("corpus.Words() = %d words, expected the full corpus", len(words))
	}
	for _, w := range words {
		if err := phonology.CheckText(w); err != nil {
			t.Errorf("%s rejected: %v", w, err)
		}
	}
}
