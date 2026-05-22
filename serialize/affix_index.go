package serialize

import (
	"fmt"
	"sort"

	"github.com/christian-oudard/ithkuil/lexicon"
)

// affixCsToIndex / affixIndexToCs implement a stable mapping between
// affix Cs clusters and a 2-byte numeric index, derived from a lexicon
// sorted by Cs. The mapping is part of the binary format contract —
// any change to the affix set requires bumping FormatVersion.
//
// Call InitAffixIndex once at startup before encoding or decoding.
// Without initialization, all affixes use the cluster fallback (ok=false
// from EncodeAffixIndex), which is safe but less compact.
//
// AffixIndexUnknown is the sentinel 0xFFFF emitted when an affix's Cs
// is not indexed; the decoder then expects a length-prefixed phoneme
// cluster (the same shape as a Cr root) to follow.
var (
	affixCsToIndex map[string]uint16
	affixIndexToCs []string
	lexiconVersion uint16
)

// AffixIndexUnknown signals "this affix's Cs is not indexed — a
// fallback cluster encoding follows in the byte stream."
const AffixIndexUnknown uint16 = 0xFFFF

// InitAffixIndex populates the affix index from lex. Call once at startup.
func InitAffixIndex(lex *lexicon.Lexicon) {
	lexiconVersion = lex.Version
	cs := make([]string, 0, len(lex.Affixes))
	for c := range lex.Affixes {
		cs = append(cs, c)
	}
	sort.Strings(cs)
	if len(cs) >= int(AffixIndexUnknown) {
		panic("serialize: lexicon has too many affixes for 2-byte index")
	}
	affixCsToIndex = make(map[string]uint16, len(cs))
	affixIndexToCs = cs
	for i, c := range cs {
		affixCsToIndex[c] = uint16(i)
	}
}

// EncodeAffixIndex returns the 2-byte affix index for a Cs cluster.
// Returns ok=false when uninitialized or Cs is unknown; caller should
// emit AffixIndexUnknown plus a cluster fallback.
func EncodeAffixIndex(cs string) (uint16, bool) {
	if affixCsToIndex == nil {
		return 0, false
	}
	i, ok := affixCsToIndex[cs]
	return i, ok
}

// DecodeAffixIndex returns the Cs cluster for a 2-byte affix index.
// AffixIndexUnknown is rejected — callers detect that value first and
// switch to cluster-decode mode.
func DecodeAffixIndex(i uint16) (string, error) {
	if i == AffixIndexUnknown {
		return "", fmt.Errorf("affix index is the unknown-affix sentinel")
	}
	if int(i) >= len(affixIndexToCs) {
		return "", fmt.Errorf("affix index out of range: %d (lexicon size %d)", i, len(affixIndexToCs))
	}
	return affixIndexToCs[i], nil
}
