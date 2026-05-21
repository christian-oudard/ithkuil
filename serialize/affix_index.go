package serialize

import (
	"fmt"
	"sort"

	"github.com/christian-oudard/ithkuil/lexicon"
)

// affixCsToIndex / affixIndexToCs implement a stable mapping between
// affix Cs clusters and a 2-byte numeric index, derived from the
// default embedded lexicon sorted by Cs. The mapping is part of the
// binary format contract for affixes when the Cs is in the lexicon —
// any change to the lexicon's affix set requires bumping FormatVersion.
//
// AffixIndexUnknown is the sentinel 0xFFFF emitted when an affix's Cs
// is not in the lexicon; the decoder then expects a length-prefixed
// phoneme cluster (the same shape as a Cr root) to follow.
var (
	affixCsToIndex map[string]uint16
	affixIndexToCs []string
	lexiconVersion string
)

// AffixIndexUnknown signals "this affix's Cs is not in the lexicon —
// a fallback cluster encoding follows in the byte stream."
const AffixIndexUnknown uint16 = 0xFFFF

func init() {
	lex, err := lexicon.LoadDefault()
	if err != nil {
		// Leave maps nil; encode falls through to the cluster
		// fallback for every affix. Decode still works for the
		// fallback path. This branch shouldn't fire in practice
		// because the embedded lexicon is always present.
		return
	}
	lexiconVersion = lex.Version
	cs := make([]string, 0, len(lex.Affixes))
	for c := range lex.Affixes {
		cs = append(cs, c)
	}
	sort.Strings(cs)
	if len(cs) >= int(AffixIndexUnknown) {
		// 65535 affixes — vanishingly unlikely with current spec,
		// but guard against silently shadowing the sentinel.
		panic("serialize: lexicon has too many affixes for 2-byte index")
	}
	affixCsToIndex = make(map[string]uint16, len(cs))
	affixIndexToCs = cs
	for i, c := range cs {
		affixCsToIndex[c] = uint16(i)
	}
}

// EncodeAffixIndex returns the 2-byte affix index for a Cs cluster.
// Returns ok=false when the Cs is not in the default lexicon; the
// caller should emit AffixIndexUnknown plus a cluster fallback.
func EncodeAffixIndex(cs string) (uint16, bool) {
	if affixCsToIndex == nil {
		return 0, false
	}
	i, ok := affixCsToIndex[cs]
	return i, ok
}

// DecodeAffixIndex returns the Cs cluster for a 2-byte affix index.
// AffixIndexUnknown is rejected here — callers detect that value
// before calling this function and switch to cluster-decode mode.
func DecodeAffixIndex(i uint16) (string, error) {
	if i == AffixIndexUnknown {
		return "", fmt.Errorf("affix index is the unknown-affix sentinel")
	}
	if int(i) >= len(affixIndexToCs) {
		return "", fmt.Errorf("affix index out of range: %d (lexicon size %d)", i, len(affixIndexToCs))
	}
	return affixIndexToCs[i], nil
}
