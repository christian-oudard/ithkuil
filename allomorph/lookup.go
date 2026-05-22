package allomorph

import (
	g "github.com/christian-oudard/ithkuil/grammar"
)

// CaForward maps each SlotVI to its surface Ca consonant cluster.
// Populated at package init time by enumerating all 3840 combinations
// and running ConstructCa on each.
var CaForward map[g.SlotVI]string

// CaReverse maps each surface Ca cluster to a SlotVI. When multiple
// SlotVI values produce the same cluster, the first one encountered
// in (Configuration, Affiliation, Perspective, Extension, Essence)
// declaration order wins.
var CaReverse map[string]g.SlotVI

// CaUngeminate maps each geminated Ca cluster back to its bare form.
// Useful for string-level layers that want to recover the un-geminated
// surface form without going through the SlotVI value.
var CaUngeminate map[string]string

func init() {
	const expected = 20 * 4 * 4 * 6 * 2 // 3840
	CaForward = make(map[g.SlotVI]string, expected)
	CaReverse = make(map[string]g.SlotVI, expected)
	CaUngeminate = make(map[string]string, expected)
	for _, c := range g.AllConfigurations {
		for _, a := range g.AllAffiliations {
			for _, p := range g.AllPerspectives {
				for _, e := range g.AllExtensions {
					for _, es := range g.AllEssences {
						s := g.SlotVI{
							Configuration: c,
							Affiliation:   a,
							Perspective:   p,
							Extension:     e,
							Essence:       es,
						}
						cluster := ConstructCa(s)
						CaForward[s] = cluster
						if _, exists := CaReverse[cluster]; !exists {
							CaReverse[cluster] = s
						}
						gem := GeminateCa(cluster)
						if _, exists := CaUngeminate[gem]; !exists {
							CaUngeminate[gem] = cluster
						}
					}
				}
			}
		}
	}
}

// ParseCa decodes a Ca consonant cluster to its SlotVI. Returns false
// for clusters that are not produced by any valid SlotVI combination.
func ParseCa(cluster string) (g.SlotVI, bool) {
	s, ok := CaReverse[cluster]
	return s, ok
}
