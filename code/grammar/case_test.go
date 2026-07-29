package grammar

import "testing"

func TestCaseCount(t *testing.T) {
	if len(AllCases) != 68 {
		t.Errorf("want 68 cases, got %d", len(AllCases))
	}
}

func TestCaseGroupCounts(t *testing.T) {
	counts := map[CaseGroup]int{}
	for _, c := range AllCases {
		counts[c.Group()]++
	}
	want := map[CaseGroup]int{
		Transrelative: 9, Appositive: 9, Associative: 9, Adverbial: 9,
		Relational: 8, Affinitive: 8, SpatioTemporal1: 8, SpatioTemporal2: 8,
	}
	for g, n := range want {
		if counts[g] != n {
			t.Errorf("%s: want %d, got %d", g, n, counts[g])
		}
	}
}
