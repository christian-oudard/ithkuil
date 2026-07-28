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

func TestCaseToVcExhaustive(t *testing.T) {
	seen := map[string]Case{}
	for _, c := range AllCases {
		v := CaseToVc(c)
		if v == "" {
			t.Errorf("CaseToVc(%s) returned empty", c)
			continue
		}
		if other, dup := seen[v]; dup {
			t.Errorf("vowel %q maps to both %s and %s", v, other, c)
		}
		seen[v] = c
	}
}

func TestCaseToVcSpotChecks(t *testing.T) {
	cases := []struct {
		c    Case
		want string
	}{
		{THM, "a"},
		{IND, "u"},
		{POS, "ai"},
		{APL, "ia"},
		{FUN, "ao"},
		{PRN, "a'a"},
		{ACT, "a'i"},
		{LOC, "i'a"},
		{CNR, "a'o"},
	}
	for _, c := range cases {
		if got := CaseToVc(c.c); got != c.want {
			t.Errorf("CaseToVc(%s) = %q, want %q", c.c, got, c.want)
		}
	}
}
