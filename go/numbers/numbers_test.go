package numbers

import "testing"

func TestNumberRoot_DirectDigits(t *testing.T) {
	cases := []struct {
		n    int
		want string
	}{
		{0, "vr"},
		{1, "ll"},
		{2, "ks"},
		{3, "z"},
		{4, "pš"},
		{5, "st"},
		{10, "j"},
	}
	for _, c := range cases {
		got, ok := NumberRoot(c.n)
		if !ok || got != c.want {
			t.Errorf("NumberRoot(%d) = (%q,%v), want (%q,true)",
				c.n, got, ok, c.want)
		}
	}
}

func TestNumberRoot_TensOnesDigit(t *testing.T) {
	// 11-99 returns the ones-digit root.
	cases := []struct {
		n    int
		want string
	}{
		{11, "ll"}, // 1
		{20, "vr"}, // 0
		{25, "st"}, // 5
		{99, "lẓ"}, // 9
	}
	for _, c := range cases {
		got, ok := NumberRoot(c.n)
		if !ok || got != c.want {
			t.Errorf("NumberRoot(%d) = (%q,%v), want (%q,true)",
				c.n, got, ok, c.want)
		}
	}
}

func TestNumberRoot_OutOfRange(t *testing.T) {
	for _, n := range []int{-1, 100, 1000} {
		if _, ok := NumberRoot(n); ok {
			t.Errorf("NumberRoot(%d) ok=true, want false", n)
		}
	}
}

func TestNumberAffix(t *testing.T) {
	// 0-10: no TNX affix.
	for n := 0; n <= 10; n++ {
		if _, _, ok := NumberAffix(n); ok {
			t.Errorf("NumberAffix(%d) ok=true, want false", n)
		}
	}
	// 11-99: cs="rs", degree=tens.
	cases := []struct {
		n   int
		deg int
	}{
		{11, 1}, {20, 2}, {59, 5}, {99, 9},
	}
	for _, c := range cases {
		cs, deg, ok := NumberAffix(c.n)
		if !ok || cs != "rs" || deg != c.deg {
			t.Errorf("NumberAffix(%d) = (%q,%d,%v), want (\"rs\",%d,true)",
				c.n, cs, deg, ok, c.deg)
		}
	}
	// >=100: no.
	if _, _, ok := NumberAffix(100); ok {
		t.Error("NumberAffix(100) ok=true, want false")
	}
}

func TestNumberVv(t *testing.T) {
	cases := []struct {
		s    NumberStem
		v    NumberVersion
		want string
	}{
		{NSCardinal, NVConcrete, "a"},
		{NSCardinal, NVAbstract, "u"},
		{NSOrdinal, NVConcrete, "e"},
		{NSOrdinal, NVAbstract, "i"},
		{NSPartitive, NVConcrete, "o"},
		{NSPartitive, NVAbstract, "ö"},
		{NSCollective, NVConcrete, "ä"},
		{NSCollective, NVAbstract, "ü"},
	}
	for _, c := range cases {
		if got := NumberVv(c.s, c.v); got != c.want {
			t.Errorf("NumberVv(%v,%v) = %q, want %q", c.s, c.v, got, c.want)
		}
	}
}

func TestConstructNumber(t *testing.T) {
	cases := []struct {
		n    int
		s    NumberStem
		v    NumberVersion
		want string
	}{
		// 0 cardinal concrete: vv="a", cr="vr", vr="a", ca="l" → "avral"
		{0, NSCardinal, NVConcrete, "avral"},
		// 5 ordinal concrete: vv="e", cr="st", vr="a", ca="l" → "estal"
		{5, NSOrdinal, NVConcrete, "estal"},
		// 10 cardinal: vv="a", cr="j", vr="a", ca="l" → "ajal"
		{10, NSCardinal, NVConcrete, "ajal"},
		// 99 cardinal: vv="a", cr="lẓ" (ones=9), vr="a", ca="l" → "alẓal"
		{99, NSCardinal, NVConcrete, "alẓal"},
	}
	for _, c := range cases {
		got, ok := ConstructNumber(c.n, c.s, c.v)
		if !ok || got != c.want {
			t.Errorf("ConstructNumber(%d,%v,%v) = (%q,%v), want (%q,true)",
				c.n, c.s, c.v, got, ok, c.want)
		}
	}
}

func TestConstructNumber_OutOfRange(t *testing.T) {
	if _, ok := ConstructNumber(-1, NSCardinal, NVConcrete); ok {
		t.Error("ConstructNumber(-1) ok=true, want false")
	}
	if _, ok := ConstructNumber(100, NSCardinal, NVConcrete); ok {
		t.Error("ConstructNumber(100) ok=true, want false")
	}
}

func TestPowerRootsAndAffixTables(t *testing.T) {
	if len(PowerRoots) != 5 {
		t.Errorf("PowerRoots = %d, want 5", len(PowerRoots))
	}
	if len(MonthAffixes) != 12 {
		t.Errorf("MonthAffixes = %d, want 12", len(MonthAffixes))
	}
	if len(DayOfWeekAffixes) != 7 {
		t.Errorf("DayOfWeekAffixes = %d, want 7", len(DayOfWeekAffixes))
	}
	if MonthAffixes[0] != "lks" {
		t.Errorf("MonthAffixes[0] = %q, want \"lks\"", MonthAffixes[0])
	}
	if DayOfWeekAffixes[6] != "mčk" {
		t.Errorf("DayOfWeekAffixes[6] = %q, want \"mčk\"", DayOfWeekAffixes[6])
	}
}

func TestParseNumberRoot(t *testing.T) {
	cases := []struct {
		in   string
		want int
	}{
		{"vr", 0}, {"ll", 1}, {"st", 5}, {"j", 10},
	}
	for _, c := range cases {
		got, ok := ParseNumberRoot(c.in)
		if !ok || got != c.want {
			t.Errorf("ParseNumberRoot(%q) = (%d,%v), want (%d,true)",
				c.in, got, ok, c.want)
		}
	}
	if n, ok := ParseNumberRoot("xyz"); ok {
		t.Errorf("ParseNumberRoot(\"xyz\") = (%d,true), want failure", n)
	}
}
