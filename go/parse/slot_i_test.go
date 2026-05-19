package parse

import (
	"testing"

	"github.com/coudard/ithkuil/go/grammar"
)

func TestParseCc_TypeOnly(t *testing.T) {
	r := ParseCc("h")
	if r.Concat == nil || *r.Concat != grammar.Type1 {
		t.Errorf("ParseCc(\"h\") concat = %v, want Type1", r.Concat)
	}
	if r.Shortcut != nil {
		t.Errorf("ParseCc(\"h\") shortcut = %v, want nil", *r.Shortcut)
	}
	r = ParseCc("hw")
	if r.Concat == nil || *r.Concat != grammar.Type2 {
		t.Errorf("ParseCc(\"hw\") concat = %v, want Type2", r.Concat)
	}
	if r.Shortcut != nil {
		t.Errorf("ParseCc(\"hw\") shortcut = %v, want nil", *r.Shortcut)
	}
}

func TestParseCc_Combined(t *testing.T) {
	cases := []struct {
		in   string
		c    grammar.ConcatenationStatus
		s    grammar.CcShortcut
		want string
	}{
		{"hl", grammar.Type1, grammar.ShortcutW, "Type1+W"},
		{"hm", grammar.Type1, grammar.ShortcutY, "Type1+Y"},
		{"hr", grammar.Type2, grammar.ShortcutW, "Type2+W"},
		{"hn", grammar.Type2, grammar.ShortcutY, "Type2+Y"},
	}
	for _, c := range cases {
		r := ParseCc(c.in)
		if r.Concat == nil || *r.Concat != c.c {
			t.Errorf("ParseCc(%q) concat = %v, want %v (%s)", c.in, r.Concat, c.c, c.want)
		}
		if r.Shortcut == nil || *r.Shortcut != c.s {
			t.Errorf("ParseCc(%q) shortcut = %v, want %v (%s)", c.in, r.Shortcut, c.s, c.want)
		}
	}
}

func TestParseCc_ShortcutOnly(t *testing.T) {
	r := ParseCc("w")
	if r.Concat != nil {
		t.Errorf("ParseCc(\"w\") concat = %v, want nil", *r.Concat)
	}
	if r.Shortcut == nil || *r.Shortcut != grammar.ShortcutW {
		t.Errorf("ParseCc(\"w\") shortcut = %v, want ShortcutW", r.Shortcut)
	}
	r = ParseCc("y")
	if r.Shortcut == nil || *r.Shortcut != grammar.ShortcutY {
		t.Errorf("ParseCc(\"y\") shortcut = %v, want ShortcutY", r.Shortcut)
	}
}

func TestParseCc_Unrecognized(t *testing.T) {
	for _, s := range []string{"", "x", "h?", "no"} {
		r := ParseCc(s)
		if r.Concat != nil || r.Shortcut != nil {
			t.Errorf("ParseCc(%q) = %+v, want empty", s, r)
		}
	}
}
