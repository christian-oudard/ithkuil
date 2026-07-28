package parse

import (
	"testing"

	"github.com/christian-oudard/ithkuil/grammar"
)

func TestParseCc_TypeOnly(t *testing.T) {
	r := ParseCc("h")
	if r.Concat == grammar.ConcatNone || r.Concat != grammar.Type1 {
		t.Errorf("ParseCc(\"h\") concat = %v, want Type1", r.Concat)
	}
	if r.Shortcut != ShortcutNone {
		t.Errorf("ParseCc(\"h\") shortcut = %v, want ShortcutNone", r.Shortcut)
	}
	r = ParseCc("hw")
	if r.Concat == grammar.ConcatNone || r.Concat != grammar.Type2 {
		t.Errorf("ParseCc(\"hw\") concat = %v, want Type2", r.Concat)
	}
	if r.Shortcut != ShortcutNone {
		t.Errorf("ParseCc(\"hw\") shortcut = %v, want ShortcutNone", r.Shortcut)
	}
}

func TestParseCc_Combined(t *testing.T) {
	cases := []struct {
		in   string
		c    grammar.ConcatenationStatus
		s    ShortcutVariant
		want string
	}{
		{"hl", grammar.Type1, ShortcutW, "Type1+W"},
		{"hm", grammar.Type1, ShortcutY, "Type1+Y"},
		{"hr", grammar.Type2, ShortcutW, "Type2+W"},
		{"hn", grammar.Type2, ShortcutY, "Type2+Y"},
	}
	for _, c := range cases {
		r := ParseCc(c.in)
		if r.Concat == grammar.ConcatNone || r.Concat != c.c {
			t.Errorf("ParseCc(%q) concat = %v, want %v (%s)", c.in, r.Concat, c.c, c.want)
		}
		if r.Shortcut != c.s {
			t.Errorf("ParseCc(%q) shortcut = %v, want %v (%s)", c.in, r.Shortcut, c.s, c.want)
		}
	}
}

func TestParseCc_ShortcutOnly(t *testing.T) {
	r := ParseCc("w")
	if r.Concat != grammar.ConcatNone {
		t.Errorf("ParseCc(\"w\") concat = %v, want nil", r.Concat)
	}
	if r.Shortcut != ShortcutW {
		t.Errorf("ParseCc(\"w\") shortcut = %v, want ShortcutW", r.Shortcut)
	}
	r = ParseCc("y")
	if r.Shortcut != ShortcutY {
		t.Errorf("ParseCc(\"y\") shortcut = %v, want ShortcutY", r.Shortcut)
	}
}

func TestParseCc_Unrecognized(t *testing.T) {
	for _, s := range []string{"", "x", "h?", "no"} {
		r := ParseCc(s)
		if r.Concat != grammar.ConcatNone || r.Shortcut != ShortcutNone {
			t.Errorf("ParseCc(%q) = %+v, want empty", s, r)
		}
	}
}
