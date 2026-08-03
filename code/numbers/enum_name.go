package numbers

import "strconv"

// enumName indexes names by v, and names an out-of-range value rather
// than panicking on the index.
//
// The bounds check is not defensive habit. fmt recovers a panic raised
// inside a String method and prints %!v(PANIC=...) in its place, and
// TinyGo's fmt does not recover, so the same out-of-range value garbles
// a CLI error message and kills the browser build. Neither is a way to
// report a bad value, and "%s(7)" says exactly what happened.
//
// Only naming methods use this. The functions that turn a value into
// its written form (parse.VsForm, slots.valenceVowel and the rest)
// index the same way and are left to panic: there is no honest fallback
// romanization, and returning one would put invented letters into a
// word instead of stopping.
func enumName[T ~int](v T, typ string, names ...string) string {
	if v < 0 || int(v) >= len(names) {
		return typ + "(" + strconv.Itoa(int(v)) + ")"
	}
	return names[v]
}
