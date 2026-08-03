#!/usr/bin/env bash
# Run the suite under TinyGo, which is what compiles the browser build.
#
# The standard suite does not cover this. TinyGo has its own runtime,
# GC and standard library, and the differences are not all cosmetic: it
# found a live panic in Stress.String that the standard toolchain hid,
# because fmt recovers a panic raised inside a String method and
# TinyGo's fmt does not. A CLI build printed %!v(PANIC=...) and carried
# on; the browser build died.
#
# Three packages are skipped, none of them for a reason that touches
# shipped code:
#
#   grammar  its architecture guard reads the package's own imports
#            through go/build, which needs runtime.GOTOOLDIR. A
#            source-inspection test, not a runtime one.
#   roman    test-only imports of store reach modernc.org/sqlite, and
#   gloss    the TinyGo compiler crashes on modernc.org/libc's tables.
#   store    Shipped code never links store into wasm, which is the
#            whole point of store depending on lexicon and not the
#            reverse; only their tests do.
set -uo pipefail

cd "$(dirname "$0")/../code"

skip="grammar roman gloss store"
fail=0

for dir in */; do
	pkg=${dir%/}
	[ -d "$pkg" ] || continue
	case " $skip " in *" $pkg "*) printf '%-12s skipped\n' "$pkg"; continue ;; esac
	ls "$pkg"/*_test.go >/dev/null 2>&1 || continue
	if out=$(tinygo test "./$pkg" 2>&1); then
		printf '%-12s ok\n' "$pkg"
	else
		printf '%-12s FAIL\n' "$pkg"
		echo "$out" | tail -20
		fail=1
	fi
done

exit $fail
