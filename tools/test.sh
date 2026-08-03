#!/usr/bin/env bash
# Run the Go test suite with cross-package coverage and print a summary.
#
# Usage:
#   script/test.sh                 # all packages
#   script/test.sh ./grammar/...   # subset (only those tests run, but
#                                  # coverage still totals over ./...)
#
# Flags (env vars):
#   COVERAGE_THRESHOLD=NN  fail if total coverage drops below NN%
#   SHOW_UNCOVERED=1       list functions still below 100% at the end
#   SKIP_DEADCODE=1        skip the unreachable-function pass
#
# -coverpkg=./... is the important bit: every test contributes coverage
# against every package, so cross-package tests are counted.
#
# The deadcode pass answers a question coverage cannot. A function at 0%
# might be untested or might be unreachable, and the two want opposite
# fixes: write a test, or delete the code. Go will not tell them apart
# either, objecting to an unused local and never to an unused
# package-level function, so a refactor that takes away the last caller
# leaves the callee sitting there. Eight had accumulated that way before
# this pass existed.

set -euo pipefail

if ! command -v go >/dev/null 2>&1; then
  exec nix develop "$(dirname "$0")/.." --command "$0" "$@"
fi

cd "$(dirname "$0")/../code"

PKGS=("${@:-./...}")
PROFILE=$(mktemp -t ithkuil-cov.XXXXXX.out)
trap 'rm -f "$PROFILE"' EXIT

# Suppress the misleading "X% of statements in ./..." line go test prints
# per package (it's "fraction of module covered by this binary", not
# per-package coverage). Real numbers come from the merged profile below.
go test -coverpkg=./... -coverprofile="$PROFILE" "${PKGS[@]}" \
  | grep -v 'of statements in' \
  || true

echo
echo "=== Per-package coverage (merged) ==="
# The profile contains one entry per (range, test-binary), so most ranges
# repeat ~N times where N is the number of tested packages. Dedupe by
# range first (taking the max count across binaries), then aggregate.
awk -v mod="$(go list -m)" '
  NR == 1 { next }                       # skip "mode:" line
  {
    range = $1
    stmts = $2 + 0
    count = $3 + 0
    if (!(range in seen) || count > seen[range]) seen[range] = count
    rstmts[range] = stmts
  }
  END {
    for (range in seen) {
      # range looks like "module/pkg/path/file.go:line.col,line.col"
      file = range
      sub(/:.*/, "", file)
      sub("^" mod "/", "", file)
      pkg = file
      sub(/\/[^\/]*$/, "", pkg)          # drop the file name
      total[pkg] += rstmts[range]
      if (seen[range] > 0) covered[pkg] += rstmts[range]
    }
    for (p in total) {
      pct = (total[p] > 0) ? 100 * covered[p] / total[p] : 0
      printf "  %-22s %6.1f%%  (%d/%d)\n", p, pct, covered[p], total[p]
    }
  }
' "$PROFILE" | sort

TOTAL=$(go tool cover -func="$PROFILE" | awk '/^total:/ {print $NF}')
echo
echo "Total: $TOTAL"

if [ "${SHOW_UNCOVERED:-}" = "1" ]; then
  echo
  echo "=== Functions below 100% ==="
  uncov=$(go tool cover -func="$PROFILE" | grep -v 100.0% | grep -v '^total:' || true)
  if [ -z "$uncov" ]; then
    echo "  (none)"
  else
    echo "$uncov"
  fi
fi

if [ "${SKIP_DEADCODE:-}" != "1" ]; then
  echo
  echo "=== Unreachable functions ==="
  # -test counts a function called only from a test as reachable, which
  # is what we want: a helper used by one test is live code. Reachability
  # is computed from the three main packages and the test binaries, so
  # this is "nothing in the tree calls it", not "no importer might".
  dead=$(go tool deadcode -test ./... || true)
  if [ -n "$dead" ]; then
    echo "$dead" >&2
    echo >&2
    echo "FAIL: unreachable functions; delete them or call them" >&2
    exit 1
  fi
  echo "  (none)"
fi

if [ -n "${COVERAGE_THRESHOLD:-}" ]; then
  pct=${TOTAL%\%}
  if awk "BEGIN { exit !($pct < $COVERAGE_THRESHOLD) }"; then
    echo
    echo "FAIL: coverage $TOTAL is below threshold ${COVERAGE_THRESHOLD}%" >&2
    exit 1
  fi
fi
