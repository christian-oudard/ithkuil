#!/usr/bin/env bash
# Build the browser module: cmd/ithkuil-wasm, its loader, and the
# lexicon it reads. Writes to $XDG_DATA_HOME/ithkuil/web, or to $1.
#
# TinyGo rather than the standard toolchain, and the difference is not
# marginal. Measured on this module, brotli'd, which is the only number
# that matters over the wire:
#
#   go build -ldflags="-s -w" -trimpath      5.08 MB   1.01 MB br
#   go build + wasm-opt -Oz                  4.32 MB   1.02 MB br
#   tinygo build                             2.60 MB   0.66 MB br
#   tinygo build -no-debug                   1.19 MB   0.32 MB br
#   tinygo build -no-debug + wasm-opt -Oz    1.14 MB   0.31 MB br
#
# So wasm-opt earns its place on the TinyGo output and loses money on
# the standard one, where it grew the compressed size. TinyGo's default
# carries DWARF, which is most of the gap between rows three and four.
#
# Not done, both measured and both refused. Stubbing out the Unicode
# NFC normalizer (golang.org/x/text/unicode/norm, the largest single
# dependency) saves 20 KB brotli'd, and costs the guarantee that "š"
# spelled with a combining caron reads the same as the composed one.
# Replacing encoding/json with hand-written marshaling saves 4 KB.
# Neither is worth its price; -panic=trap saves 42 KB and is a closer
# call, but a trap with no message is the opposite of failing loudly.
set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)
out=${1:-${XDG_DATA_HOME:-$HOME/.local/share}/ithkuil/web}
mkdir -p "$out"

cd "$repo/code"
tinygo build -target=wasm -no-debug -o "$out/ithkuil.wasm" ./cmd/ithkuil-wasm
wasm-opt -Oz --enable-bulk-memory --enable-nontrapping-float-to-int \
	"$out/ithkuil.wasm" -o "$out/ithkuil.wasm"

# TinyGo's loader, which is not interchangeable with the standard
# toolchain's: they disagree on the import object.
# install rather than cp: the source is read-only in the Nix store, so
# a plain copy fails the second time the script runs.
install -m 644 \
	"$(dirname "$(dirname "$(readlink -f "$(command -v tinygo)")")")"/share/tinygo/targets/wasm_exec.js \
	"$out/wasm_exec.js"

# The lexicon ships in two pieces because the module's load() merges
# rather than replaces. Affixes are a fifth of the size of roots and
# cover the affix ladder, so a page can show meanings well before the
# roots land. Nothing needs either to parse.
python3 - "$repo/data/data.json" "$out" <<'PY'
import json, sys
doc = json.load(open(sys.argv[1]))
out = sys.argv[2]
# The notes ride with the affixes: 6 KB brotli'd, and nothing reads
# well without them, since a glossary row's link has nowhere to go.
json.dump({"version": doc["version"], "affixes": doc["affixes"],
           "grammar": [{"abbrev": g["abbrev"],
                        "explanation": g.get("explanation", ""),
                        "guidance": g.get("guidance", "")}
                       for g in doc["grammar"]
                       if g.get("explanation") or g.get("guidance")],
           "topics": doc.get("topics", [])},
          open(out + "/affixes.json", "w"), ensure_ascii=False)
json.dump({"version": doc["version"], "roots": doc["roots"]},
          open(out + "/roots.json", "w"), ensure_ascii=False)
PY

files="$out/ithkuil.wasm $out/affixes.json $out/roots.json"

for f in $files; do
	brotli -f -q 11 "$f" -o "$f.br"
done

printf '\n%-14s %10s %10s\n' FILE RAW BROTLI
for f in $files; do
	awk -v n="$(basename "$f")" -v r="$(stat -c%s "$f")" -v b="$(stat -c%s "$f.br")" \
		'BEGIN { printf "%-14s %9.0fK %9.0fK\n", n, r/1024, b/1024 }'
done
