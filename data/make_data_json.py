#!/usr/bin/env python3
"""One-time script to bootstrap data/data.json from gen_grammar.go output
and the existing lexicon.json.

Usage (from repo root):
    go run data/gen_grammar.go > /tmp/grammar.json
    python3 data/make_data_json.py
"""
import json
from pathlib import Path

data_dir = Path(__file__).parent

with open(data_dir / "lexicon.json", encoding="utf-8") as f:
    lex = json.load(f)

with open("/tmp/grammar.json", encoding="utf-8") as f:
    grammar = json.load(f)

data = {
    "version": lex["version"],
    "grammar": grammar,
    "roots": lex["roots"],
    "affixes": lex["affixes"],
}

out = data_dir / "data.json"
with open(out, "w", encoding="utf-8") as f:
    json.dump(data, f, ensure_ascii=False, indent=2)
    f.write("\n")

print(f"Wrote {out}")
print(f"  grammar entries: {len(grammar)}")
print(f"  roots:           {len(lex['roots'])}")
print(f"  affixes:         {len(lex['affixes'])}")
