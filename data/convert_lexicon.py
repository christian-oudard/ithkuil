#!/usr/bin/env python3
"""Convert Ithkuil lexicon TSV files to multiple formats."""

import csv
import json
import sys
from pathlib import Path

def load_roots(path: Path) -> list[dict]:
    """Load roots from TSV."""
    roots = []
    with open(path, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f, delimiter='\t')
        for row in reader:
            root = row.get('Root', '').strip()
            if not root:
                continue
            roots.append({
                'cr': root,
                'stem0': row.get('Stem 0 / Basic=mist', '').strip(),
                'stem1': row.get('Stem 1', '').strip(),
                'stem2': row.get('Stem 2', '').strip(),
                'stem3': row.get('Stem 3', '').strip(),
            })
    return roots

def load_affixes(path: Path) -> list[dict]:
    """Load affixes from TSV."""
    affixes = []
    with open(path, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f, delimiter='\t')
        for row in reader:
            cs = row.get('Affix', '').strip()
            if not cs:
                continue
            affixes.append({
                'cs': cs,
                'abbrev': row.get('Abv.', '').strip(),
                'description': row.get('Description', '').strip(),
                'type': row.get('Type', '').strip(),
                'degrees': [
                    row.get(f'Degree {i}', '').strip()
                    for i in range(1, 10)
                ]
            })
    return affixes

def to_json(roots: list[dict], affixes: list[dict], out_dir: Path):
    """Export as JSON."""
    with open(out_dir / 'roots.json', 'w', encoding='utf-8') as f:
        json.dump(roots, f, ensure_ascii=False, indent=2)
    with open(out_dir / 'affixes.json', 'w', encoding='utf-8') as f:
        json.dump(affixes, f, ensure_ascii=False, indent=2)

def to_haskell(roots: list[dict], affixes: list[dict], out_dir: Path):
    """Export as Haskell module."""
    hs = '''{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Lexicon Data
-- Auto-generated from collaborative spreadsheet
module Ithkuil.Lexicon where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Root entry
data RootEntry = RootEntry
  { rootCr :: Text
  , rootStem0 :: Text  -- Basic/generic meaning
  , rootStem1 :: Text
  , rootStem2 :: Text
  , rootStem3 :: Text
  } deriving (Show, Eq)

-- | Affix entry
data AffixEntry = AffixEntry
  { affixCs :: Text
  , affixAbbrev :: Text
  , affixDesc :: Text
  , affixType :: Text  -- "1", "2", "3", "D1", "D2", "A1", "A2", "0"
  , affixDegrees :: [Text]  -- 9 degrees
  } deriving (Show, Eq)

-- | All roots indexed by consonant form
roots :: Map Text RootEntry
roots = Map.fromList
'''
    for r in roots:
        cr = r['cr'].replace('"', '\\"')
        s0 = r['stem0'].replace('"', '\\"')
        s1 = r['stem1'].replace('"', '\\"')
        s2 = r['stem2'].replace('"', '\\"')
        s3 = r['stem3'].replace('"', '\\"')
        hs += f'  [ ("{cr}", RootEntry "{cr}" "{s0}" "{s1}" "{s2}" "{s3}")\n'
        break  # Just first for template
    hs += '  -- ... (full data in JSON)\n  ]\n\n'

    hs += '''-- | All affixes indexed by consonant form
affixes :: Map Text AffixEntry
affixes = Map.fromList
'''
    for a in affixes[:1]:
        cs = a['cs'].replace('"', '\\"')
        abbr = a['abbrev'].replace('"', '\\"')
        desc = a['description'].replace('"', '\\"')
        typ = a['type'].replace('"', '\\"')
        degs = ', '.join(f'"{d.replace(chr(34), chr(92)+chr(34))}"' for d in a['degrees'])
        hs += f'  [ ("{cs}", AffixEntry "{cs}" "{abbr}" "{desc}" "{typ}" [{degs}])\n'
    hs += '  -- ... (full data in JSON)\n  ]\n'

    hs += f'''
-- | Statistics
rootCount :: Int
rootCount = {len(roots)}

affixCount :: Int
affixCount = {len(affixes)}
'''

    with open(out_dir / 'Lexicon.hs', 'w', encoding='utf-8') as f:
        f.write(hs)

def to_ocaml(roots: list[dict], affixes: list[dict], out_dir: Path):
    """Export as OCaml module."""
    ml = f'''(** Ithkuil V4 Lexicon Data
    Auto-generated from collaborative spreadsheet *)

(** Root entry *)
type root_entry = {{
  cr : string;
  stem0 : string;
  stem1 : string;
  stem2 : string;
  stem3 : string;
}}

(** Affix entry *)
type affix_entry = {{
  cs : string;
  abbrev : string;
  description : string;
  affix_type : string;
  degrees : string list;
}}

(** Statistics *)
let root_count = {len(roots)}
let affix_count = {len(affixes)}

(** Sample roots (full data loaded from JSON at runtime) *)
let sample_roots : root_entry list = [
'''
    for r in roots[:5]:
        cr = r['cr'].replace('"', '\\"')
        s0 = r['stem0'].replace('"', '\\"')
        s1 = r['stem1'].replace('"', '\\"')
        s2 = r['stem2'].replace('"', '\\"')
        s3 = r['stem3'].replace('"', '\\"')
        ml += f'  {{ cr = "{cr}"; stem0 = "{s0}"; stem1 = "{s1}"; stem2 = "{s2}"; stem3 = "{s3}" }};\n'
    ml += ']\n\n'

    ml += '''(** Sample affixes *)
let sample_affixes : affix_entry list = [
'''
    for a in affixes[:5]:
        cs = a['cs'].replace('"', '\\"')
        abbr = a['abbrev'].replace('"', '\\"')
        desc = a['description'].replace('"', '\\"')
        typ = a['type'].replace('"', '\\"')
        degs = '; '.join(f'"{d.replace(chr(34), chr(92)+chr(34))}"' for d in a['degrees'])
        ml += f'  {{ cs = "{cs}"; abbrev = "{abbr}"; description = "{desc}"; affix_type = "{typ}"; degrees = [{degs}] }};\n'
    ml += ']\n'

    with open(out_dir / 'lexicon.ml', 'w', encoding='utf-8') as f:
        f.write(ml)

def to_lean(roots: list[dict], affixes: list[dict], out_dir: Path):
    """Export as Lean 4 module."""
    lean = f'''/-- Ithkuil V4 Lexicon Data
    Auto-generated from collaborative spreadsheet -/

namespace Ithkuil.Lexicon

/-- Root entry -/
structure RootEntry where
  cr : String
  stem0 : String
  stem1 : String
  stem2 : String
  stem3 : String
deriving Repr, Inhabited

/-- Affix entry -/
structure AffixEntry where
  cs : String
  abbrev : String
  description : String
  affixType : String
  degrees : List String
deriving Repr, Inhabited

/-- Statistics -/
def rootCount : Nat := {len(roots)}
def affixCount : Nat := {len(affixes)}

/-- Sample roots (full data loaded from JSON at runtime) -/
def sampleRoots : List RootEntry := [
'''
    for r in roots[:5]:
        cr = r['cr'].replace('"', '\\"')
        s0 = r['stem0'].replace('"', '\\"')
        s1 = r['stem1'].replace('"', '\\"')
        s2 = r['stem2'].replace('"', '\\"')
        s3 = r['stem3'].replace('"', '\\"')
        lean += f'  ⟨"{cr}", "{s0}", "{s1}", "{s2}", "{s3}"⟩,\n'
    lean += ']\n\n'

    lean += '''/-- Sample affixes -/
def sampleAffixes : List AffixEntry := [
'''
    for a in affixes[:5]:
        cs = a['cs'].replace('"', '\\"')
        abbr = a['abbrev'].replace('"', '\\"')
        desc = a['description'].replace('"', '\\"')
        typ = a['type'].replace('"', '\\"')
        degs = ', '.join(f'"{d.replace(chr(34), chr(92)+chr(34))}"' for d in a['degrees'])
        lean += f'  ⟨"{cs}", "{abbr}", "{desc}", "{typ}", [{degs}]⟩,\n'
    lean += ']\n\nend Ithkuil.Lexicon\n'

    with open(out_dir / 'Lexicon.lean', 'w', encoding='utf-8') as f:
        f.write(lean)

def main():
    data_dir = Path(__file__).parent

    print("Loading roots...")
    roots = load_roots(data_dir / 'roots.tsv')
    print(f"  Loaded {len(roots)} roots")

    print("Loading affixes...")
    affixes = load_affixes(data_dir / 'affixes.tsv')
    print(f"  Loaded {len(affixes)} affixes")

    print("Exporting JSON...")
    to_json(roots, affixes, data_dir)

    print("Exporting Haskell...")
    to_haskell(roots, affixes, data_dir)

    print("Exporting OCaml...")
    to_ocaml(roots, affixes, data_dir)

    print("Exporting Lean...")
    to_lean(roots, affixes, data_dir)

    print("Done!")

if __name__ == '__main__':
    main()
