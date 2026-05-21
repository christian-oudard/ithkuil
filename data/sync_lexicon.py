#!/usr/bin/env python3
"""Sync the Ithkuil lexicon from the upstream Google Sheet.

Source: https://docs.google.com/spreadsheets/d/1JdaG1PaSQJRE2LpILvdzthbzz1k_a0VT86XSXouwGy8
(Collaborative Ithkuil IV Roots and Affixes Spreadsheet)

Fetches the Roots and Affixes sheets as CSV, normalises mathematical-
alphabet styled letters (𝕘𝕣𝕒𝕞𝕞𝕒𝕥𝕚𝕔𝕒𝕝 → grammatical) back to plain Latin,
and writes refreshed TSV (for diff visibility) and JSON (consumed by the
Go code).

Locally-supplemented affix fields (description, type) are preserved
where upstream is blank — many older affixes were curated from PDF
references and the spreadsheet's `Type` column is derivable from the Cs
cluster but not always filled in.
"""

import csv
import json
import sys
import unicodedata
import urllib.request
from pathlib import Path

SHEET_ID = "1JdaG1PaSQJRE2LpILvdzthbzz1k_a0VT86XSXouwGy8"
ROOTS_URL = (
    f"https://docs.google.com/spreadsheets/d/{SHEET_ID}/gviz/tq"
    "?tqx=out:csv&sheet=Roots(Positional%20order)&headers=1"
)
AFFIXES_URL = (
    f"https://docs.google.com/spreadsheets/d/{SHEET_ID}/gviz/tq"
    "?tqx=out:csv&sheet=Affixes&headers=1"
)

# Unicode "Mathematical Alphanumeric Symbols" block: U+1D400..U+1D7FF.
# Subscript/superscript digits (U+2070..U+209F) deliberately not folded —
# they carry semantic meaning in affix degree descriptions like
# "rightward₁/leftward₂".
MATH_ALPHA_START = 0x1D400
MATH_ALPHA_END = 0x1D7FF


def normalize(s: str) -> str:
    out = []
    for ch in s:
        cp = ord(ch)
        if MATH_ALPHA_START <= cp <= MATH_ALPHA_END:
            out.append(unicodedata.normalize("NFKC", ch))
        else:
            out.append(ch)
    return "".join(out).strip()


def fetch(url: str) -> str:
    with urllib.request.urlopen(url) as resp:
        return resp.read().decode("utf-8")


def parse_roots(csv_text: str) -> list[dict]:
    reader = csv.DictReader(csv_text.splitlines())
    out = []
    for row in reader:
        cr = normalize(row.get("Root", ""))
        if not cr:
            continue
        entry = {
            "cr": cr,
            "stem0": normalize(row.get("Stem 0 / Basic", "")),
            "stem1": normalize(row.get("Stem 1", "")),
            "stem2": normalize(row.get("Stem 2", "")),
            "stem3": normalize(row.get("Stem 3", "")),
        }
        contential = normalize(row.get("Contential", ""))
        if contential:
            entry["contential"] = contential
        constitutive = normalize(row.get("Constitutive", ""))
        if constitutive:
            entry["constitutive"] = constitutive
        dynamic = normalize(row.get("Dynamic", ""))
        if dynamic:
            entry["dynamic"] = dynamic

        def trio(col_prefix: str) -> list[str] | None:
            vals = [normalize(row.get(f"S{i} {col_prefix}", "")) for i in (1, 2, 3)]
            return vals if any(vals) else None

        objective = trio("Objective")
        if objective:
            entry["objective"] = objective
        completive = trio("Completive")
        if completive:
            entry["completive"] = completive
        wikidata = trio("Wikidata")
        if wikidata:
            entry["wikidata"] = wikidata
        out.append(entry)
    return out


def parse_affixes(csv_text: str) -> list[dict]:
    reader = csv.DictReader(csv_text.splitlines())
    out = []
    for row in reader:
        cs = normalize(row.get("Affix", ""))
        if not cs:
            continue
        out.append({
            "cs": cs,
            "abbrev": normalize(row.get("Abv.", "")),
            "description": normalize(row.get("Description", "")),
            "type": normalize(row.get("Type", "")),
            "degrees": [normalize(row.get(f"Degree {i}", "")) for i in range(1, 10)],
        })
    return out


def merge_affixes(upstream: list[dict], existing_path: Path) -> list[dict]:
    """Preserve local description/type when upstream is blank.

    Match on (cs, abbrev) since the same Cs cluster can carry two
    affixes (e.g. ḑg = MDI and ḑg = S07 are both in upstream).
    """
    if not existing_path.exists():
        return upstream
    with open(existing_path, encoding="utf-8") as f:
        local = json.load(f)
    local_by_key = {(a["cs"], a["abbrev"]): a for a in local}
    for a in upstream:
        prior = local_by_key.get((a["cs"], a["abbrev"]))
        if not prior:
            continue
        if not a["description"] and prior.get("description"):
            a["description"] = prior["description"]
        if not a["type"] and prior.get("type"):
            a["type"] = prior["type"]
    return upstream


def write_roots_tsv(roots: list[dict], path: Path) -> None:
    cols = [
        "Root",
        "Stem 0 / Basic", "Stem 1", "Stem 2", "Stem 3",
        "Contential", "Constitutive",
        "S1 Objective", "S2 Objective", "S3 Objective",
        "S1 Completive", "S2 Completive", "S3 Completive",
        "Dynamic",
        "S1 Wikidata", "S2 Wikidata", "S3 Wikidata",
    ]
    with open(path, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, delimiter="\t", lineterminator="\n")
        w.writerow(cols)
        for r in roots:
            obj = r.get("objective", ["", "", ""])
            cpt = r.get("completive", ["", "", ""])
            wik = r.get("wikidata", ["", "", ""])
            w.writerow([
                r["cr"],
                r["stem0"], r["stem1"], r["stem2"], r["stem3"],
                r.get("contential", ""), r.get("constitutive", ""),
                obj[0], obj[1], obj[2],
                cpt[0], cpt[1], cpt[2],
                r.get("dynamic", ""),
                wik[0], wik[1], wik[2],
            ])


def write_affixes_tsv(affixes: list[dict], path: Path) -> None:
    with open(path, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, delimiter="\t", lineterminator="\n")
        w.writerow(
            ["Affix", "Abv."]
            + [f"Degree {i}" for i in range(1, 10)]
            + ["Type", "Description"]
        )
        for a in affixes:
            w.writerow(
                [a["cs"], a["abbrev"]] + a["degrees"] + [a["type"], a["description"]]
            )


def write_json(data: list[dict], path: Path) -> None:
    with open(path, "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=False, indent=2)
        f.write("\n")


def main() -> int:
    data_dir = Path(__file__).parent

    print("Fetching roots from upstream sheet...")
    roots = parse_roots(fetch(ROOTS_URL))
    print(f"  {len(roots)} root entries")

    print("Fetching affixes from upstream sheet...")
    affixes = parse_affixes(fetch(AFFIXES_URL))
    affixes = merge_affixes(affixes, data_dir / "affixes.json")
    print(f"  {len(affixes)} affix entries")

    write_roots_tsv(roots, data_dir / "roots.tsv")
    write_affixes_tsv(affixes, data_dir / "affixes.tsv")
    write_json(roots, data_dir / "roots.json")
    write_json(affixes, data_dir / "affixes.json")
    print("Wrote roots.tsv, affixes.tsv, roots.json, affixes.json")
    return 0


if __name__ == "__main__":
    sys.exit(main())
