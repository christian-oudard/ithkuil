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

DATA_DIR = Path(__file__).resolve().parent.parent / "data"

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


# Column positions in the Roots sheet, 0-based.
#
# The sheet is read positionally, not by header name. It carried a header
# row when this was written in May 2026 and does not now, so a
# DictReader silently took the first root as the field names and yielded
# nothing at all — 5947 rows in, zero out. Position is the more honest
# assumption: when it goes wrong, check_shape below says so.
#
# The sheet's own header names the columns, and it is not at the top:
# it sits at row 4451, pasted mid-list. The layout either side of it is
# identical, so it is a stray rather than a section boundary, but it is
# why a DictReader found nothing — row 1 is the root "b".
#
#   1 Root  2 Stem 0 / Basic  3 Stem 1  4 Stem 2  5 Stem 3
#   6-8 S1-S3 Completive   9-11 unlabelled, numeric
#   12-14 S1-S3 Pattern    15 Basic
#   16 Contential  17 Constitutive  18-20 S1-S3 Objective  21 Dynamic
#   22-24 unlabelled, holding the Wikidata ids
#
# Pattern and Basic we do not store. Completive precedes Objective here,
# where our own roots.tsv writes it after; that file's header is ours,
# not the sheet's, so the two orders are unrelated.
ROOT_COLS = {
    "cr": 0,
    "stem0": 1, "stem1": 2, "stem2": 3, "stem3": 4,
    "completive": (5, 6, 7),
    "contential": 15, "constitutive": 16,
    "objective": (17, 18, 19),
    "dynamic": 20,
    "wikidata": (21, 22, 23),
}

# Fewer roots than this and something is wrong with the fetch or the
# sheet, not with the language. The lexicon has never shrunk.
MIN_ROOTS = 5000


def check_shape(rows: list[list[str]]) -> None:
    """Fail loudly when the sheet stops looking like the sheet."""
    if len(rows) < MIN_ROOTS:
        raise ValueError(
            f"only {len(rows)} root rows; expected at least {MIN_ROOTS}. "
            f"The sheet's shape has probably changed — check ROOT_COLS."
        )
    width = max(ROOT_COLS["wikidata"]) + 1
    narrow = sum(1 for r in rows if len(r) < width)
    if narrow > len(rows) // 100:
        raise ValueError(
            f"{narrow} of {len(rows)} rows are narrower than {width} columns; "
            f"the sheet's shape has probably changed — check ROOT_COLS."
        )
    # Stem 0 is filled for every root in every version we have seen. If
    # it is mostly empty, the columns have moved under us.
    filled = sum(1 for r in rows if len(r) > 1 and r[1].strip())
    if filled < len(rows) * 0.9:
        raise ValueError(
            f"Stem 0 is empty in {len(rows) - filled} of {len(rows)} rows; "
            f"the columns have probably moved — check ROOT_COLS."
        )


def parse_roots(csv_text: str) -> list[dict]:
    rows = [r for r in csv.reader(csv_text.splitlines()) if r and r[0].strip()]
    # Drop header rows wherever they appear, not just first: the sheet
    # keeps one at row 4451. "Root" is not a possible C_R — §3 admits no
    # root beginning with r- followed by another consonant here — so
    # matching on it cannot swallow a real entry.
    rows = [r for r in rows if r[0].strip().lower() != "root"]
    check_shape(rows)

    def cell(row: list[str], i: int) -> str:
        return normalize(row[i]) if i < len(row) else ""

    out = []
    for row in rows:
        cr = cell(row, ROOT_COLS["cr"])
        if not cr:
            continue
        entry = {"cr": cr}
        for f in ("stem0", "stem1", "stem2", "stem3"):
            entry[f] = cell(row, ROOT_COLS[f])
        for f in ("contential", "constitutive", "dynamic"):
            v = cell(row, ROOT_COLS[f])
            if v:
                entry[f] = v
        for f in ("objective", "completive", "wikidata"):
            vals = [cell(row, i) for i in ROOT_COLS[f]]
            if any(vals):
                entry[f] = vals
        out.append(entry)
    return out


# §3.5.0.1's seven gradient types, plus the empty cell. The trailing
# asterisk is that section's separate mark for an affix that also has a
# C_R root, so it is stripped before the check and kept in the value.
LEGAL_TYPES = {"", "0", "A1", "A2", "B", "C", "D1", "D2"}


def clean_type(cell: str) -> str:
    """Take the type off a Type cell, and refuse one that is not a type.

    The cell is free text and does not always hold only a type. ANG's
    reads "0* 1 arc-seconds 2 arc-minutes 3 mils ..." — the type, then
    a whole nine-item degree list somebody drafted in the wrong column.
    Taking the first token keeps the type and drops the spill.

    Raising rather than passing the cell through is deliberate. The
    value reaches consumers through the API, and a type nothing
    recognizes is worse there than a sync that stops and says so.
    """
    head = cell.split(" ", 1)[0]
    if head.rstrip("*") not in LEGAL_TYPES:
        raise ValueError(f"unrecognized gradient type {cell!r}")
    return head


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
            "type": clean_type(normalize(row.get("Type", ""))),
            "degrees": [normalize(row.get(f"Degree {i}", "")) for i in range(1, 10)],
        })
    return out


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
                [a["cs"], a.get("abbrev", "")]
                + a.get("degrees", [])
                + [a.get("type", ""), a.get("description", "")]
            )


def write_json(data: list[dict], path: Path) -> None:
    with open(path, "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=False, indent=2)
        f.write("\n")


def merge_affixes_from_data(parsed: list[dict], data_path: Path) -> list[dict]:
    """Preserve local description/type from data.json when upstream is blank."""
    if not data_path.exists():
        return parsed
    with open(data_path, encoding="utf-8") as f:
        data = json.load(f)
    return merge_affix_entries(parsed, data.get("affixes", []))


def merge_affix_entries(parsed: list[dict], existing: list[dict]) -> list[dict]:
    by_cs = {a["cs"]: a for a in existing}
    out = []
    for a in parsed:
        if cur := by_cs.get(a["cs"]):
            if not a.get("description") and cur.get("description"):
                a["description"] = cur["description"]
            if not a.get("type") and cur.get("type"):
                a["type"] = cur["type"]
        out.append(a)
    # Affixes documented in the grammar but absent from the spreadsheet
    # (XCL -çx, at the time of writing) would otherwise be dropped on
    # every sync. Keep them, and name them so the run says what it kept.
    upstream_cs = {a["cs"] for a in parsed}
    local_only = [a for a in existing if a["cs"] not in upstream_cs]
    if local_only:
        names = ", ".join(f"{a['cs']} ({a.get('abbrev', '?')})" for a in local_only)
        print(f"  keeping {len(local_only)} affix(es) not in upstream: {names}")
        out.extend(local_only)
    return out


def write_data(roots: list[dict], affixes: list[dict], path: Path) -> int:
    """Write roots and affixes into data.json, preserving every other section.

    Only roots and affixes come from the sheet. Everything else in the
    file is ours and is carried through untouched, whatever it is called
    — this used to name "grammar" explicitly and so silently deleted the
    42-entry "topics" section the first time a sync ran after it was
    added.

    Versioning: monotonically increasing uint16. Version bumps only when
    roots or affixes content changes; the other sections don't affect it.
    """
    new_content = {"roots": roots, "affixes": affixes}
    new_payload = json.dumps(new_content, sort_keys=True, ensure_ascii=False)

    prev_version = 0
    prev_payload = None
    kept: dict = {}
    if path.exists():
        with open(path, encoding="utf-8") as f:
            prev = json.load(f)
        prev_version = int(prev.get("version", 0))
        kept = {k: v for k, v in prev.items()
                if k not in ("version", "roots", "affixes")}
        prev_payload = json.dumps(
            {"roots": prev.get("roots", []), "affixes": prev.get("affixes", [])},
            sort_keys=True, ensure_ascii=False,
        )

    version = prev_version if prev_payload == new_payload else prev_version + 1
    if version > 0xFFFF:
        raise ValueError(f"version overflow: {version}")

    combined = {"version": version, **kept, "roots": roots, "affixes": affixes}
    with open(path, "w", encoding="utf-8") as f:
        json.dump(combined, f, ensure_ascii=False, indent=2)
        f.write("\n")
    return version


def drop_retired(roots: list[dict]) -> list[dict]:
    """Discard entries the sheet has retired.

    A trailing dagger on the Stem 0 gloss is upstream's mark for a word
    that is no longer part of the language: five grammar-metalanguage
    roots (mps "relative clause head", mpx "case scope" and their like)
    and fifty-one household-appliance roots that a later scheme
    replaced. They are not alternative meanings, they are withdrawn
    ones, so nothing downstream should be able to reach them.

    Keeping them was not free. They were the only roots in the lexicon
    that broke §2.13, so the phonotactic sweep needed a named exclusion
    list to stay green, and ksmy "oven" was one of two entries competing
    for a single C_R key.

    roots.tsv still carries them: it mirrors the sheet, and the point of
    the mirror is to diff against upstream.
    """
    live = [r for r in roots if not r.get("stem0", "").rstrip().endswith("†")]
    print(f"  dropping {len(roots) - len(live)} retired entries")
    return live


def apply_root_overrides(roots: list[dict]) -> list[dict]:
    """Move roots off a C_R that two unrelated meanings both claim.

    A C_R names one root, so the four collisions in the sheet are a
    defect in it rather than a fact about the language. Dropping one
    side, which is what the store used to do, loses a real meaning;
    these move it instead.

    data/lexicon_overrides.json holds the reassignments and
    docs/reference/ERRATA.md the reasoning for each. An override is
    matched on both the old C_R and the Stem 0 gloss, so an upstream
    repair that renames or removes that row stops matching and the
    override falls away rather than corrupting a row that has since
    become something else.
    """
    path = DATA_DIR / "lexicon_overrides.json"
    if not path.exists():
        return roots
    with open(path, encoding="utf-8") as f:
        spec = json.load(f)
    for o in spec.get("overrides", []):
        hits = [
            r for r in roots
            if r.get("cr") == o["cr"] and r.get("stem0", "").strip() == o["match_stem0"]
        ]
        if len(hits) != 1:
            print(f"  override {o['cr']} -> {o['new_cr']}: {len(hits)} rows match, skipped")
            continue
        if any(r.get("cr") == o["new_cr"] for r in roots):
            print(f"  override {o['cr']} -> {o['new_cr']}: destination taken, skipped")
            continue
        hits[0]["cr"] = o["new_cr"]
        print(f"  {o['cr']} -> {o['new_cr']} ({o['basis']}): {o['match_stem0']}")
    return roots


def apply_affix_overrides(affixes: list[dict]) -> list[dict]:
    """Repair affix rows the sheet gets wrong, in two ways.

    "affixes" moves an affix off a C_S that two unrelated ones both
    claim, the same repair as apply_root_overrides on the other half of
    the sheet. A hand-edit in data.json will not survive a sync, which
    is how MDI's move off ḑg was lost: it had been corrected in place,
    the fetch overwrote it, and the store then refused the duplicate
    key.

    "degrees" restores the nine meanings of a row that is on the right
    C_S but whose meanings are blank or belong to a different affix.
    Nine affixes need it: six Functional Groups the sheet never filled
    in, and MET/GPJ/ENS, three adjacent rows whose content was pasted
    one row low so that each carries its predecessor's list and the
    first is empty. Every restored string is Quijada's, from the affix
    document the sheet transcribes, so this adds nothing the sources do
    not already say.
    """
    path = DATA_DIR / "lexicon_overrides.json"
    if not path.exists():
        return affixes
    with open(path, encoding="utf-8") as f:
        spec = json.load(f)
    for o in spec.get("affixes", []):
        hits = [
            a for a in affixes
            if a.get("cs") == o["cs"] and a.get("abbrev") == o["match_abbrev"]
        ]
        if len(hits) != 1:
            print(f"  affix override {o['cs']} -> {o['new_cs']}: {len(hits)} rows match, skipped")
            continue
        if any(a.get("cs") == o["new_cs"] for a in affixes):
            print(f"  affix override {o['cs']} -> {o['new_cs']}: destination taken, skipped")
            continue
        hits[0]["cs"] = o["new_cs"]
        print(f"  {o['cs']} -> {o['new_cs']} ({o['basis']}): {o['match_abbrev']}")

    for o in spec.get("degrees", []):
        hits = [
            a for a in affixes
            if a.get("cs") == o["cs"] and a.get("abbrev") == o["match_abbrev"]
        ]
        if len(hits) != 1:
            print(f"  degrees {o['match_abbrev']}: {len(hits)} rows match, skipped")
            continue
        # match_degree1 pins the broken state, so an upstream repair
        # stops the override rather than overwriting the repair. It is
        # the empty string for the rows the sheet never filled in.
        got = hits[0]["degrees"][0].strip()
        if got != o["match_degree1"].strip():
            print(f"  degrees {o['match_abbrev']}: upstream now reads "
                  f"{got[:40]!r}, not the state this repairs; skipped")
            continue
        hits[0]["degrees"] = list(o["degrees"])
        print(f"  degrees {o['match_abbrev']} (-{o['cs']}): restored 9 from the affix document")

    for o in spec.get("types", []):
        hits = [
            a for a in affixes
            if a.get("cs") == o["cs"] and a.get("abbrev") == o["match_abbrev"]
        ]
        if len(hits) != 1:
            print(f"  type {o['match_abbrev']}: {len(hits)} rows match, skipped")
            continue
        if hits[0].get("type") != o["match_type"]:
            print(f"  type {o['match_abbrev']}: upstream now reads "
                  f"{str(hits[0].get('type'))[:40]!r}; skipped")
            continue
        hits[0]["type"] = o["new_type"]
        print(f"  type {o['match_abbrev']} (-{o['cs']}): -> {o['new_type']}")
    return affixes


def main() -> int:
    data_path = DATA_DIR / "data.json"

    print("Fetching roots from upstream sheet...")
    roots = parse_roots(fetch(ROOTS_URL))
    print(f"  {len(roots)} root entries")

    print("Fetching affixes from upstream sheet...")
    affixes = parse_affixes(fetch(AFFIXES_URL))
    # Overrides first: they repair upstream's own duplicate, and only
    # then can the merge tell a kept local affix from a stale one.
    affixes = merge_affixes_from_data(apply_affix_overrides(affixes), data_path)
    print(f"  {len(affixes)} affix entries")

    write_roots_tsv(roots, DATA_DIR / "roots.tsv")
    write_affixes_tsv(affixes, DATA_DIR / "affixes.tsv")
    version = write_data(apply_root_overrides(drop_retired(roots)), affixes, data_path)
    print(f"Wrote roots.tsv, affixes.tsv, data.json (version v{version})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
