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


# Three consecutive rows of the upstream Affixes sheet have their degree
# cells shifted down by one: MET's nine meanings sit on GPJ's row, GPJ's
# on ENS's, MET's own row is blank, and ENS's fall off the end and are
# lost. The shape is what "insert cells, shift down" over a three-row
# selection produces.
#
# Which reading is right is not a judgement call. MET is "Metonymic
# Categories" and "part for whole" is metonymy; GPJ is "Functional Group
# J" and thiocyanate is a functional group; ENS is "Environmental Niche"
# and crepuscular/nocturnal is one. Upstream pairs each description with
# the previous row's meanings.
#
# ENS's nine degrees survive only in docs/reference/affixes_reference.md,
# which predates the spreadsheet sync, so all three are restored from
# there verbatim. merge_affixes reports when it overrides, so a sync that
# stops printing these three has had the shift repaired upstream and the
# table can go.
SHIFTED_DEGREES = {
    "MET": [
        "part for whole",
        "producer for product",
        "object used or owned for user/owner",
        "controller for controlled",
        "institution for people responsible",
        "place for inhabitants/occupants",
        "place for event",
        "place for institution",
        "attribute or characteristic for owner",
    ],
    "GPJ": [
        "thiocyanate, thocyanato-, -thiocyanate",
        "isothiocyanate,  isothiocyanato-, -isothiocyanate",
        "methanethioyl-, -thial",
        "carbothioic S-acid, mercaptocarbonyl-, -thioic S-acid",
        "carbothioic O-acid, hydroxythiocarbonyl-, -thioic O-acid",
        "thioester, S-alkyl-alkane-thioate",
        "thionoester, O-alkyl-alkane-thioate",
        "carbodithioic acid, dithiocarboxy-, -dithioic acid",
        "dithiocarboxylic acid ester, -dithioate",
    ],
    "ENS": [
        "active at twilight/crepuscular",
        "active at night/nocturnal",
        "active around dawn",
        "active during the morning",
        "active during the day/diurnal",
        "sessile, not motile -- adhering to a substrate by direct attachment (not via a stalk/stipe/pedicel/connecting medium)",
        "attached to a substrate via a stalk/stipe/pedicel/connecting medium",
        "motile in reaction to heat",
        "motile in reaction to light",
    ],
}


# GPB through GPG carry a C_S, an abbreviation and a description upstream
# and nine empty degree cells apiece. They are not empty in Quijada: his
# affix document populates all six, and these are extracted from it by
# column geometry (see docs/reference/ISSUES.md, A5).
#
# The extraction was checked against the three groups the sheet does
# carry. GPJ came back identical in all nine degrees; GPA and GPH differ
# only in that the sheet's versions are shortened — "alkyl halide" for
# the PDF's "halo-, alkyl halide" — so the reading is faithful and
# fuller than what upstream has.
#
# The "..." in several entries is Quijada's own placeholder for the
# elided stem, kept as printed rather than reworded.
RECOVERED_DEGREES = {
    "GPB": [
        "hydroxil, hydroxy-, -ol",
        "carbonyl, oxo-, -oyl-, -one",
        "aldehyde, -formyl-, -al",
        "haloformyl, carbono...oyl-, -oyl halide",
        "carbonate ester, alkoxycarbonyloxy-, alkyl carbonate",
        "carboxylate, carboxylato-, -oate",
        "carboxyl, carboxy-, -oic acid",
        "carboalcoxy, alkanoyloxy-, alkyl alkanoate",
        "methoxy, methoxy-",
    ],
    "GPC": [
        "hydroperoxy-, alkyl hydroperoxide",
        "peroxy-, alkyl peroxide",
        "ether, alkoxy-, alkyl ether",
        "hemiacetal, alkoxy -ol, -al alkyl hemiacetal",
        "hemiketal, alxoxy -ol, -one alkyl hemiketal",
        "acetal, dialkoxy-, -al dialkyl acetal",
        "ketal, dialcoxy-, -one dialkyl ketal",
        "orthoester, -trialkoxy",
        "orthocarbonate ester, tetralkoxy-, tetraalkyl orthocarbonate",
    ],
    "GPD": [
        "methylenedioxy-, -dioxole",
        "carboxylic anhydride, anhydride",
        "carboxamide, carboxamido-, carbamoyl-, -amide",
        "primary amine, amino-, -amine",
        "secondary amine, amino-, -amine",
        "tertiary amine, amino-, -amine",
        "ammonio-, -ammonium",
        "imide, imido-, -imide",
        "azide, azido-, alkyl azide",
    ],
    "GPE": [
        "primary ketimine, imino-, imine",
        "secondary ketimine, imino-, -imine",
        "primary aldimine, imino-, imine",
        "secondary aldimine, imino-, -imine",
        "azo diimide, azo-, -diazene",
        "cyanate, cyanato-, alkyl cyanate",
        "isocyanate, isocyanato-, alkyl isocyanate",
        "nitrate, nitrooxy-, nitroxy-, alkyl nitrate",
        "nitrite, nitrosooxy-, alkyl nitrite",
    ],
    "GPF": [
        "nitrile, cyano-, alkanenitrile, alkyl cyanide",
        "isonitrile, isocyano-, alkaneisonitrile, alkyl isocyanide",
        "nitro compound, nitro-",
        "nitroso compound, nitroso-, nitrosyl-",
        "oxime",
        "pyridyl, 4-pyridyl, 3-pyridyl, 2-pyridyl, -pyridine",
        "carbamate, -carbamoyloxy-, -carbamate",
        "phosphine, phosphanyl-, -phosphane",
        "phosphonic acid, phosphono-, -phosphonic acid",
    ],
    "GPG": [
        "phosphate, phosphonooxy-, O-phospono-, ... phosphate",
        "phosphodiester, hydroxyphosphoryloxy-, di...hydrogen phosphate",
        "boronic acid, borono-, ... boronic acid",
        "boronate, O-alkylboronyl-, ... boronic acid di... ester",
        "borinic acid, hydroxyborino-, di... borinic acid",
        "borinate, O-alkoxydialkylboronyl-, di... borinic acid ... ester",
        "alkyllithium, -lithium",
        "alkylmagnesium halide, -magnesium halide",
        "alkylaluminium, -aluminium / -aluminum",
    ],
}


# A Type cell holds one of Quijada's seven gradient types, optionally
# starred to mark that the affix has an associated C_R root. ANG's cell
# holds its type followed by a second, differently-ordered assignment of
# its nine degrees, spilled in from somewhere:
#
#   0* 1 arc-seconds 2 arc-minutes 3 mils 4 grads 5 degrees 6 points
#   7 hour angles 8 radians 9 sextants
#
# The degree columns of the same row order them points, hour angles,
# grads, mils, radians, sextants, arc-seconds, arc-minutes, degrees,
# which is the order the affix document gives, so the spill is the stray
# text and the degrees stand. Only the type is taken from the cell.
LEGAL_TYPES = {"", "0", "A1", "A2", "B", "C", "D1", "D2"}


def clean_type(cell: str) -> str:
    head = cell.split(" ", 1)[0]
    if head.rstrip("*") not in LEGAL_TYPES:
        raise ValueError(f"unrecognized gradient type {cell!r}")
    return head


def merge_affixes(upstream: list[dict], existing_path: Path) -> list[dict]:
    """Preserve local description/type when upstream is blank, and undo
    the three-row degree shift described at SHIFTED_DEGREES, and fill
    the degree lists upstream leaves blank from RECOVERED_DEGREES.

    Match on (cs, abbrev) since the same Cs cluster can carry two
    affixes (e.g. ḑg = MDI and ḑg = S07 are both in upstream).
    """
    for a in upstream:
        a["type"] = clean_type(a["type"])
        blank = RECOVERED_DEGREES.get(a["abbrev"])
        if blank and not any(d.strip() for d in a["degrees"]):
            print(f"  filling blank degrees for {a['abbrev']} ({a['cs']}) from the PDF")
            a["degrees"] = list(blank)
        fixed = SHIFTED_DEGREES.get(a["abbrev"])
        if fixed and a["degrees"] != fixed:
            print(f"  restoring shifted degrees for {a['abbrev']} ({a['cs']})")
            a["degrees"] = list(fixed)

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
    """Write roots and affixes into data.json, preserving the grammar section.

    Versioning: monotonically increasing uint16. Version bumps only when
    roots or affixes content changes; grammar changes don't affect it.
    """
    new_content = {"roots": roots, "affixes": affixes}
    new_payload = json.dumps(new_content, sort_keys=True, ensure_ascii=False)

    prev_version = 0
    prev_payload = None
    prev_grammar: list[dict] = []
    if path.exists():
        with open(path, encoding="utf-8") as f:
            prev = json.load(f)
        prev_version = int(prev.get("version", 0))
        prev_grammar = prev.get("grammar", [])
        prev_payload = json.dumps(
            {"roots": prev.get("roots", []), "affixes": prev.get("affixes", [])},
            sort_keys=True, ensure_ascii=False,
        )

    version = prev_version if prev_payload == new_payload else prev_version + 1
    if version > 0xFFFF:
        raise ValueError(f"version overflow: {version}")

    combined = {"version": version, "grammar": prev_grammar, "roots": roots, "affixes": affixes}
    with open(path, "w", encoding="utf-8") as f:
        json.dump(combined, f, ensure_ascii=False, indent=2)
        f.write("\n")
    return version


def main() -> int:
    data_path = DATA_DIR / "data.json"

    print("Fetching roots from upstream sheet...")
    roots = parse_roots(fetch(ROOTS_URL))
    print(f"  {len(roots)} root entries")

    print("Fetching affixes from upstream sheet...")
    affixes = parse_affixes(fetch(AFFIXES_URL))
    affixes = merge_affixes_from_data(affixes, data_path)
    print(f"  {len(affixes)} affix entries")

    write_roots_tsv(roots, DATA_DIR / "roots.tsv")
    write_affixes_tsv(affixes, DATA_DIR / "affixes.tsv")
    version = write_data(roots, affixes, data_path)
    print(f"Wrote roots.tsv, affixes.tsv, data.json (version v{version})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
