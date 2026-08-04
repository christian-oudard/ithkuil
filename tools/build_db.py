#!/usr/bin/env python3
"""Build the SQLite data store from data/data.json.

Usage:
    python3 tools/build_db.py [--lang LANG] [-o PATH]

LANG selects which data file to read: data/data.LANG.json (default: data/data.json).

The database is a build artifact, so it is written outside the source tree:
$XDG_DATA_HOME/ithkuil/data.db, or ~/.local/share/ithkuil/data.db when
XDG_DATA_HOME is unset. This is the path store.DefaultPath() reads in Go.
"""
import argparse
import json
import os
import sqlite3
from collections import Counter
from pathlib import Path

DATA_DIR = Path(__file__).resolve().parent.parent / "data"


def default_db_path() -> Path:
    xdg = os.environ.get("XDG_DATA_HOME")
    base = Path(xdg) if xdg else Path.home() / ".local" / "share"
    return base / "ithkuil" / "data.db"

SCHEMA = """
CREATE TABLE grammar (
    abbrev      TEXT NOT NULL PRIMARY KEY CHECK (abbrev <> ''),
    name        TEXT NOT NULL DEFAULT '',
    category    TEXT NOT NULL DEFAULT '',
    form        TEXT NOT NULL DEFAULT '',
    description TEXT NOT NULL DEFAULT '',
    explanation TEXT NOT NULL DEFAULT '',
    guidance    TEXT NOT NULL DEFAULT ''
);

-- Explanations that do not belong to any one value of a category: a
-- construction, a slot, an affix pattern. Keyed by their own name
-- because there is no abbreviation to hang them on.
CREATE TABLE topics (
    key         TEXT NOT NULL PRIMARY KEY CHECK (key <> ''),
    category    TEXT NOT NULL DEFAULT '',
    name        TEXT NOT NULL DEFAULT '',
    explanation TEXT NOT NULL DEFAULT '',
    guidance    TEXT NOT NULL DEFAULT ''
);

CREATE TABLE roots (
    cr            TEXT NOT NULL PRIMARY KEY CHECK (cr <> ''),
    stem0         TEXT NOT NULL DEFAULT '',
    stem1         TEXT NOT NULL DEFAULT '',
    stem2         TEXT NOT NULL DEFAULT '',
    stem3         TEXT NOT NULL DEFAULT '',
    contential    TEXT NOT NULL DEFAULT '',
    constitutive  TEXT NOT NULL DEFAULT '',
    dynamic       TEXT NOT NULL DEFAULT '',
    objective     TEXT NOT NULL DEFAULT '[]',
    completive    TEXT NOT NULL DEFAULT '[]',
    wikidata      TEXT NOT NULL DEFAULT '[]'
);

CREATE TABLE affixes (
    cs          TEXT NOT NULL PRIMARY KEY CHECK (cs <> ''),
    abbrev      TEXT NOT NULL DEFAULT '',
    description TEXT NOT NULL DEFAULT '',
    type        TEXT NOT NULL DEFAULT '',
    degrees     TEXT NOT NULL DEFAULT '[]'
);

-- porter stems, so "cats" finds a cat and "speaks" finds speech.
-- Without it a search matched whole tokens or prefixes of them, and an
-- inflected query found almost nothing: "trees" answered with 2 roots
-- against "tree"'s 306, and "studies" with none at all.
--
-- search.Stem is the same algorithm in Go, for the browser, which has
-- no SQLite. FTS5's tokenizer is original Porter rather than Snowball
-- English, which is why that one is too; store/stem_parity_test.go
-- holds the two against each other over the whole lexicon.
CREATE VIRTUAL TABLE roots_fts USING fts5(
    cr, stem0, stem1, stem2, stem3,
    contential, constitutive, dynamic,
    content=roots, content_rowid=rowid,
    tokenize="porter unicode61"
);

CREATE VIRTUAL TABLE affixes_fts USING fts5(
    cs, abbrev, description,
    content=affixes, content_rowid=rowid,
    tokenize="porter unicode61"
);
"""


# A C_R names one root, so the roots table is keyed on it and the key
# is unique. Four clusters carry two unrelated live meanings upstream,
# which the key cannot express:
#
#   cfw   magnoliaceae            / myristicaceae
#   lzbḑ  psychodomorph           / tabanid fly
#   nļt   groin undergarment      / cicadomorphic bug
#   rţnw  vitaceae 2              / rosoideae 7
#
# Neither side is marked retired, so there is nothing to choose between
# them and the first is kept. A fifth, ksmy "oven" against "jagged
# line", used to be here and is gone: the sheet had daggered the oven
# sense and sync_lexicon now drops retired entries before they reach
# data.json.
#
# Listed rather than tolerated, so the loss is deliberate rather than
# whatever INSERT OR REPLACE happened to do. A new collision, or one of
# these being repaired upstream, fails the build.
KNOWN_DUPLICATE_ROOTS = {"cfw", "lzbḑ", "nļt", "rţnw"}


def dedupe_roots(roots: list[dict]) -> list[dict]:
    collisions = {cr for cr, n in Counter(r["cr"] for r in roots).items() if n > 1}
    if collisions != KNOWN_DUPLICATE_ROOTS:
        raise ValueError(
            f"duplicate C_R set changed: {sorted(collisions)} "
            f"!= {sorted(KNOWN_DUPLICATE_ROOTS)}"
        )
    kept: dict[str, dict] = {}
    for r in roots:
        if r["cr"] in kept:
            print(f"  duplicate C_R {r['cr']}: dropping {r.get('stem0', '')!r}")
            continue
        kept[r["cr"]] = r
    return list(kept.values())


def build(data_path: Path, db_path: Path) -> None:
    with open(data_path, encoding="utf-8") as f:
        data = json.load(f)

    if db_path.exists():
        db_path.unlink()

    conn = sqlite3.connect(db_path)
    conn.executescript(SCHEMA)

    conn.executemany(
        "INSERT INTO grammar VALUES (?,?,?,?,?,?,?)",
        [
            (
                e["abbrev"],
                e.get("name", ""),
                e.get("category", ""),
                e.get("form", ""),
                e.get("description", ""),
                e.get("explanation", ""),
                e.get("guidance", ""),
            )
            for e in data["grammar"]
        ],
    )

    conn.executemany(
        "INSERT INTO topics VALUES (?,?,?,?,?)",
        [
            (
                t["key"],
                t.get("category", ""),
                t.get("name", ""),
                t.get("explanation", ""),
                t.get("guidance", ""),
            )
            for t in data.get("topics", [])
        ],
    )

    conn.executemany(
        "INSERT INTO roots VALUES (?,?,?,?,?,?,?,?,?,?,?)",
        [
            (
                r["cr"],
                r.get("stem0", ""),
                r.get("stem1", ""),
                r.get("stem2", ""),
                r.get("stem3", ""),
                r.get("contential", ""),
                r.get("constitutive", ""),
                r.get("dynamic", ""),
                json.dumps(r.get("objective", []), ensure_ascii=False),
                json.dumps(r.get("completive", []), ensure_ascii=False),
                json.dumps(r.get("wikidata", []), ensure_ascii=False),
            )
            for r in dedupe_roots(data["roots"])
        ],
    )
    conn.execute('INSERT INTO roots_fts(roots_fts) VALUES ("rebuild")')

    conn.executemany(
        "INSERT INTO affixes VALUES (?,?,?,?,?)",
        [
            (
                a["cs"],
                a.get("abbrev", ""),
                a.get("description", ""),
                a.get("type", ""),
                json.dumps(a.get("degrees", []), ensure_ascii=False),
            )
            for a in data["affixes"]
        ],
    )
    conn.execute('INSERT INTO affixes_fts(affixes_fts) VALUES ("rebuild")')

    counts = {
        t: conn.execute(f"SELECT count(*) FROM {t}").fetchone()[0]
        for t in ("grammar", "topics", "roots", "affixes")
    }
    conn.commit()
    conn.close()

    print(f"Built {db_path} ({db_path.stat().st_size // 1024} KB)")
    for t, n in counts.items():
        print(f"  {t + ':':9}{n} entries")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--lang", default="", help="language suffix (e.g. 'zh' → data.zh.json)")
    parser.add_argument("-o", "--output", type=Path, default=default_db_path(),
                        help="output path (default: $XDG_DATA_HOME/ithkuil/data.db)")
    args = parser.parse_args()

    suffix = f".{args.lang}" if args.lang else ""
    data_path = DATA_DIR / f"data{suffix}.json"
    args.output.parent.mkdir(parents=True, exist_ok=True)
    build(data_path, args.output)


if __name__ == "__main__":
    main()
