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
    abbrev      TEXT PRIMARY KEY,
    name        TEXT NOT NULL DEFAULT '',
    category    TEXT NOT NULL DEFAULT '',
    form        TEXT NOT NULL DEFAULT '',
    description TEXT NOT NULL DEFAULT '',
    explanation TEXT NOT NULL DEFAULT ''
);

CREATE TABLE roots (
    cr            TEXT PRIMARY KEY,
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
    cs          TEXT PRIMARY KEY,
    abbrev      TEXT NOT NULL DEFAULT '',
    description TEXT NOT NULL DEFAULT '',
    type        TEXT NOT NULL DEFAULT '',
    degrees     TEXT NOT NULL DEFAULT '[]'
);

CREATE VIRTUAL TABLE roots_fts USING fts5(
    cr, stem0, stem1, stem2, stem3,
    contential, constitutive, dynamic,
    content=roots, content_rowid=rowid
);

CREATE VIRTUAL TABLE affixes_fts USING fts5(
    cs, abbrev, description,
    content=affixes, content_rowid=rowid
);
"""


# Five C_R clusters carry two unrelated meanings in the community
# lexicon, and the roots table is keyed on C_R, so one meaning of each
# pair cannot be stored. Only ksmy resolves: its "oven" entry is
# daggered, the sheet's mark for a retired word, so the live "jagged
# line" is the one to keep. The other four are live homonyms with
# nothing to choose between them, and the entry kept is simply the
# first.
#
# Listed rather than tolerated so the loss is deliberate. A new
# collision, or one of these being repaired upstream, fails the build.
KNOWN_DUPLICATE_ROOTS = {"cfw", "ksmy", "lzbḑ", "nļt", "rţnw"}


def dedupe_roots(roots: list[dict]) -> list[dict]:
    kept: dict[str, dict] = {}
    for r in roots:
        prior = kept.get(r["cr"])
        if prior is None or prior.get("stem0", "").endswith("†"):
            kept[r["cr"]] = r
    seen = Counter(r["cr"] for r in roots)
    collisions = {cr for cr, n in seen.items() if n > 1}
    if collisions != KNOWN_DUPLICATE_ROOTS:
        raise ValueError(
            f"duplicate C_R set changed: {sorted(collisions)} "
            f"!= {sorted(KNOWN_DUPLICATE_ROOTS)}"
        )
    for cr in sorted(collisions):
        dropped = [r for r in roots if r["cr"] == cr and r is not kept[cr]]
        for d in dropped:
            print(f"  duplicate C_R {cr}: dropping {d.get('stem0', '')!r}")
    return list(kept.values())


def build(data_path: Path, db_path: Path) -> None:
    with open(data_path, encoding="utf-8") as f:
        data = json.load(f)

    if db_path.exists():
        db_path.unlink()

    conn = sqlite3.connect(db_path)
    conn.executescript(SCHEMA)

    conn.executemany(
        "INSERT INTO grammar VALUES (?,?,?,?,?,?)",
        [
            (
                e["abbrev"],
                e.get("name", ""),
                e.get("category", ""),
                e.get("form", ""),
                e.get("description", ""),
                e.get("explanation", ""),
            )
            for e in data["grammar"]
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
        "INSERT OR REPLACE INTO affixes VALUES (?,?,?,?,?)",
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
        for t in ("grammar", "roots", "affixes")
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
