#!/usr/bin/env python3
"""Build data/data.db (SQLite) from data/data.json.

Usage (from repo root):
    python3 data/build_db.py [--lang LANG]

LANG selects which data file to read: data/data.LANG.json (default: data/data.json).
The output is always data/data.db.
"""
import argparse
import json
import sqlite3
from pathlib import Path

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
        "INSERT OR REPLACE INTO roots VALUES (?,?,?,?,?,?,?,?,?,?,?)",
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
            for r in data["roots"]
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

    conn.commit()
    conn.close()

    print(f"Built {db_path} ({db_path.stat().st_size // 1024} KB)")
    print(f"  grammar: {len(data['grammar'])} entries")
    print(f"  roots:   {len(data['roots'])} entries")
    print(f"  affixes: {len(data['affixes'])} entries")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--lang", default="", help="language suffix (e.g. 'zh' → data.zh.json)")
    args = parser.parse_args()

    data_dir = Path(__file__).parent
    suffix = f".{args.lang}" if args.lang else ""
    data_path = data_dir / f"data{suffix}.json"
    db_path = data_dir / "data.db"
    build(data_path, db_path)


if __name__ == "__main__":
    main()
