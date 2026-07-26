"""Pull candidate Ithkuil word tokens out of the extracted v4 archive."""
import json
import re
import collections
import pathlib
import paths

SRC = pathlib.Path(paths.extracted_dir()) / "ithkuil_messages.json"
OUT = pathlib.Path(paths.extracted_dir()) / "v4_words.txt"

# Only the two channels that are unambiguously v4.
V4_CHANNELS = ("v4-only_700825122017378374", "works-v4_725787403163271189")

LETTERS = "abcçdḑfghijklļmnňoprřsštţuvwxyzžẓäëïöüáéíóúâêîôû'-"
TOKEN = re.compile(r"[%s]+" % re.escape(LETTERS), re.IGNORECASE)
ITHKUIL_ONLY = set("ţřšžňļḑçëüöäẓ")

data = json.load(open(SRC, encoding="utf-8"))
if isinstance(data, dict):
    data = [m for v in data.values() for m in v]

counts = collections.Counter()
kept_msgs = 0
for m in data:
    if m.get("channel") not in V4_CHANNELS:
        continue
    text = m.get("content", "")
    if not text.strip():
        continue
    # Drop URLs and code spans, which carry Latin junk.
    text = re.sub(r"https?://\S+", " ", text)
    text = re.sub(r"`[^`]*`", " ", text)
    kept_msgs += 1
    for tok in TOKEN.findall(text.lower()):
        tok = tok.strip("-'")
        # Require a distinctively Ithkuil letter so English words drop out.
        if len(tok) >= 3 and ITHKUIL_ONLY & set(tok):
            counts[tok] += 1

OUT.write_text("\n".join(w for w, _ in counts.most_common()), encoding="utf-8")
print(f"messages scanned: {kept_msgs}")
print(f"distinct candidate words: {len(counts)}")
print(f"total tokens: {sum(counts.values())}")
print("top 15:", [w for w, _ in counts.most_common(15)])
