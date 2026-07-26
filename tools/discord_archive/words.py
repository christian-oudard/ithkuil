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

# The morphology we implement is v1.3.1, published in 2023, and the
# channels predate it by three years. Older messages are written in
# earlier drafts, sometimes in an alphabet v4 no longer has (dotless ı,
# grave ì and ù). Measured against our parser the break is sharp: every
# half-year through 2022 sits at 36-56% parsed, every half-year from
# 2023 on sits at 61-80%. Scoring the parser against a grammar it does
# not implement measures nothing, so those years are dropped.
SINCE = "2023-01-01"

LETTERS = "abcčçdḑefghijklļmnňoprřsštţuvwxyzžẓäëïöüáéíóúâêîôû'-"
TOKEN = re.compile(r"[%s]+" % re.escape(LETTERS), re.IGNORECASE)
ITHKUIL_ONLY = set("ţřšžňļḑçëüöäẓ")

data = json.load(open(SRC, encoding="utf-8"))
if isinstance(data, dict):
    data = [m for v in data.values() for m in v]

counts = collections.Counter()
kept_msgs = 0
dropped_msgs = 0
for m in data:
    if m.get("channel") not in V4_CHANNELS:
        continue
    if m.get("date", "") < SINCE:
        dropped_msgs += 1
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
print(f"messages scanned: {kept_msgs} (dropped {dropped_msgs} from before {SINCE})")
print(f"distinct candidate words: {len(counts)}")
print(f"total tokens: {sum(counts.values())}")
print("top 15:", [w for w, _ in counts.most_common(15)])
