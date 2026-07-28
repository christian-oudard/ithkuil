"""Pull candidate Ithkuil word tokens out of the extracted v4 archive."""
import json
import re
import collections
import pathlib
import unicodedata
import paths

SRC = pathlib.Path(paths.extracted_dir()) / "ithkuil_messages.json"
OUT = pathlib.Path(paths.extracted_dir()) / "v4_words.txt"

# Which channels count as v4. The original server ran every version of
# the language side by side, so only its two v4 channels qualify. The
# study group was founded after v4 won, so all of it does.
V4_CHANNELS = {
    "ithkuil": ("v4-only_700825122017378374", "works-v4_725787403163271189"),
    "study_group": None,  # every channel
}

# The morphology we implement is v1.3.1, published in 2023, and the
# channels predate it by three years. Older messages are written in
# earlier drafts, sometimes in an alphabet v4 no longer has (dotless ı,
# grave ì and ù). Measured against our parser the break is sharp: every
# half-year through 2022 sits at 36-56% parsed, every half-year from
# 2023 on sits at 61-80%. Scoring the parser against a grammar it does
# not implement measures nothing, so those years are dropped.
SINCE = "2023-01-01"

ALPHABET = set("abcčçdḑefghijklļmnňoprřsštţuvwxyzžẓäëïöüáéíóúâêîôû'-")
ITHKUIL_ONLY = set("ţřšžňļḑçëüöäẓ")

# Spellings of a letter the alphabet already has. Kept in step with
# surface.variants in normalize.go, which folds the same set for
# anything typed at the parser. This copy is needed because tokenizing
# happens here, before any Go code sees the text: an unrecognised
# character ends the chunk, so a typographic apostrophe would split
# "wala’na" into "wala" and "na" and feed both to the audit as words.
#
# Only letters v4 already has are folded. The pre-v4 ones — dotless ı,
# grave ì and ù, đ — stay unrecognised on purpose, so a word carrying
# one is dropped whole rather than rewritten into something that parses.
VARIANTS = str.maketrans({
    "’": "'",  # ’ right single quotation mark
    "‘": "'",  # ‘ left single quotation mark
    "ʼ": "'",  # ʼ modifier letter apostrophe
    "ț": "ţ",  # ț t-comma → ţ t-cedilla
    "Ț": "Ţ",
})


def normalize(text):
    """Fold letter spellings v4 has under a different code point.

    NFC first: an accented vowel typed as a base letter plus a
    combining acute is two code points, and the combining mark is not
    in the alphabet, so "á" would end the chunk after "a".
    """
    return unicodedata.normalize("NFC", text).translate(VARIANTS)

# A chunk is a run of letters, optionally joined by the apostrophe that
# writes the glottal stop or the hyphen that joins a §3.1.7 chain. The
# run is taken whole and then required to lie inside the alphabet, so a
# word carrying one foreign letter is dropped rather than split around
# it. Matching the alphabet directly would instead cut "ıţkuil" down to
# "ţkuil" and feed that fragment to the audit as a parse failure.
CHUNK = re.compile(r"[^\W\d_]+(?:['\-][^\W\d_]+)*", re.UNICODE)


def tokens(text):
    """Candidate Ithkuil word tokens in one message's text."""
    # Drop URLs and code spans, which carry Latin junk.
    text = re.sub(r"https?://\S+", " ", text)
    text = re.sub(r"`[^`]*`", " ", text)
    text = normalize(text)
    out = []
    for tok in CHUNK.findall(text.lower()):
        tok = tok.strip("-'")
        if len(tok) < 3 or not set(tok) <= ALPHABET:
            continue
        # Require a distinctively Ithkuil letter so English words drop out.
        if ITHKUIL_ONLY & set(tok):
            out.append(tok)
    return out


def in_scope(m):
    if m.get("guild") not in V4_CHANNELS:
        return False
    allowed = V4_CHANNELS[m["guild"]]
    return allowed is None or m.get("channel") in allowed


def main():
    data = json.load(open(SRC, encoding="utf-8"))
    if isinstance(data, dict):
        data = [m for v in data.values() for m in v]

    counts = collections.Counter()
    kept_msgs = 0
    dropped_msgs = 0
    bot_msgs = 0
    for m in data:
        if not in_scope(m):
            continue
        # Bot output is another implementation's opinion, not attested
        # usage, and the dictionary bots post bare roots that are not
        # words.
        if m.get("bot"):
            bot_msgs += 1
            continue
        if m.get("date", "") < SINCE:
            dropped_msgs += 1
            continue
        text = m.get("content", "")
        if not text.strip():
            continue
        kept_msgs += 1
        counts.update(tokens(text))

    OUT.write_text("\n".join(w for w, _ in counts.most_common()), encoding="utf-8")
    print(f"messages scanned: {kept_msgs} "
          f"(dropped {dropped_msgs} from before {SINCE}, {bot_msgs} from bots)")
    print(f"distinct candidate words: {len(counts)}")
    print(f"total tokens: {sum(counts.values())}")
    print("top 15:", [w for w, _ in counts.most_common(15)])


if __name__ == "__main__":
    main()
