#!/usr/bin/env python3
"""Extract all Ithkuil V4 text and translations from archived Discord data."""

import json
import os
import re
import sys

import paths

ARCHIVE_DIR = str(paths.guild_dir())

# Ithkuil-specific characters
ITHKUIL_CHARS = set("ţřšžňļḑçëüöäâîûôẓ")

def has_ithkuil(text):
    """Check if text contains Ithkuil-specific characters."""
    return any(c in ITHKUIL_CHARS for c in text.lower())

def extract_messages(directory):
    """Extract all messages from batch files in a directory, chronological order."""
    messages = []
    for f in sorted(os.listdir(directory)):
        if not f.startswith("batch_"):
            continue
        with open(os.path.join(directory, f)) as fh:
            batch = json.load(fh)
        messages.extend(reversed(batch))  # Reverse: batches are newest-first
    return messages

def extract_translation_pairs(messages):
    """Find messages with Ithkuil text + spoilered English translation."""
    pairs = []
    for m in messages:
        content = m.get("content", "")
        if not content.strip():
            continue

        spoilers = re.findall(r'\|\|(.+?)\|\|', content, re.DOTALL)
        non_spoiler = re.sub(r'\|\|.+?\|\|', '', content, flags=re.DOTALL).strip()

        if has_ithkuil(non_spoiler) and spoilers:
            english_parts = [s.strip() for s in spoilers
                           if len(s.strip()) > 5 and re.search(r'[a-z]{3,}', s, re.IGNORECASE)]
            if english_parts:
                pairs.append({
                    "date": m.get("timestamp", "")[:10],
                    "author": m.get("author", {}).get("username", "?"),
                    "ithkuil": non_spoiler,
                    "english": " / ".join(english_parts),
                    "message_id": m.get("id", ""),
                })
    return pairs

def extract_grammar_discussions(messages):
    """Find messages discussing grammar rules, corrections, morphology."""
    grammar_terms = re.compile(
        r'\b(specification|case|configuration|affiliation|perspective|extension|'
        r'essence|version|stem|function|valence|mood|illocution|'
        r'Ca|Vr|Vc|Vn|Cn|slot|formative|adjunct|carrier|'
        r'THM|INS|ABS|ERG|EFF|DAT|IND|AFF|STM|'
        r'BSC|CTE|CSV|OBJ|STA|DYN|'
        r'concatenat|referential|bias|register|'
        r'stress|penultimate|ultimate|antepenultimate)\b',
        re.IGNORECASE
    )

    discussions = []
    for m in messages:
        content = m.get("content", "")
        if grammar_terms.search(content) and len(content) > 30:
            discussions.append({
                "date": m.get("timestamp", "")[:10],
                "author": m.get("author", {}).get("username", "?"),
                "content": content,
                "message_id": m.get("id", ""),
            })
    return discussions

def main():
    all_pairs = []
    all_grammar = []
    all_ithkuil_messages = []

    channel_dirs = sorted(os.listdir(ARCHIVE_DIR))

    for ch_dir_name in channel_dirs:
        ch_path = os.path.join(ARCHIVE_DIR, ch_dir_name)
        if not os.path.isdir(ch_path):
            continue
        if ch_dir_name.startswith("_"):
            continue

        messages = extract_messages(ch_path)
        if not messages:
            continue

        # Translation pairs
        pairs = extract_translation_pairs(messages)
        for p in pairs:
            p["channel"] = ch_dir_name
        all_pairs.extend(pairs)

        # Grammar discussions
        grammar = extract_grammar_discussions(messages)
        for g in grammar:
            g["channel"] = ch_dir_name
        all_grammar.extend(grammar)

        # All messages containing Ithkuil text
        for m in messages:
            content = m.get("content", "")
            if has_ithkuil(content) and len(content.strip()) > 5:
                all_ithkuil_messages.append({
                    "date": m.get("timestamp", "")[:10],
                    "author": m.get("author", {}).get("username", "?"),
                    "content": content,
                    "channel": ch_dir_name,
                    "message_id": m.get("id", ""),
                })

        if pairs or grammar:
            print(f"{ch_dir_name}: {len(pairs)} translations, {len(grammar)} grammar discussions, {len(messages)} total msgs")

    # Save outputs
    out_dir = str(paths.extracted_dir())
    os.makedirs(out_dir, exist_ok=True)

    with open(os.path.join(out_dir, "translation_pairs.json"), "w") as f:
        json.dump(all_pairs, f, indent=2, ensure_ascii=False)

    with open(os.path.join(out_dir, "grammar_discussions.json"), "w") as f:
        json.dump(all_grammar, f, indent=2, ensure_ascii=False)

    with open(os.path.join(out_dir, "ithkuil_messages.json"), "w") as f:
        json.dump(all_ithkuil_messages, f, indent=2, ensure_ascii=False)

    print(f"\nTotal: {len(all_pairs)} translation pairs, {len(all_grammar)} grammar discussions, {len(all_ithkuil_messages)} Ithkuil messages")
    print(f"Output: {out_dir}/")

if __name__ == "__main__":
    main()
