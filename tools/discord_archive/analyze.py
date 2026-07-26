#!/usr/bin/env python3
"""Analyze Ithkuil V4 Discord archive for patterns, idioms, and interesting usage."""

import json
import os
import re
from collections import Counter

import paths

EXTRACTED_DIR = str(paths.extracted_dir())
ITHKUIL_CHARS = set("ţřšžňļḑçëüöäâîûôẓ")


def load_messages():
    with open(os.path.join(EXTRACTED_DIR, "ithkuil_messages.json")) as f:
        return json.load(f)


def load_translations():
    with open(os.path.join(EXTRACTED_DIR, "translation_pairs.json")) as f:
        return json.load(f)


def load_grammar():
    with open(os.path.join(EXTRACTED_DIR, "grammar_discussions.json")) as f:
        return json.load(f)


def extract_words(text):
    text = re.sub(r'\|\|.+?\|\|', '', text, flags=re.DOTALL)
    text = re.sub(r'<:[^>]+>', '', text)
    text = re.sub(r'https?://\S+', '', text)
    text = re.sub(r'[*_~`]', '', text)
    words = []
    for word in text.split():
        word = word.strip('.,!?;:()[]{}"""\'')
        if word and any(c in ITHKUIL_CHARS for c in word.lower()) and len(word) >= 3:
            words.append(word.lower())
    return words


def analyze_common_roots(messages):
    """Find most commonly used roots."""
    all_words = []
    for m in messages:
        all_words.extend(extract_words(m["content"]))
    return Counter(all_words)


def analyze_prolific_authors(messages):
    """Who writes the most Ithkuil?"""
    author_counts = Counter()
    author_words = Counter()
    for m in messages:
        words = extract_words(m["content"])
        if words:
            author_counts[m["author"]] += 1
            author_words[m["author"]] += len(words)
    return author_counts, author_words


def find_long_texts(messages):
    """Find substantial Ithkuil texts (multi-sentence)."""
    long_texts = []
    for m in messages:
        content = m["content"]
        # Remove spoilers for word counting
        clean = re.sub(r'\|\|.+?\|\|', '', content, flags=re.DOTALL)
        words = extract_words(clean)
        if len(words) >= 5:
            long_texts.append({
                "date": m["date"],
                "author": m["author"],
                "content": content,
                "word_count": len(words),
                "channel": m.get("channel", ""),
            })
    long_texts.sort(key=lambda x: x["word_count"], reverse=True)
    return long_texts


def find_common_phrases(messages):
    """Find common 2-3 word phrases."""
    bigrams = Counter()
    trigrams = Counter()
    for m in messages:
        words = extract_words(m["content"])
        for i in range(len(words) - 1):
            bigrams[f"{words[i]} {words[i+1]}"] += 1
        for i in range(len(words) - 2):
            trigrams[f"{words[i]} {words[i+1]} {words[i+2]}"] += 1
    return bigrams, trigrams


def find_corrections(grammar):
    """Find messages where someone corrects an Ithkuil form."""
    corrections = []
    correction_pat = re.compile(
        r'(should be|instead of|not .{1,20} but|correct.{0,10} would be|'
        r'it.s actually|you mean|fixed|correction)',
        re.IGNORECASE
    )
    for m in grammar:
        if correction_pat.search(m["content"]) and len(m["content"]) > 30:
            corrections.append(m)
    return corrections


def find_technique_discussions(grammar):
    """Find discussions about translation techniques and strategies."""
    technique_pat = re.compile(
        r'(how (do|would) (you|I|we)|the (way|trick|idea) (is|to)|'
        r'you can (also |)use|instead.{0,10}(use|try)|'
        r'better (way|approach)|modal|frame|'
        r'case.?frame|referential|carrier|concatenat)',
        re.IGNORECASE
    )
    techniques = []
    for m in grammar:
        if technique_pat.search(m["content"]) and len(m["content"]) > 80:
            techniques.append(m)
    return techniques


def main():
    print("Loading data...", flush=True)
    messages = load_messages()
    translations = load_translations()
    grammar = load_grammar()

    results = {}

    # 1. Prolific authors
    print("Analyzing authors...", flush=True)
    auth_msgs, auth_words = analyze_prolific_authors(messages)
    results["prolific_authors"] = [
        {"author": a, "messages": auth_msgs[a], "words": auth_words[a]}
        for a, _ in auth_words.most_common(20)
    ]
    print(f"  Top authors: {', '.join(a for a, _ in auth_words.most_common(5))}")

    # 2. Common words
    print("Counting word frequencies...", flush=True)
    word_freq = analyze_common_roots(messages)
    results["common_words"] = [
        {"word": w, "count": c} for w, c in word_freq.most_common(100)
    ]

    # 3. Long texts (multi-word Ithkuil compositions)
    print("Finding long texts...", flush=True)
    long_texts = find_long_texts(messages)
    results["long_texts"] = long_texts[:100]
    print(f"  {len(long_texts)} multi-word texts found, longest: {long_texts[0]['word_count']} words")

    # 4. Common phrases
    print("Finding common phrases...", flush=True)
    bigrams, trigrams = find_common_phrases(messages)
    results["common_bigrams"] = [
        {"phrase": p, "count": c} for p, c in bigrams.most_common(50)
    ]
    results["common_trigrams"] = [
        {"phrase": p, "count": c} for p, c in trigrams.most_common(30)
    ]

    # 5. Translation pairs (clean up)
    print(f"Processing {len(translations)} translation pairs...", flush=True)
    # Deduplicate and clean
    seen = set()
    clean_translations = []
    for t in translations:
        key = (t["ithkuil"].strip(), t["english"].strip())
        if key not in seen and len(t["english"]) > 10:
            seen.add(key)
            clean_translations.append(t)
    clean_translations.sort(key=lambda x: len(x["ithkuil"]), reverse=True)
    results["translations"] = clean_translations
    print(f"  {len(clean_translations)} unique translation pairs")

    # 6. Corrections (community grammar fixes)
    print("Finding corrections...", flush=True)
    corrections = find_corrections(grammar)
    results["corrections"] = corrections[:200]
    print(f"  {len(corrections)} correction messages")

    # 7. Technique discussions
    print("Finding technique discussions...", flush=True)
    techniques = find_technique_discussions(grammar)
    results["techniques"] = techniques[:200]
    print(f"  {len(techniques)} technique discussions")

    # 8. Interesting thread titles (all in Ithkuil!)
    thread_titles = []
    archive_dir = str(paths.guild_dir())
    for d in sorted(os.listdir(archive_dir)):
        if d.startswith("thread_") and any(c in ITHKUIL_CHARS for c in d):
            # Extract title from directory name
            title = d.split("_", 1)[1].rsplit("_", 2)[0]
            thread_titles.append(title)
    results["thread_titles"] = thread_titles

    # Save
    out_file = os.path.join(EXTRACTED_DIR, "analysis.json")
    with open(out_file, "w") as f:
        json.dump(results, f, indent=2, ensure_ascii=False)
    print(f"\nSaved to {out_file}")

    # Print highlights
    print("\n" + "="*60)
    print("HIGHLIGHTS")
    print("="*60)

    print("\n--- TOP 20 MOST COMMON WORDS ---")
    for item in results["common_words"][:20]:
        print(f"  {item['count']:4d}x  {item['word']}")

    print("\n--- TOP 10 COMMON BIGRAMS ---")
    for item in results["common_bigrams"][:10]:
        print(f"  {item['count']:3d}x  {item['phrase']}")

    print(f"\n--- TOP 5 LONGEST TEXTS ---")
    for t in results["long_texts"][:5]:
        content = t["content"][:200].replace('\n', ' ')
        print(f"  [{t['date']}] {t['author']} ({t['word_count']} words)")
        print(f"    {content}")
        print()

    print(f"\n--- SAMPLE TRANSLATIONS ---")
    # Show the best ones: both Ithkuil and English are substantial
    good = [t for t in clean_translations if len(t["ithkuil"]) > 15 and len(t["english"]) > 20]
    for t in good[:15]:
        print(f"  [{t['date']}] {t['author']}")
        print(f"    {t['ithkuil'].strip()[:100]}")
        print(f"    = {t['english'].strip()[:100]}")
        print()


if __name__ == "__main__":
    main()
