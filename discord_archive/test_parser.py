#!/usr/bin/env python3
"""Test our Ithkuil parser against words found in Discord conversations.

Extracts individual Ithkuil words, runs them through our parser in batches,
and reports failures vs successes.
"""

import json
import os
import re
import subprocess
import sys

EXTRACTED_DIR = os.path.join(os.path.dirname(__file__), "extracted")
ITHKUIL_CHARS = set("ţřšžňļḑçëüöäâîûôẓ")


def extract_ithkuil_words(text):
    """Extract individual Ithkuil words from text."""
    text = re.sub(r'\|\|.+?\|\|', '', text, flags=re.DOTALL)
    text = re.sub(r'<:[^>]+>', '', text)
    text = re.sub(r'https?://\S+', '', text)
    text = re.sub(r'[*_~`]', '', text)

    words = []
    for word in text.split():
        word = word.strip('.,!?;:()[]{}"""\'')
        if not word:
            continue
        if any(c in ITHKUIL_CHARS for c in word.lower()):
            alpha = sum(1 for c in word if c.isalpha() or c in "''-")
            if alpha >= 3:
                words.append(word)
    return words


def run_parser_batch(words, batch_size=100):
    """Run parser on a batch of words. Returns dict of word -> (success, output)."""
    results = {}
    for i in range(0, len(words), batch_size):
        batch = words[i:i+batch_size]
        input_text = "\n".join(batch)
        try:
            result = subprocess.run(
                ["nix-shell", "--run",
                 f"cabal run ithkuil-gloss -- 2>/dev/null <<'WORDS'\n{input_text}\nWORDS"],
                capture_output=True, text=True, timeout=120,
                cwd="/projects/ithkuil"
            )
            output = result.stdout

            # Parse output: each word section starts with the bold word
            # Pattern: \x1b[1m<word>\x1b[0m
            sections = re.split(r'\x1b\[1m', output)
            current_word = None
            for section in sections:
                if not section.strip():
                    continue
                # First line has the word
                match = re.match(r'(.+?)\x1b\[0m', section)
                if match:
                    w = match.group(1).strip()
                    has_gloss = "GLOSS:" in section
                    has_error = "error" in section.lower() or "unknown" in section.lower()
                    # Strip ANSI codes for clean output
                    clean = re.sub(r'\x1b\[[0-9;]*m', '', section).strip()
                    results[w.lower()] = (has_gloss and not has_error, clean)
        except subprocess.TimeoutExpired:
            for w in batch:
                if w.lower() not in results:
                    results[w.lower()] = (False, "(timeout)")
        except Exception as e:
            for w in batch:
                if w.lower() not in results:
                    results[w.lower()] = (False, str(e))

        done = min(i + batch_size, len(words))
        print(f"  Tested {done}/{len(words)} words...", flush=True)

    return results


def main():
    with open(os.path.join(EXTRACTED_DIR, "ithkuil_messages.json")) as f:
        messages = json.load(f)

    # Extract unique words
    word_sources = {}
    for m in messages:
        words = extract_ithkuil_words(m["content"])
        for w in words:
            w_lower = w.lower()
            if w_lower not in word_sources:
                word_sources[w_lower] = {
                    "word": w,
                    "author": m["author"],
                    "date": m["date"],
                    "context": m["content"][:200],
                }

    print(f"Found {len(word_sources)} unique Ithkuil words from Discord")

    # Test all words
    all_words = [word_sources[w]["word"] for w in sorted(word_sources.keys())]
    print(f"Testing {len(all_words)} words in batches...")

    results = run_parser_batch(all_words)

    successes = []
    failures = []
    not_found = []

    for w_lower, info in sorted(word_sources.items()):
        if w_lower in results:
            success, output = results[w_lower]
            entry = {"word": info["word"], "output": output[:500], **info}
            if success:
                successes.append(entry)
            else:
                failures.append(entry)
        else:
            not_found.append(info)

    total = len(successes) + len(failures)
    rate = f"{len(successes)/total*100:.1f}%" if total else "N/A"

    output = {
        "total_unique_words": len(word_sources),
        "tested": total,
        "successes": len(successes),
        "failures": len(failures),
        "not_parsed": len(not_found),
        "success_rate": rate,
        "failure_list": failures,
    }

    out_file = os.path.join(EXTRACTED_DIR, "parser_test_results.json")
    with open(out_file, "w") as f:
        json.dump(output, f, indent=2, ensure_ascii=False)

    print(f"\nResults: {len(successes)}/{total} passed ({rate})")
    print(f"Saved to {out_file}")

    if failures:
        print(f"\nTop 30 failures:")
        for item in failures[:30]:
            print(f"  {item['word']}")
            # Show just the first line of output
            first_line = item['output'].split('\n')[0][:100] if item['output'] else ""
            print(f"    -> {first_line}")


if __name__ == "__main__":
    main()
