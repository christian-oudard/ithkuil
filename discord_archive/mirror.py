#!/usr/bin/env python3
"""Mirror Discord guild message history to JSON files.

Uses the Discord REST API directly with a user token.
Starts from most recent messages and works backward.
Respects rate limits and stays under a configurable disk budget.
"""

import json
import os
import sys
import time
import urllib.request
import urllib.error
import urllib.parse

API_BASE = "https://discord.com/api/v10"
TOKEN = os.environ.get("DISCORD_TOKEN", "")
OUTPUT_DIR = os.environ.get("DISCORD_OUTPUT_DIR", os.path.join(os.path.dirname(__file__), "data"))
MAX_BYTES = int(os.environ.get("DISCORD_MAX_BYTES", 10 * 1024 * 1024 * 1024))  # 10GB default

HEADERS = {
    "Authorization": TOKEN,
    "Content-Type": "application/json",
    "User-Agent": "Mozilla/5.0 (X11; Linux x86_64; rv:128.0) Gecko/20100101 Firefox/128.0",
}

# Ithkuil guild
GUILD_ID = "131937038139260928"

# All text channels in priority order (V4 first)
PRIORITY_CHANNELS = [
    "700825122017378374",   # v4-only
    "509906677210808340",   # general-v4
    "725787403163271189",   # works-v4
    "725786718900453396",   # audio-v4
    "959536715733811210",   # challenges
    "478629150903500801",   # bots-and-exegesis
    "832613156290428949",   # scripts
    "422916359966687244",   # brainstorm
    "131937038139260928",   # general
    "385962947052175361",   # announcements
    "198559368772452352",   # resources
    "702636602043662336",   # sources
    "508087626516004865",   # world-building
    "184758308115054592",   # off-topic
    "836190924684918824",   # memes-and-animals
    "993145670779293777",   # music-recs
    "930161426151129100",   # bios-and-intros
    "184758428424601601",   # general-v3
    "184758408065318912",   # v3-only
    "478635181968982017",   # audio-v3
    "385962343424720897",   # works-v3
    "1165735390997655612",  # general-v2
    "1165735439383138386",  # v2-only
    "1165735487839944835",  # audio-v2
    "1165735558534934629",  # works-v2
    "1165735602835181720",  # general-v1
    "1165735642760745033",  # v1-only
    "1165735677133062205",  # audio-v1
    "1165735736230805514",  # works-v1
    "469617525110472714",   # feed
    "814639356743516191",   # submit
    "317760317281009666",   # submit-discussion
    "814639370018619482",   # complete
    "317757660713975809",   # complete-discussion
]


def api_get(path, retries=5):
    url = f"{API_BASE}{path}"
    req = urllib.request.Request(url, headers=HEADERS)
    for attempt in range(retries):
        try:
            with urllib.request.urlopen(req) as resp:
                return json.loads(resp.read().decode())
        except urllib.error.HTTPError as e:
            if e.code == 429:
                body = json.loads(e.read().decode())
                wait = body.get("retry_after", 5)
                print(f"  Rate limited, waiting {wait:.1f}s...", flush=True)
                time.sleep(wait + 0.5)
            elif e.code in (500, 502, 503):
                time.sleep(2 ** attempt)
            elif e.code == 403:
                print(f"  Forbidden (no access)", flush=True)
                return None
            elif e.code == 400:
                print(f"  Bad request: {e.read().decode()[:200]}", flush=True)
                return None
            else:
                print(f"  HTTP {e.code}: {e.read().decode()[:200]}", flush=True)
                raise
    raise RuntimeError(f"Failed after {retries} retries: {url}")


def get_disk_usage():
    total = 0
    for root, dirs, files in os.walk(OUTPUT_DIR):
        for f in files:
            total += os.path.getsize(os.path.join(root, f))
    return total


def save_json(path, data):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        json.dump(data, f, indent=2)


def archive_channel(ch_id, ch_name, guild_dir):
    ch_dir = os.path.join(guild_dir, f"{ch_name}_{ch_id}")
    os.makedirs(ch_dir, exist_ok=True)

    progress_file = os.path.join(ch_dir, "_progress.json")
    before = None
    total_messages = 0
    batch_num = 0
    if os.path.exists(progress_file):
        with open(progress_file) as f:
            progress = json.load(f)
            if progress.get("complete"):
                print(f"  Already complete ({progress.get('total_messages', 0)} messages)", flush=True)
                return progress.get("total_messages", 0)
            before = progress.get("oldest_id")
            total_messages = progress.get("total_messages", 0)
            batch_num = total_messages // 100
            print(f"  Resuming from {before} ({total_messages} already)", flush=True)

    while True:
        if get_disk_usage() >= MAX_BYTES:
            print(f"  Disk budget reached ({MAX_BYTES / 1e9:.1f}GB)", flush=True)
            return total_messages

        params = "?limit=100"
        if before:
            params += f"&before={before}"
        messages = api_get(f"/channels/{ch_id}/messages{params}")

        if messages is None or not messages:
            break

        batch_file = os.path.join(ch_dir, f"batch_{batch_num:06d}.json")
        save_json(batch_file, messages)

        total_messages += len(messages)
        before = messages[-1]["id"]
        batch_num += 1

        save_json(progress_file, {
            "oldest_id": before,
            "total_messages": total_messages,
            "last_updated": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        })

        ts = messages[-1].get("timestamp", "?")[:10]
        print(f"  #{ch_name}: {total_messages} msgs (oldest: {ts})", flush=True)

        if len(messages) < 100:
            break

        time.sleep(0.5)

    save_json(progress_file, {
        "oldest_id": before,
        "total_messages": total_messages,
        "complete": True,
        "last_updated": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
    })
    return total_messages


def fetch_archived_threads(ch_id):
    threads = []
    for kind in ["public", "private"]:
        has_more = True
        before = None
        while has_more:
            path = f"/channels/{ch_id}/threads/archived/{kind}"
            if before:
                encoded = urllib.parse.quote(before)
                path += f"?before={encoded}"
            data = api_get(path)
            if data is None:
                break
            batch = data.get("threads", [])
            threads.extend(batch)
            has_more = data.get("has_more", False)
            if batch:
                before = batch[-1].get("thread_metadata", {}).get("archive_timestamp", "")
                if not before:
                    break
    return threads


def main():
    if not TOKEN:
        print("Set DISCORD_TOKEN environment variable")
        sys.exit(1)

    os.makedirs(OUTPUT_DIR, exist_ok=True)
    guild_dir = os.path.join(OUTPUT_DIR, f"ithkuil_{GUILD_ID}")
    os.makedirs(guild_dir, exist_ok=True)

    print("Fetching channel list...", flush=True)
    channels = api_get(f"/guilds/{GUILD_ID}/channels")
    save_json(os.path.join(guild_dir, "_channels.json"), channels)
    ch_by_id = {c["id"]: c for c in channels}

    grand_total = 0

    for i, ch_id in enumerate(PRIORITY_CHANNELS):
        ch = ch_by_id.get(ch_id, {})
        ch_name = ch.get("name", ch_id)
        print(f"\n[{i+1}/{len(PRIORITY_CHANNELS)}] #{ch_name} ({ch_id})", flush=True)

        count = archive_channel(ch_id, ch_name, guild_dir)
        grand_total += count

        if get_disk_usage() >= MAX_BYTES:
            print("Disk budget reached, stopping.", flush=True)
            break

        # Threads
        threads = fetch_archived_threads(ch_id)
        if threads:
            print(f"  {len(threads)} archived threads", flush=True)
            for t in threads:
                t_name = t.get("name", t["id"])[:50].replace("/", "_")
                t_id = t["id"]
                print(f"    Thread: {t_name}", flush=True)
                t_count = archive_channel(t_id, f"thread_{t_name}_{t_id}", guild_dir)
                grand_total += t_count
                if get_disk_usage() >= MAX_BYTES:
                    break

    disk_mb = get_disk_usage() / 1e6
    print(f"\nDone! {grand_total} messages. Disk: {disk_mb:.1f}MB", flush=True)
    print(f"Output: {guild_dir}", flush=True)


if __name__ == "__main__":
    main()
