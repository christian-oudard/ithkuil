#!/usr/bin/env python3
"""Check that the Discord token works, and list a guild's channels.

Read-only, and prints nothing that identifies the token. Run this
before mirror.py to confirm access and to see what is there:

    python3 probe.py [GUILD_ID]
"""
import sys

import mirror
import paths


def main():
    mirror.HEADERS["Authorization"] = paths.token()
    guild_id = sys.argv[1] if len(sys.argv) > 1 else paths.GUILD_ID

    me = mirror.api_get("/users/@me")
    if not me:
        raise SystemExit("token rejected")
    print(f"authenticated as {me['username']} ({me['id']})")

    guild = mirror.api_get(f"/guilds/{guild_id}")
    if not guild:
        raise SystemExit(f"no access to guild {guild_id}")
    print(f"guild: {guild['name']} ({guild_id})")

    channels = mirror.api_get(f"/guilds/{guild_id}/channels")
    order, by_id = mirror.channel_order(channels)
    cats = {c["id"]: c["name"] for c in channels if c.get("type") == 4}
    print(f"{len(order)} message-bearing channels of {len(channels)} total:")
    for ch_id in order:
        c = by_id[ch_id]
        parent = cats.get(c.get("parent_id"), "-")
        print(f"  {ch_id}  {parent:<20} #{c['name']}")


if __name__ == "__main__":
    main()
