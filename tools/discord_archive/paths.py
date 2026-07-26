"""Shared locations for the Discord archive tools.

The mirror and its extracts are generated data, so they live outside the
source tree: $XDG_DATA_HOME/ithkuil/discord/, or ~/.local/share/ithkuil/
discord/ when XDG_DATA_HOME is unset. Set ITHKUIL_DISCORD_DIR to override.
"""
import os
from pathlib import Path

# The servers we mirror, newest first. The community moved to a
# revamped server in 2025; the original is kept because it holds every
# message before that.
GUILDS = {
    # "New Ithkuil Study Group (hlacnyo'unfé-ediláu)", founded 2025.
    "1345994901200044072": "study_group",
    # The original server, which holds everything before that.
    "131937038139260928": "ithkuil",
}
GUILD_ID = "131937038139260928"


def base_dir() -> Path:
    override = os.environ.get("ITHKUIL_DISCORD_DIR")
    if override:
        return Path(override)
    xdg = os.environ.get("XDG_DATA_HOME")
    root = Path(xdg) if xdg else Path.home() / ".local" / "share"
    return root / "ithkuil" / "discord"


def mirror_dir() -> Path:
    """Raw message JSON as fetched from the Discord API."""
    return base_dir() / "mirror"


def guild_dir(guild_id: str = GUILD_ID) -> Path:
    """One guild's raw messages, inside mirror_dir()."""
    return mirror_dir() / f"{GUILDS.get(guild_id, 'guild')}_{guild_id}"


def token() -> str:
    """The Discord user token, from DISCORD_TOKEN or from a file.

    Discord tokens are short-lived, so the file is the usual route: it
    is copied into the data directory and never read into anything that
    gets printed or committed.
    """
    env = os.environ.get("DISCORD_TOKEN")
    if env:
        return env.strip()
    path = base_dir() / "token"
    if path.exists():
        return path.read_text().strip()
    raise SystemExit(f"no Discord token: set DISCORD_TOKEN or write {path}")


def extracted_dir() -> Path:
    """Ithkuil text and analyses derived from the mirror."""
    return base_dir() / "extracted"
