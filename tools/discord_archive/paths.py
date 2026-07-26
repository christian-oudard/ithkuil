"""Shared locations for the Discord archive tools.

The mirror and its extracts are generated data, so they live outside the
source tree: $XDG_DATA_HOME/ithkuil/discord/, or ~/.local/share/ithkuil/
discord/ when XDG_DATA_HOME is unset. Set ITHKUIL_DISCORD_DIR to override.
"""
import os
from pathlib import Path

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


def guild_dir() -> Path:
    """The one guild we mirror, inside mirror_dir()."""
    return mirror_dir() / f"ithkuil_{GUILD_ID}"


def extracted_dir() -> Path:
    """Ithkuil text and analyses derived from the mirror."""
    return base_dir() / "extracted"
