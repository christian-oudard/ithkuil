"""
Ithkuil V3 typesetting: lay out a sequence of consonant characters into a word.

Usage:
    from v3.typeset import typeset_word
    paper = typeset_word([
        (K, HookRightOnBottom, Normal),
        (S, Normal, Normal),
    ])
    svg = paper.to_svg()

Each character is a (consonant_class, side_ending_class, bottom_ending_class)
triple. Characters are placed left-to-right with a small horizontal gap.
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from pen import Paper
from .common import MIDDLE, TOP, BOTTOM


# Horizontal spacing between character bodies.
# Characters are centered on x=0 in their own paper; we shift them.
CHAR_SPACING = 0.5  # units of gap between adjacent bounding boxes


def typeset_word(chars, width=0.5):
    """
    Lay out a sequence of (consonant_class, side_ending_class, bottom_ending_class)
    tuples into a single Paper.

    width: pen stroke width (default 0.5)
    Returns a Paper in the usual Y-up coordinate system.
    """
    word_paper = Paper()
    cursor_x = 0.0

    for consonant_cls, side_cls, bottom_cls in chars:
        char = consonant_cls(side_cls, bottom_cls)
        char_paper = char.draw_character(width=width)
        b = char_paper.bounds()

        # Align left edge of this character to cursor_x
        shift_x = cursor_x - b.left
        char_paper.translate(shift_x, 0)
        word_paper.merge(char_paper)

        # Advance cursor past right edge + spacing
        cursor_x += (b.right - b.left) + CHAR_SPACING

    return word_paper
