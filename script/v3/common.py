"""Constants and base classes for the Ithkuil V3 writing system."""

import math
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from pen import Paper, Pen, Bounds

# Calligraphic slant factors: 1/sin(angle)
slant45 = 1 / math.sin(math.radians(45))
slant60 = 1 / math.sin(math.radians(60))
slant75 = 1 / math.sin(math.radians(75))

# Vertical layout constants (Y-up)
HALF_HEIGHT = 3.5
BOTTOM = 0
MIDDLE = BOTTOM + HALF_HEIGHT
TOP    = MIDDLE + HALF_HEIGHT
UNDER  = BOTTOM - 2.0
OVER   = TOP + 2.0


class Character:

    mirrored_x = False
    mirrored_y = False

    def draw_character(self, width=0.5, fuse=True):
        raise NotImplementedError()


class Ending:

    def __init__(self, character):
        self.character = character

    def angle(self):
        return None

    def draw(self, pen):
        return


def mirror_character_x(cls, name, **cls_attributes):
    """Create a horizontally mirrored variant of a character class."""

    class MirroredX(cls):

        mirrored_x = True

        def draw_character(self, width=0.5, fuse=True):
            paper = cls.draw_character(self, width, fuse)
            paper.mirror_x(0)
            return paper

    MirroredX.__name__ = name
    for key, value in cls_attributes.items():
        setattr(MirroredX, key, value)
    return MirroredX


def mirror_character_y(cls, name, **cls_attributes):
    """Create a vertically mirrored variant of a character class."""

    class MirroredY(cls):

        mirrored_y = True

        def draw_character(self, width=0.5, fuse=True):
            paper = cls.draw_character(self, width, fuse)
            paper.mirror_y(MIDDLE)
            return paper

    MirroredY.__name__ = name
    for key, value in cls_attributes.items():
        setattr(MirroredY, key, value)
    return MirroredY


def mirror_character_xy(cls, name, **cls_attributes):
    """Create an XY-mirrored variant of a character class."""

    class MirroredXY(cls):

        mirrored_xy = True

        def draw_character(self, width=0.5, fuse=True):
            paper = cls.draw_character(self, width, fuse)
            paper.mirror_x(0)
            paper.mirror_y(MIDDLE)
            return paper

    MirroredXY.__name__ = name
    for key, value in cls_attributes.items():
        setattr(MirroredXY, key, value)
    return MirroredXY
