# TODO: Test suite to try drawing all possible characters.
# TODO: Typesetting.
# TODO: Kerning by taking the convex hull of the written letter then finding a
# spot where they're the right distance apart. This function should be strictly
# increasing because of the convex hull thing.
# TODO: Make really nice pronunciation charts for the writing system and give
# them to John Quijada.

import math
from canoepaddle.pen import flip_angle_x

# Constants.
WIDTH = 1.0

HALF_HEIGHT = 3.6
BOTTOM = 0
MIDDLE = BOTTOM + HALF_HEIGHT
TOP = MIDDLE + HALF_HEIGHT
UNDER = BOTTOM - 2.0
OVER = TOP + 2.0

CHAR_MARGIN = 1
CHAR_HEIGHT = OVER - UNDER + WIDTH
LINE_SPACING = 1.25
LINE_HEIGHT = CHAR_HEIGHT * LINE_SPACING
PAGE_MARGIN = 5


slant45 = 1 / math.sin(math.radians(45))
slant60 = 1 / math.sin(math.radians(60))
slant75 = 1 / math.sin(math.radians(75))
bevel_distance = WIDTH * math.tan(math.radians(22.5)) + 0.1


def flip_ending_horizontal(cls):
    # Replace the ending with one that is flipped in the x direction.
    class Flipped(cls):
        def angle(self):
            a = cls.angle(self)
            return flip_angle_x(a)

        def draw(self, pen):
            pen.flip_x()
            cls.draw(self, pen)
            pen.flip_x()
    return Flipped


def flip_consonant_horizontal(cls):
    class Flipped(cls):
        def draw_character(self, pen):
            pen.flip_x()
            cls.draw_character(self, pen)
            pen.flip_x()
    return Flipped


class Character:
    def draw_character(self, pen):
        self.draw(pen)


class Ending:
    def __init__(self, character):
        self.character = character

    def angle(self):
        return None

    def draw(self, pen):
        return
