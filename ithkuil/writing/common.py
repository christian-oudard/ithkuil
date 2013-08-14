# TODO: bottom ending DiagonalDownRightOnRight is messing up the
# outline joins on top when flipped, it has a little flat spot.
# TODO: Test suite to try drawing all possible characters.
# TODO: Kerning by taking the convex hull of the written letter then finding a
# spot where they're the right distance apart. This function should be strictly
# increasing because of the convex hull thing.
# TODO: Make really nice pronunciation charts for the writing system and give
# them to John Quijada.
# TODO: Make a program to write your name in phonetic ithkuil.

import math

# Constants.
slant45 = 1 / math.sin(math.radians(45))
slant60 = 1 / math.sin(math.radians(60))
slant75 = 1 / math.sin(math.radians(75))

WIDTH = 1.0
#WIDTH = 0.5
HOOK_BASE_WIDTH = 1.25 * WIDTH

HALF_HEIGHT = 3.6
BOTTOM = 0
MIDDLE = BOTTOM + HALF_HEIGHT
TOP = MIDDLE + HALF_HEIGHT
UNDER = BOTTOM - 2.0
OVER = TOP + 2.0

CHAR_MARGIN = 1.25
CHAR_HEIGHT = OVER - UNDER + WIDTH
LINE_SPACING = 1.25
LINE_HEIGHT = CHAR_HEIGHT * LINE_SPACING
PAGE_MARGIN = 5

bevel_distance = WIDTH * math.tan(math.radians(22.5)) + 0.1


def flip_ending_horizontal(cls):
    # Replace the ending with one that is flipped in the x direction.
    class Flipped(cls):
        def angle(self):
            return cls.angle(self)  # STUB
            #a = cls.angle(self)
            #return flip_angle_x(a)

        def draw(self, pen):
            cls.draw(self, pen)
    return Flipped


def flip_consonant_horizontal(cls):

    class Flipped(cls):

        def draw_character(self, mode):
            paper = cls.draw_character(self, mode)
            paper.mirror_x(0)
            return paper

        def __str__(self):
            return 'Flipped(consonant.{})({}, {})'.format(
                cls.__name__,
                self.side_ending,
                self.bottom_ending,
            )

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
