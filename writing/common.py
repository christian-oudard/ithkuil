import math
from canoepaddle import flip_angle_x

# Constants.
WIDTH = 1

OVER = 10
TOP = 8
MIDDLE = 4
BOTTOM = 0
UNDER = -2

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
        pen.paper.center_on_x(0)


class Ending:
    def __init__(self, character):
        self.character = character

    def angle(self):
        return None

    def draw(self, pen):
        return
