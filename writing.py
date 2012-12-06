import math
import traceback
from canoepaddle import Pen, flip_angle_x

# Constants.
WIDTH = 1
HALFWIDTH = WIDTH / 2

LEFT = 0.0 # DEP
CENTER = 3.0
RIGHT = 6.0 # DEP

TOP_BAR_LENGTH = 5.5

OVER = 10
TOP = 8
MIDDLE = 4
BOTTOM = 0
UNDER = -2


sqrt2 = math.sqrt(2)
sqrt3 = math.sqrt(3)


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
        def draw(self, pen):
            pen.flip_x()
            cls.draw(self, pen)
            pen.flip_x()
    return Flipped


class ConsonantCharacter:
    bottom_type = NotImplemented # 'straight' or 'slanted'
    bottom_orientation = NotImplemented # 'left' or 'right'
    side_flipped = NotImplemented # True for 45 angle, False for -45 angle.

    def __init__(self, side_ending_class, bottom_ending_class):
        self.side_ending = side_ending_class(self)
        if self.bottom_slanted() and self.bottom_orientation == 'left':
            bottom_ending_class = flip_ending_horizontal(bottom_ending_class)
        self.bottom_ending = bottom_ending_class(self)

    def bottom_straight(self):
        return self.bottom_type == 'straight'

    def bottom_slanted(self):
        return self.bottom_type == 'slanted'

    def draw(self, pen):
        pass

class ConsP(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = True
    def draw(self, pen):
        pen.move_to((0, TOP - HALFWIDTH))
        self.side_ending.draw(pen)
        pen.turn_to(180)
        pen.stroke_forward(TOP_BAR_LENGTH, start_angle=self.side_ending.angle())
        pen.turn_to(-45)
        pen.stroke_to_y(MIDDLE + WIDTH)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )
        self.bottom_ending.draw(pen)


class ConsT(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.move_to((0, TOP - HALFWIDTH))
        self.side_ending.draw(pen)
        pen.turn_to(180)
        pen.stroke_forward(TOP_BAR_LENGTH, start_angle=self.side_ending.angle())
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )
        self.bottom_ending.draw(pen)


class ConsK(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'right'
    side_flipped = False
    def draw(self, pen):
        pen.move_to((0, TOP - HALFWIDTH))
        self.side_ending.draw(pen)
        pen.turn_to(180)
        pen.stroke_forward(TOP_BAR_LENGTH, start_angle=self.side_ending.angle())
        pen.turn_to(-65)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )
        self.bottom_ending.draw(pen)


class ConsQ(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.move_to((0, TOP - HALFWIDTH))
        self.side_ending.draw(pen)
        pen.turn_to(180)
        pen.stroke_forward(TOP_BAR_LENGTH, start_angle=self.side_ending.angle())
        pen.turn_to(-45)
        pen.stroke_to_y(MIDDLE, end_angle=0)
        pen.turn_to(180)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH / sqrt3)
        pen.turn_left(60)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            start_angle=0,
            end_angle=self.bottom_ending.angle(),
        )
        self.bottom_ending.draw(pen)


class ConsC(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.move_to((0, TOP - HALFWIDTH))
        self.side_ending.draw(pen)
        pen.turn_to(180)
        pen.stroke_forward(TOP_BAR_LENGTH, start_angle=self.side_ending.angle())
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE + HALFWIDTH)
        pen.turn_to(0)
        pen.stroke_forward(TOP_BAR_LENGTH / 2)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )
        self.bottom_ending.draw(pen)


class ConsCHacek(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.move_to((0, TOP - HALFWIDTH))
        self.side_ending.draw(pen)
        pen.turn_to(180)
        pen.stroke_forward(TOP_BAR_LENGTH, start_angle=self.side_ending.angle())
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE + HALFWIDTH)
        pen.turn_to(0)
        pen.stroke_forward(TOP_BAR_LENGTH * (3/4))
        pen.turn_to(-135)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )
        self.bottom_ending.draw(pen)


class ConsL(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'right'
    side_flipped = False
    def draw(self, pen):
        pen.move_to((0, TOP - HALFWIDTH))
        self.side_ending.draw(pen)
        pen.turn_to(180)
        pen.stroke_forward(TOP_BAR_LENGTH, start_angle=self.side_ending.angle())
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE)
        pen.turn_to(-45)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )
        self.bottom_ending.draw(pen)

class ConsL(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'right'
    side_flipped = False
    def draw(self, pen):
        pen.move_to((0, TOP - HALFWIDTH))
        self.side_ending.draw(pen)
        pen.turn_to(180)
        pen.stroke_forward(TOP_BAR_LENGTH, start_angle=self.side_ending.angle())
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE)
        pen.turn_to(-45)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )
        self.bottom_ending.draw(pen)


ConsB = flip_consonant_horizontal(ConsP)
ConsD = flip_consonant_horizontal(ConsT)
ConsG = flip_consonant_horizontal(ConsK)
ConsZDot = flip_consonant_horizontal(ConsC)
ConsJ = flip_consonant_horizontal(ConsCHacek)


class Ending:
    def __init__(self, character):
        self.character = character

    def angle(self):
        return None

    def draw(self, pen):
        return


class BottomEnding(Ending):
    def offset_y(self, pen):
        return 0


class BottomNormal(BottomEnding):
    def angle(self):
        if self.character.bottom_straight():
            return 45
        elif self.character.bottom_slanted():
            return 0


class BottomLong(BottomEnding):
    # Consonant Prefix L
    def offset_y(self, pen):
        return UNDER - BOTTOM
    angle = BottomNormal.angle


class BottomDiagonalDownRightOnRight(BottomEnding):
    # Consonant Prefix M
    def angle(self):
        return 45

    def offset_y(self, pen):
        return +WIDTH

    def draw(self, pen):
        pen.stroke_to_y(
            BOTTOM + pen.last_slant_width() / sqrt2 / 2,
            end_angle=45,
        )
        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH / 2)
        pen.turn_to(-45)
        pen.stroke_to_y(BOTTOM, end_angle=0)


class BottomDownOnRight(BottomEnding):
    # Consonant Prefix R
    def angle(self):
        return 45

    def offset_y(self, pen):
        return +HALFWIDTH

    def draw(self, pen):
        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + sqrt2 * WIDTH / 2)
        pen.turn_to(-90)
        pen.stroke_forward(2, start_angle=45, end_angle=45)


class BottomRightOnRight(BottomEnding):
    # Consonant Prefix L cedilla
    def angle(self):
        return 45

    def offset_y(self, pen):
        return +WIDTH

    def draw(self, pen):
        pen.stroke_to_y(
            BOTTOM - pen.last_slant_width() / sqrt2 / 2,
            end_angle=45,
        )
        pen.turn_to(45)
        pen.move_to_y(BOTTOM + HALFWIDTH)
        pen.turn_to(0)
        pen.stroke_forward(2, start_angle=45, end_angle=45)


class BottomDiagonalDownLeftOnRight(BottomEnding):
    # Consonant Prefix N
    def angle(self):
        return 0

    def offset_y(self, pen):
        return 0

    def draw(self, pen):
        pen.turn_to(0)
        pen.move_forward(pen.last_slant_width() / 2 + sqrt2 * WIDTH / 2)
        pen.turn_to(-135)
        pen.stroke_forward(2, start_angle=0, end_angle=0)


class BottomBend(BottomEnding):
    # Consonant Prefix S
    def offset_y(self, pen):
        return +HALFWIDTH

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.turn_to(0)
            pen.stroke_forward(2, end_angle=-45)
        elif self.character.bottom_slanted():
            pen.turn_to(-90)
            pen.stroke_forward(2, end_angle=45)


class BottomFold(BottomEnding):
    # Consonant Prefix S hacek
    # Consonant Prefix Z hacek
    def angle(self):
        if self.character.bottom_straight():
            return -45
        elif self.character.bottom_slanted():
            return 45

    def offset_y(self, pen):
        return +WIDTH

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.stroke_to_y(
                BOTTOM + pen.last_slant_width() / sqrt2 / 2,
                end_angle=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(pen.last_slant_width() / 2 + sqrt2 * WIDTH / 2)
            pen.turn_to(0)
            pen.stroke_forward(2, start_angle=-45, end_angle=-45)
        elif self.character.bottom_slanted():
            pen.stroke_to_y(
                BOTTOM + pen.last_slant_width() / sqrt2 / 2,
                end_angle=45,
            )
            pen.turn_to(-135)
            pen.move_forward(pen.last_slant_width() / 2 + sqrt2 * WIDTH / 2)
            pen.turn_to(180)
            pen.stroke_forward(2, start_angle=45, end_angle=45)


class BottomBarb(BottomEnding):
    # Consonant Prefix N hacek
    angle = BottomNormal.angle

    def offset_y(self, pen):
        return +WIDTH / 4

    def draw(self, pen):
        pen.set_width(WIDTH / 2)
        if self.character.bottom_straight():
            pen.turn_to(45)
            pen.stroke_forward(WIDTH * 1.5, end_angle=20)
        elif self.character.bottom_slanted():
            pen.turn_to(180)
            pen.stroke_forward(WIDTH * 1.5, end_angle=25)


class SideEnding(Ending):
    def offset_x(self):
        return 0


class SideNormal(SideEnding):
    def angle(self):
        if self.character.side_flipped:
            return -45
        else:
            return 45


def draw_template_path(x, y):
    pen = Pen(offset=(x, y))
    pen.turn_to(0)
    pen.move_to((1, UNDER))
    pen.stroke_forward(3)
    pen.move_to((0, BOTTOM))
    pen.stroke_forward(8)
    pen.move_to((0, MIDDLE))
    pen.stroke_forward(8)
    pen.move_to((0, TOP))
    pen.stroke_forward(8)
    pen.move_to((1, OVER))
    pen.stroke_forward(3)
    return pen.paper.to_svg_path(precision=5)

def draw_letters(letters):
    width = 14
    max_width = 180
    height = 14
    x, y = x_start, y_start = 10, -14
    character_path = []
    template_path = []

    for letter in letters:
        pen = Pen(offset=(x, y))
        pen.set_width(WIDTH)

        try:
            letter.draw(pen)
            character_path.append(pen.paper.to_svg_path_thick(precision=5))
        except Exception:
            traceback.print_exc()
        template_path.append(draw_template_path(x, y))

        x += width
        if x >= max_width:
            x = x_start
            y -= height

    return ''.join(character_path), ''.join(template_path)

# Lists of character parts.
consonants = [
    ConsP,
    ConsT,
    ConsK,
    ConsQ,
    ConsC,
    ConsCHacek,
    ConsB,
    ConsD,
    ConsG,
    ConsZDot,
    ConsJ,
]
side_endings = [
    SideNormal,
]
bottom_endings = [
    BottomNormal,
    BottomLong,
    BottomDiagonalDownRightOnRight,
    BottomDownOnRight,
    BottomRightOnRight,
    BottomDiagonalDownLeftOnRight,
    BottomBend,
    BottomFold,
    BottomBarb,
]

if __name__ == '__main__':
    letters = []
    seen = set()
    def add_letter(c, s, b):
        if (c, s, b) in seen:
            return
        else:
            seen.add((c, s, b))
        letters.append(c(s, b))

    for consonant_class in consonants:
        for side_ending_class in side_endings:
            add_letter(consonant_class, side_ending_class, BottomNormal)
        for bottom_ending_class in bottom_endings:
            add_letter(consonant_class, SideNormal, bottom_ending_class)

    character_path, template_path = draw_letters(letters)

    from string import Template
    with open('template.svg') as f:
        t = Template(f.read())
    output = t.substitute(
        character_path=character_path,
        template_path=template_path,
    )
    print(output)
