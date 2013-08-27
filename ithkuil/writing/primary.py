from canoepaddle import Pen, Paper
from .common import (
    Character,
    OVER,
    MIDDLE,
    UNDER,
    mirror_character_x,
    mirror_character_y,
    mirror_character_xy,
)


class PrimaryCharacter(Character):

    case = NotImplemented
    top_straight = NotImplemented
    top_flipped = False
    bottom_straight = NotImplemented
    bottom_flipped = False

    def __init__(self, top_ending_class, bottom_ending_class, mode):
        super().__init__(mode)
        self.top_ending = top_ending_class(
            self,
            mode,
            self.top_straight,
            self.top_flipped,
        )
        self.bottom_ending = bottom_ending_class(
            self,
            mode,
            self.bottom_straight,
            self.bottom_flipped,
        )

    def __str__(self):
        return 'primary.{}({}, {})'.format(
            self.__class__.__name__,
            self.top_ending,
            self.bottom_ending,
        )

    def draw_character(self, fuse=True):
        paper = Paper()

        # While drawing the body of the primary character, two copies of the
        # pen are created, one ready to draw each of the endings.
        pen = Pen()
        pen.set_mode(self.mode)

        top_pen, bottom_pen = self.draw(pen)
        top_start_position = top_pen.position
        bottom_start_position = bottom_pen.position

        # Draw the endings.
        # If the ending orientations are to the left, then we have to flip them
        # horizontally.
        # The top ending is drawn as if it were a bottom ending, so
        # mirror it to the top.
        if self.top_flipped:
            top_pen.turn_to(top_pen.heading.flipped_x())
        top_pen.move_to(top_pen.position.flipped_y(MIDDLE))
        top_pen.turn_to(top_pen.heading.flipped_y())
        self.top_ending.draw(top_pen)
        top_pen.paper.mirror_y(MIDDLE)
        if self.top_flipped:
            top_pen.paper.mirror_x(top_start_position.x)

        if self.bottom_flipped:
            bottom_pen.turn_to(bottom_pen.heading.flipped_x())
        self.bottom_ending.draw(bottom_pen)
        if self.bottom_flipped:
            bottom_pen.paper.mirror_x(bottom_start_position.x)

        paper.merge(pen.paper)
        paper.merge(top_pen.paper)
        paper.merge(bottom_pen.paper)
        if fuse:
            paper.join_paths()
            paper.fuse_paths()

        # Override the bounds so that the letter reaches the full line height.
        bounds = paper.bounds()
        bounds.bottom = UNDER
        bounds.top = OVER
        paper.override_bounds(bounds)

        # We need to center on x=0 here because otherwise flipped
        # consonants wouldn't flip at the right x value.
        paper.center_on_x(0)

        return paper


class Oblique(PrimaryCharacter):

    case = 'OBL'
    top_straight = False
    top_flipped = False
    bottom_straight = False
    bottom_flipped = False

    def draw(self, pen):
        pen.move_to((0, MIDDLE + self.width))
        pen.turn_to(-135)
        top_pen = pen.copy()
        top_pen.turn_left(180)

        pen.line_to_y(MIDDLE)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE - self.width)
        bottom_pen = pen.copy()

        return top_pen, bottom_pen


class Inducive(PrimaryCharacter):

    case = 'IND'
    top_straight = False
    top_flipped = False
    bottom_straight = True
    bottom_flipped = False

    def draw(self, pen):
        pen.move_to((0, MIDDLE + 1.5 * self.width))
        pen.turn_to(-135)
        top_pen = pen.copy()
        top_pen.turn_left(180)

        pen.line_to_y(MIDDLE + self.width / 2)
        pen.turn_to(0)
        pen.line_forward(3.0)
        pen.turn_to(-90)
        pen.line_forward(self.width)
        bottom_pen = pen.copy()

        return top_pen, bottom_pen


class Absolutive(PrimaryCharacter):

    case = 'ABS'
    top_straight = False
    top_flipped = False
    bottom_straight = False
    bottom_flipped = True

    def draw(self, pen):
        pen.move_to((0, MIDDLE + 1.5 * self.width))
        pen.turn_to(-135)
        top_pen = pen.copy()
        top_pen.turn_left(180)

        pen.line_to_y(MIDDLE + self.width / 2)
        pen.turn_to(0)
        pen.line_forward(3.0)
        pen.turn_to(-135)
        pen.line_to_y(MIDDLE - self.width / 2)
        bottom_pen = pen.copy()

        return top_pen, bottom_pen


# Mirrored characters.

mx = mirror_character_x
my = mirror_character_y
mxy = mirror_character_xy

Purposive = mx(Oblique, 'Purposive', case='PUR')
Considerative = mx(Inducive, 'Considerative', case='CSD')
Essive = mx(Absolutive, 'Essive', case='ESS')
# ...
Aversive = my(Inducive, 'Aversive', case='AVR')
# ...
Comparative1B = mxy(Inducive, 'Comparative1B', case='CMP1B')


# Character list.

primary_characters = [
    Oblique,
    Inducive,
    Absolutive,
    # ...
    Purposive,
    Considerative,
    Essive,
    # ...
    Aversive,
    # ...
    Comparative1B,
]
