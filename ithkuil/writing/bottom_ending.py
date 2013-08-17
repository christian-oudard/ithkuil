from .common import (
    Ending,
    slant45,
    slant60,
    WIDTH,
    BOTTOM,
    UNDER,
)
from .util import hook


class BottomEnding(Ending):

    def __str__(self):
        return 'bottom_ending.{}'.format(self.__class__.__name__)

    def predict_slant_width(self, pen, end_angle):
        old_position = pen.position
        pen.line_to_y(BOTTOM, end_slant=end_angle)
        slant_width = pen.last_slant_width()
        pen.undo()
        pen.move_to(old_position)
        return slant_width


class Normal(BottomEnding):

    def angle(self):
        if self.character.bottom_straight():
            if not self.character.bottom_flipped:
                return 45
            else:
                return -45
        elif self.character.bottom_slanted():
            return 0

    def draw(self, pen):
        pen.line_to_y(BOTTOM, end_slant=self.angle())


class Long(Normal):
    # Consonant Prefix L
    angle = Normal.angle

    def draw(self, pen):
        pen.line_to_y(UNDER, end_slant=self.angle())


class DiagonalDownRightOnRight(BottomEnding):
    # Consonant Prefix M
    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM + slant_width / slant45 / 2,
            end_slant=45,
        )

        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH / 2)

        pen.turn_to(-45)
        pen.line_to_y(BOTTOM, end_slant=0)


class DownOnRight(BottomEnding):
    # Consonant Prefix R
    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM - slant_width / slant45 / 2,
            end_slant=45,
        )

        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)

        pen.turn_to(-90)
        pen.line_forward(2, start_slant=45, end_slant=45)


class RightOnRight(BottomEnding):
    # Consonant Prefix L Cedilla
    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM - slant_width / slant45 / 2,
            end_slant=45,
        )

        pen.turn_to(45)
        pen.move_to_y(BOTTOM + WIDTH / 2)
        pen.turn_to(0)
        pen.line_forward(2, start_slant=45, end_slant=45)


class DiagonalDownLeftOnRight(BottomEnding):
    # Consonant Prefix N
    def draw(self, pen):
        pen.line_to_y(BOTTOM, end_slant=0)
        pen.turn_to(0)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
        pen.turn_to(-135)
        pen.line_forward(2, start_slant=0, end_slant=0)


class Bend(BottomEnding):
    # Consonant Prefix S
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM + WIDTH / 2, end_slant=0)
            pen.turn_to(0)
            pen.line_forward(2.5, end_slant=-45)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(-90)
            pen.line_forward(2, end_slant=45)


class Fold(BottomEnding):
    # Consonant Prefix S hacek
    # Consonant Prefix Z hacek
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(
                BOTTOM + WIDTH / 2,
                end_slant=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(WIDTH * slant45)
            pen.turn_to(0)
            pen.line_forward(2, start_slant=-45, end_slant=-45)
        elif self.character.bottom_slanted():
            slant_width = self.predict_slant_width(pen, 45)
            pen.line_to_y(
                BOTTOM + slant_width / slant45 / 2,
                end_slant=45,
            )
            pen.turn_to(-135)
            pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
            pen.turn_to(180)
            pen.line_forward(2, start_slant=45, end_slant=45)


class Barb(BottomEnding):
    # Consonant Prefix N hacek
    angle = Normal.angle

    def draw(self, pen):
        pen.line_to_y(BOTTOM + WIDTH / 4, end_slant=self.angle())

        old_mode = pen.mode
        new_mode = old_mode.copy()
        new_mode.width = WIDTH / 2
        pen.set_mode(new_mode)

        if self.character.bottom_straight():
            pen.turn_to(45)
            pen.line_forward(2 * WIDTH)
            seg = pen.last_segment()
            seg.b_left = seg.a_left
        elif self.character.bottom_slanted():
            pen.turn_to(180)
            pen.line_forward(2 * WIDTH)
            seg = pen.last_segment()
            seg.b_right = seg.a_right

        pen.set_mode(old_mode)


class DiagonalUpRight(BottomEnding):
    # Consonant Prefix Z
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM + WIDTH / 2, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(pen.last_slant_width() / 2 + WIDTH / 2)
            pen.turn_to(45)
            pen.line_forward(2, start_slant=-45, end_slant=0)
        elif self.character.bottom_slanted():
            slant_width = self.predict_slant_width(pen, 90)
            pen.line_to_y(
                BOTTOM - slant_width / 2,
                end_slant=90
            )
            pen.turn_to(90)
            pen.move_to_y(WIDTH * slant60 / 2)
            pen.turn_to(30)
            pen.line_forward(2, start_slant=90, end_slant=90)


class Acute(BottomEnding):
    # Consonant Prefix R Hacek
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM + WIDTH / slant60)
            pen.turn_to(30)
            pen.line_forward(2.5, end_slant=90)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM + WIDTH / 2)
            pen.turn_to(180)
            pen.line_forward(3, end_slant=-45)


class RightOnBottom(BottomEnding):
    # Consonant Prefix C Cedilla
    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM + slant_width / slant45 / 2,
            end_slant=45,
        )
        pen.turn_to(-135)
        pen.move_to_y(BOTTOM - WIDTH / 2)
        pen.turn_to(0)
        pen.line_forward(3, start_slant=45, end_slant=45)


class StraightOnLeft(BottomEnding):
    # Consonant Prefix C
    # Consonant Prefix Z Dot
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM - WIDTH / 2, end_slant=-45)
            pen.turn_to(135)
            pen.move_forward(WIDTH * slant45)
            pen.turn_to(-90)
            pen.line_forward(2, start_slant=-45, end_slant=-45)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM, end_slant=0)
            original_heading = pen.heading
            pen.turn_to(180)
            pen.move_forward(pen.last_slant_width())
            pen.turn_to(original_heading)
            pen.line_forward(2, start_slant=0, end_slant=0)


class BreakRightBendLeft(BottomEnding):
    # Consonant Prefix C Hacek
    # Consonant Prefix J
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(180)
            pen.move_forward(WIDTH / 2 + WIDTH * slant45 / 2)
            pen.turn_to(-45)
            pen.line_forward(2, start_slant=0, end_slant=0)
        elif self.character.bottom_slanted():
            slant_width = self.predict_slant_width(pen, 90)
            pen.line_to_y(
                BOTTOM + slant_width / 2,
                end_slant=90
            )
            pen.turn_to(-90)
            pen.move_to_y(BOTTOM - WIDTH * slant60 / 2)
            pen.turn_to(30)
            pen.line_forward(2, start_slant=90, end_slant=90)


class BreakRightBendRight(BottomEnding):
    # Consonant Prefix Z Hacek
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM - WIDTH / 2, end_slant=-45)
            pen.turn_to(135)
            pen.move_forward(WIDTH * slant45)
            pen.turn_to(180)
            pen.line_forward(2, start_slant=-45, end_slant=-45)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(180)
            pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
            pen.turn_to(-135)
            pen.line_forward(2, start_slant=0, end_slant=0)


class HookLeftOnRight(BottomEnding):
    # Consonant Prefix K
    # Consonant Prefix G
    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM + slant_width / slant45 / 2,
            end_slant=45,
        )
        pen.turn_to(45)
        pen.move_forward(slant_width / 2)
        hook(pen, -135, 90, 2.5)


class HookRightOnRight(BottomEnding):
    # Consonant Prefix P
    # Consonant Prefix B
    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM + slant_width / slant45 / 2,
            end_slant=45,
        )
        pen.turn_to(45)
        pen.move_forward(slant_width / 2)
        hook(pen, -45, -90, 2.5)


class FoldHookRight(BottomEnding):
    # Consonant Prefix Q
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(
                BOTTOM + WIDTH / 2,
                end_slant=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(WIDTH * slant45 / 2)
            hook(pen, 90, -75, 2.25)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(pen.last_slant_width() / 2)
            hook(pen, 75, -75, 2.0)


bottom_endings = [
    Normal,
    Long,
    DiagonalDownRightOnRight,
    DownOnRight,
    RightOnRight,
    DiagonalDownLeftOnRight,
    Bend,
    Fold,
    Barb,
    DiagonalUpRight,
    Acute,
    RightOnBottom,
    StraightOnLeft,
    BreakRightBendLeft,
    BreakRightBendRight,
    HookLeftOnRight,
    HookRightOnRight,
    FoldHookRight,
]


class BottomAll(BottomEnding):
    """
    A debug ending showing every one of the endings superimposed.
    """
    def angle(self):
        return None

    def offset_y(self, pen):
        return 0

    def draw(self, pen):
        start_position = pen.position
        start_heading = pen.heading
        for bottom_ending_class in bottom_endings:
            if bottom_ending_class == BottomAll:
                continue
            bottom_ending = bottom_ending_class(self.character)
            pen.move_to(start_position)
            pen.turn_to(start_heading)
            pen.move_to_y(3)
            pen.line_to_y(bottom_ending.offset_y(pen), end_slant=bottom_ending.angle())
            bottom_ending.draw(pen)
