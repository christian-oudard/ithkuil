#TODO: remove smidgen, canoepaddle should not crash on these situations.
from .common import (
    Ending,
    slant45,
    slant60,
    WIDTH,
    HOOK_BASE_WIDTH,
    BOTTOM,
    UNDER,
)

# A small amount added to some offsets to prevent illegal line crossings due to
# round off error.
smidgen = 0.001


class BottomEnding(Ending):

    def __str__(self):
        return 'bottom_ending.{}'.format(self.__class__.__name__)

    def predict_slant_width(self, pen, end_angle):
        old_position = pen.position
        pen.line_to_y(BOTTOM, end_angle=end_angle)
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
        pen.line_to_y(BOTTOM, end_angle=self.angle())


class Long(Normal):
    # Consonant Prefix L
    angle = Normal.angle

    def draw(self, pen):
        pen.line_to_y(UNDER, end_angle=self.angle())


class DiagonalDownRightOnRight(BottomEnding):
    # Consonant Prefix M
    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM + slant_width / slant45 / 2,
            end_angle=45,
        )

        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH / 2)

        pen.turn_to(-45)
        pen.line_to_y(BOTTOM, end_angle=0)


class DownOnRight(BottomEnding):
    # Consonant Prefix R
    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM - slant_width / slant45 / 2,
            end_angle=45,
        )

        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)

        pen.turn_to(-90)
        pen.line_forward(2, start_angle=45, end_angle=45)


class RightOnRight(BottomEnding):
    # Consonant Prefix L Cedilla
    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM - slant_width / slant45 / 2,
            end_angle=45,
        )

        pen.turn_to(45)
        pen.move_to_y(BOTTOM + WIDTH / 2)
        pen.turn_to(0)
        pen.line_forward(2, start_angle=45, end_angle=45)


class DiagonalDownLeftOnRight(BottomEnding):
    # Consonant Prefix N
    def draw(self, pen):
        pen.line_to_y(BOTTOM, end_angle=0)
        pen.turn_to(0)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
        pen.turn_to(-135)
        pen.line_forward(2, start_angle=0, end_angle=0)


class Bend(BottomEnding):
    # Consonant Prefix S
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM + WIDTH / 2, end_angle=0)
            pen.turn_to(0)
            pen.line_forward(2.5, end_angle=-45)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM, end_angle=0)
            pen.turn_to(-90)
            pen.line_forward(2, end_angle=45)


class Fold(BottomEnding):
    # Consonant Prefix S hacek
    # Consonant Prefix Z hacek
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(
                BOTTOM + WIDTH / 2,
                end_angle=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(WIDTH * slant45)
            pen.turn_to(0)
            pen.line_forward(2, start_angle=-45, end_angle=-45)
        elif self.character.bottom_slanted():
            slant_width = self.predict_slant_width(pen, 45)
            pen.line_to_y(
                BOTTOM + slant_width / slant45 / 2,
                end_angle=45,
            )
            pen.turn_to(-135)
            pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
            pen.turn_to(180)
            pen.line_forward(2, start_angle=45, end_angle=45)


class Barb(BottomEnding):
    # Consonant Prefix N hacek
    angle = Normal.angle

    def draw(self, pen):
        pen.line_to_y(BOTTOM + WIDTH / 4, end_angle=self.angle())

        old_mode = pen.mode
        new_mode = old_mode.copy()
        new_mode.width = WIDTH / 2
        pen.set_mode(new_mode)

        if self.character.bottom_straight():
            pen.turn_to(45)
            pen.line_forward(2 * WIDTH)
            seg = pen.paper.elements[-1].segments[-1]
            seg.b_left = seg.a_left
        elif self.character.bottom_slanted():
            pen.turn_to(180)
            pen.line_forward(2 * WIDTH)
            seg = pen.paper.elements[-1].segments[-1]
            seg.b_right = seg.a_right

        pen.set_mode(old_mode)


class DiagonalUpRight(BottomEnding):
    # Consonant Prefix Z
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM + WIDTH / 2, end_angle=-45)
            pen.turn_to(-45)
            pen.move_forward(pen.last_slant_width() / 2 + WIDTH / 2)
            pen.turn_to(45)
            pen.line_forward(2, start_angle=-45, end_angle=0)
        elif self.character.bottom_slanted():
            slant_width = self.predict_slant_width(pen, 90)
            pen.line_to_y(
                BOTTOM - slant_width / 2,
                end_angle=90
            )
            pen.turn_to(90)
            pen.move_to_y(WIDTH * slant60 / 2)
            pen.turn_to(30)
            pen.line_forward(2, start_angle=90, end_angle=90)


class Acute(BottomEnding):
    # Consonant Prefix R Hacek
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM + WIDTH / slant60)
            pen.turn_to(30)
            pen.line_forward(2.5, end_angle=90)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM + WIDTH / 2)
            pen.turn_to(180)
            pen.line_forward(3, end_angle=-45)


class RightOnBottom(BottomEnding):
    # Consonant Prefix C Cedilla
    def angle(self):
        return 45

    def offset_y(self, pen):
        return +WIDTH + smidgen

    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM + slant_width / slant45 / 2,
            end_angle=45,
        )
        pen.turn_to(-135)
        pen.move_to_y(BOTTOM - WIDTH / 2)
        pen.turn_to(0)
        pen.line_forward(3, start_angle=45, end_angle=45)


class StraightOnLeft(BottomEnding):
    # Consonant Prefix C
    # Consonant Prefix Z Dot
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM - WIDTH / 2, end_angle=-45)
            pen.turn_to(135)
            pen.move_forward(WIDTH * slant45)
            pen.turn_to(-90)
            pen.line_forward(2, start_angle=-45, end_angle=-45)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM, end_angle=0)
            original_heading = pen.heading
            pen.turn_to(180)
            pen.move_forward(pen.last_slant_width())
            pen.turn_to(original_heading)
            pen.line_forward(2, start_angle=0, end_angle=0)


class BreakRightBendLeft(BottomEnding):
    # Consonant Prefix C Hacek
    # Consonant Prefix J
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM, end_angle=0)
            pen.turn_to(180)
            pen.move_forward(WIDTH / 2 + WIDTH * slant45 / 2)
            pen.turn_to(-45)
            pen.line_forward(2, start_angle=0, end_angle=0)
        elif self.character.bottom_slanted():
            slant_width = self.predict_slant_width(pen, 90)
            pen.line_to_y(
                BOTTOM + slant_width / 2,
                end_angle=90
            )
            pen.turn_to(-90)
            pen.move_to_y(BOTTOM - WIDTH * slant60 / 2)
            pen.turn_to(30)
            pen.line_forward(2, start_angle=90, end_angle=90)


class BreakRightBendRight(BottomEnding):
    # Consonant Prefix Z Hacek
    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM - WIDTH / 2, end_angle=-45)
            pen.turn_to(135)
            pen.move_forward(WIDTH * slant45)
            pen.turn_to(180)
            pen.line_forward(2, start_angle=-45, end_angle=-45)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM, end_angle=0)
            pen.turn_to(180)
            pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
            pen.turn_to(-135)
            pen.line_forward(2, start_angle=0, end_angle=0)


class CurveUpOnRight(BottomEnding):
    # Consonant Prefix K
    # Consonant Prefix G
    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM + slant_width / slant45 / 2,
            end_angle=45,
        )
        pen.turn_to(45)

        old_mode = pen.mode
        pen.set_mode(old_mode.outliner_mode())

        # Mark bottom of hook base.
        pen.turn_to(45)
        pen.move_forward(slant_width / 2)
        a = pen.position

        # Mark tip of hook.
        pen.turn_to(-90)
        pen.move_to_y(BOTTOM)
        pen.move_relative((2.5 * WIDTH, 0))
        tip = pen.position

        # Mark the correct heading to leave the hook at.
        pen.move_to(a)
        pen.turn_to(-60)
        pen.arc_to(tip)
        heading = pen.heading
        pen.undo()

        # Draw the hook.
        pen.move_to(a)
        pen.turn_to(45)
        pen.line_forward(WIDTH)
        pen.turn_to(-60)
        pen.arc_to(tip)
        pen.turn_to(heading + 180)
        pen.arc_to(a)

        pen.set_mode(old_mode)


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
            pen.line_to_y(bottom_ending.offset_y(pen), end_angle=bottom_ending.angle())
            bottom_ending.draw(pen)


bottom_endings = [
    Normal,
    #BottomAll, #DEBUG
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
    CurveUpOnRight,
]
