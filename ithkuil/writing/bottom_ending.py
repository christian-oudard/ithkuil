from .common import (
    Ending,
    slant45,
    slant60,
    BOTTOM,
    UNDER,
)
from .util import hook


WIDTH = 1.0 #TEMP


class BottomEnding(Ending):

    pronunciation = NotImplemented

    def __str__(self):
        return 'bottom_ending.{}'.format(self.__class__.__name__)

    def predict_slant_width(self, pen, end_angle):
        temp_pen = pen.copy()
        temp_pen.line_to_y(BOTTOM, end_slant=end_angle)
        return temp_pen.last_slant_width()


class Normal(BottomEnding):

    pronunciation = ''

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

    pronunciation = 'l-'

    angle = Normal.angle

    def draw(self, pen):
        pen.line_to_y(UNDER, end_slant=self.angle())


class DiagonalDownRightOnRight(BottomEnding):

    pronunciation = 'm-'

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

    pronunciation = 'r-'

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

    pronunciation = 'l,-'

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

    pronunciation = 'n-'

    def draw(self, pen):
        pen.line_to_y(BOTTOM, end_slant=0)
        pen.turn_to(0)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
        pen.turn_to(-135)
        pen.line_forward(2, start_slant=0, end_slant=0)


class Bend(BottomEnding):

    pronunciation = ('s-', 'z-')

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

    pronunciation = ('s^-', 'z^-')

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

    pronunciation = 'n^-'

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

    pronunciation = 'z-'

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

    pronunciation = 'r^-'

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(BOTTOM + WIDTH / slant60)
            pen.turn_to(22.5)
            pen.line_forward(2.0, end_slant=90)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM + WIDTH / 2)
            pen.turn_to(180)
            pen.line_forward(2.5, end_slant=-45)


class RightOnBottom(BottomEnding):

    pronunciation = 'c,-'

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


class Break(BottomEnding):

    pronunciation = ('c-', 'dz-')

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


class BreakTurnLeft(BottomEnding):

    pronunciation = ('c^-', 'j-')

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


class BreakTurnRight(BottomEnding):

    pronunciation = 'z^-'

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

    pronunciation = ('k-', 'g-')

    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM + slant_width / slant45 / 2,
            end_slant=45,
        )
        pen.turn_to(45)
        pen.move_forward(slant_width / 2)
        hook(pen, -90, 30, 2.5, adjust_outside=15)


class HookRightOnRight(BottomEnding):

    pronunciation = ('p-', 'b-')

    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM + slant_width / slant45 / 2,
            end_slant=45,
        )
        pen.turn_to(45)
        pen.move_forward(slant_width / 2)
        hook(pen, -90, -30, 2.5, adjust_outside=15)


class FoldHookRight(BottomEnding):

    pronunciation = 'q-'

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(
                BOTTOM + WIDTH / 2,
                end_slant=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(WIDTH * slant45 / 2)
            hook(pen, 90, -30, 2.5, adjust_outside=15)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(pen.last_slant_width() / 2)
            hook(pen, 60, -30, 2.5, adjust_outside=15)


class FoldHookLeft(BottomEnding):

    pronunciation = ('t-', 'd-')

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(
                BOTTOM + WIDTH / 2,
                end_slant=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(WIDTH * slant45 / 2)
            hook(pen, 60, 30, 2.5, adjust_outside=15)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(pen.last_slant_width() / 2)
            hook(pen, 60, 30, 2.5, adjust_outside=15)


class TowardRightHookLeft(BottomEnding):

    pronunciation = 'x-'

    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM + slant_width / slant45 / 2,
            end_slant=45,
        )
        pen.turn_to(-135)
        pen.move_to_y(BOTTOM)
        hook(pen, 120, 30, 3.5, adjust_outside=5)


class TowardLeftHookLeft(BottomEnding):

    pronunciation = ('t,-', 'dh-')

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(
                BOTTOM + WIDTH / 2,
                end_slant=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(WIDTH * slant45 / 2)
            hook(pen, -120, 30, 2.5, adjust_outside=15)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(pen.last_slant_width() / 2)
            hook(pen, -120, 30, 2.5, adjust_outside=15)


class TowardLeftHookRight(BottomEnding):

    pronunciation = ('f,-', 'v-')

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.line_to_y(
                BOTTOM + WIDTH / 2,
                end_slant=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(WIDTH * slant45 / 2)
            hook(pen, -120, -45, 3.0, adjust_outside=5)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(pen.last_slant_width() / 2)
            hook(pen, -120, -45, 3.0, adjust_outside=5)


class TowardRightHookRight(BottomEnding):

    pronunciation = 'xh-'

    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM + slant_width / slant45 / 2,
            end_slant=45,
        )
        pen.turn_to(-135)
        pen.move_to_y(BOTTOM)
        hook(pen, 120, -30, 3.5, adjust_outside=15)


class BendBreak(BottomEnding):

    pronunciation = 'GEMINATE'

    def draw(self, pen):
        pass


class Scythe(BottomEnding):

    pronunciation = '-w'

    def draw(self, pen):
        pass


class AcuteFold(BottomEnding):

    pronunciation = '-y'

    def draw(self, pen):
        pass


class BendBarb(BottomEnding):

    pronunciation = '-l'

    def draw(self, pen):
        pass


class AcuteBarb(BottomEnding):

    pronunciation = '-r'

    def draw(self, pen):
        pass


class AcuteBreak(BottomEnding):

    pronunciation = '-r^'

    def draw(self, pen):
        pass


class AcuteLineHigh(BottomEnding):

    pronunciation = '-m'

    def draw(self, pen):
        pass


class AcuteLineLow(BottomEnding):

    pronunciation = '-n'

    def draw(self, pen):
        pass


class DoubleBend(BottomEnding):

    pronunciation = '-v'

    def draw(self, pen):
        pass


class BreakSlightRight(BottomEnding):

    pronunciation = '-n^'

    def draw(self, pen):
        pass


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
    Break,
    BreakTurnLeft,
    BreakTurnRight,
    HookLeftOnRight,
    HookRightOnRight,
    FoldHookRight,
    FoldHookLeft,
    TowardRightHookLeft,
    TowardLeftHookLeft,
    TowardLeftHookRight,
    TowardRightHookRight,
    BendBreak,
    Scythe,
    AcuteFold,
    BendBarb,
    AcuteBarb,
    AcuteBreak,
    AcuteLineHigh,
    AcuteLineLow,
    DoubleBend,
    BreakSlightRight,
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
            pen.break_stroke()
            bottom_ending.draw(pen)
