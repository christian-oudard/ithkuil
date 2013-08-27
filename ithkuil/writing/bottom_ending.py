#TODO: barbs are a little bit excessive.
import math
import vec
from .common import (
    Ending,
    slant45,
    slant60,
    BOTTOM,
    UNDER,
)
from .util import hook


class BottomEnding(Ending):

    pronunciation = NotImplemented

    def __init__(self, character, mode, straight, flipped):
        super().__init__(character, mode)
        self.straight = straight
        self.flipped = flipped

    def __str__(self):
        return 'bottom_ending.{}'.format(self.__class__.__name__)

    def predict_slant_width(self, pen, end_angle):
        temp_pen = pen.copy()
        temp_pen.line_to_y(BOTTOM, end_slant=end_angle)
        return temp_pen.last_slant_width()


class Normal(BottomEnding):

    pronunciation = ''

    def angle(self):
        if self.straight:
            if not self.flipped:
                return 45
            else:
                return -45
        else:
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
        pen.move_forward(pen.last_slant_width() / 2 + self.width / 2)

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
        pen.move_forward(pen.last_slant_width() / 2 + self.width * slant45 / 2)

        pen.turn_to(-90)
        pen.line_forward(2.0, start_slant=45, end_slant=45)


class RightOnRight(BottomEnding):

    pronunciation = 'l,-'

    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(
            BOTTOM - slant_width / slant45 / 2,
            end_slant=45,
        )

        pen.turn_to(45)
        pen.move_to_y(BOTTOM + self.width / 2)
        pen.turn_to(0)
        pen.line_forward(2.0, start_slant=45, end_slant=45)


class DiagonalDownLeftOnRight(BottomEnding):

    pronunciation = 'n-'

    def draw(self, pen):
        pen.line_to_y(BOTTOM, end_slant=0)
        pen.turn_to(0)
        pen.move_forward(pen.last_slant_width() / 2 + self.width * slant45 / 2)
        pen.turn_to(-135)
        pen.line_forward(2.0, start_slant=0, end_slant=0)


class Bend(BottomEnding):

    pronunciation = ('s-', 'z-')

    def draw(self, pen):
        if self.straight:
            pen.line_to_y(BOTTOM + self.width / 2)
            pen.turn_to(0)
            pen.line_forward(2.5, end_slant=-45)
        else:
            temp_pen = pen.copy()
            temp_pen.line_to_y(BOTTOM)
            temp_pen.turn_to(-90)
            temp_pen.line_forward(2.0)
            seg = temp_pen.last_segment()
            offset_y = seg.a_right.y - BOTTOM

            pen.line_to_y(BOTTOM - offset_y)
            pen.turn_to(-90)
            pen.line_forward(2.5, end_slant=45)


class Fold(BottomEnding):

    pronunciation = ('s^-', 'z^-')

    def draw(self, pen):
        if self.straight:
            pen.line_to_y(
                BOTTOM + self.width / 2,
                end_slant=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(self.width * slant45)
            pen.turn_to(0)
            pen.line_forward(2.0, start_slant=-45, end_slant=-45)
        else:
            slant_width = self.predict_slant_width(pen, 45)
            pen.line_to_y(
                BOTTOM + slant_width / slant45 / 2,
                end_slant=45,
            )
            pen.turn_to(-135)
            pen.move_forward(pen.last_slant_width() / 2 + self.width * slant45 / 2)
            pen.turn_to(180)
            pen.line_forward(2.0, start_slant=45, end_slant=45)


class Barb(BottomEnding):

    pronunciation = 'n^-'

    angle = Normal.angle

    def draw(self, pen):
        pen.line_to_y(BOTTOM + self.width / 4, end_slant=self.angle())

        old_mode = pen.mode
        new_mode = old_mode.copy()
        new_mode.width = self.width / 2
        pen.set_mode(new_mode)

        if self.straight:
            pen.turn_to(45)
            pen.line_forward(2 * self.width)
            seg = pen.last_segment()
            seg.b_left = seg.a_left
        else:
            pen.turn_to(180)
            pen.line_forward(2 * self.width)
            seg = pen.last_segment()
            seg.b_right = seg.a_right

        pen.set_mode(old_mode)


class DiagonalUpRight(BottomEnding):

    pronunciation = 'z-'

    def draw(self, pen):
        if self.straight:
            pen.line_to_y(BOTTOM + self.width / 2, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(pen.last_slant_width() / 2 + self.width / 2)
            pen.turn_to(45)
            pen.line_forward(2.0, start_slant=-45, end_slant=0)
        else:
            slant_width = self.predict_slant_width(pen, 90)
            pen.line_to_y(
                BOTTOM - slant_width / 2,
                end_slant=90
            )
            pen.turn_to(90)
            pen.move_to_y(self.width * slant60 / 2)
            pen.turn_to(30)
            pen.line_forward(2.0, start_slant=90, end_slant=90)


class Acute(BottomEnding):

    pronunciation = 'r^-'

    def straight_acute(self, pen, angle):
        temp_pen = pen.copy()
        temp_pen.line_to_y(BOTTOM)
        temp_pen.turn_to(angle)
        temp_pen.line_forward(2.0)
        seg = temp_pen.last_segment()
        offset_y = seg.a_right.y - BOTTOM

        pen.line_to_y(BOTTOM - offset_y)
        pen.turn_to(angle)

    def draw(self, pen):
        if self.straight:
            self.straight_acute(pen, 30)
            pen.line_forward(2.0, end_slant=90)
        else:
            pen.line_to_y(BOTTOM + self.width / 2)
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
        pen.move_to_y(BOTTOM - self.width / 2)
        pen.turn_to(0)
        pen.line_forward(3, start_slant=45, end_slant=45)


class Break(BottomEnding):

    pronunciation = ('c-', 'dz-')

    def draw(self, pen):
        if self.straight:
            pen.line_to_y(BOTTOM - self.width / 2, end_slant=-45)
            pen.turn_to(135)
            pen.move_forward(self.width * slant45)
            pen.turn_to(-90)
            pen.line_forward(2.0, start_slant=-45, end_slant=-45)
        else:
            pen.line_to_y(BOTTOM, end_slant=0)
            original_heading = pen.heading
            pen.turn_to(180)
            pen.move_forward(pen.last_slant_width())
            pen.turn_to(original_heading)
            pen.line_forward(2.0, start_slant=0, end_slant=0)


class BreakTurnLeft(BottomEnding):

    pronunciation = ('c^-', 'j-')

    def draw(self, pen):
        if self.straight:
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(180)
            pen.move_forward(self.width / 2 + self.width * slant45 / 2)
            pen.turn_to(-45)
            pen.line_forward(2.0, start_slant=0, end_slant=0)
        else:
            slant_width = self.predict_slant_width(pen, 90)
            pen.line_to_y(
                BOTTOM + slant_width / 2,
                end_slant=90
            )
            pen.turn_to(-90)
            pen.move_to_y(BOTTOM - self.width * slant60 / 2)
            pen.turn_to(30)
            pen.line_forward(2.0, start_slant=90, end_slant=90)


class BreakTurnRight(BottomEnding):

    pronunciation = 'z^-'

    def draw(self, pen):
        if self.straight:
            pen.line_to_y(BOTTOM - self.width / 2, end_slant=-45)
            pen.turn_to(135)
            pen.move_forward(self.width * slant45)
            pen.turn_to(180)
            pen.line_forward(2.0, start_slant=-45, end_slant=-45)
        else:
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(180)
            pen.move_forward(pen.last_slant_width() / 2 + self.width * slant45 / 2)
            pen.turn_to(-135)
            pen.line_forward(2.0, start_slant=0, end_slant=0)


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
        if self.straight:
            pen.line_to_y(
                BOTTOM + self.width / 2,
                end_slant=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(self.width * slant45 / 2)
            hook(pen, 90, -30, 2.5, adjust_outside=15)
        else:
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(pen.last_slant_width() / 2)
            hook(pen, 60, -30, 2.5, adjust_outside=15)


class FoldHookLeft(BottomEnding):

    pronunciation = ('t-', 'd-')

    def draw(self, pen):
        if self.straight:
            pen.line_to_y(
                BOTTOM + self.width / 2,
                end_slant=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(self.width * slant45 / 2)
            hook(pen, 60, 30, 2.5, adjust_outside=15)
        else:
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
        if self.straight:
            pen.line_to_y(
                BOTTOM + self.width / 2,
                end_slant=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(self.width * slant45 / 2)
            hook(pen, -120, 30, 2.5, adjust_outside=15)
        else:
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(pen.last_slant_width() / 2)
            hook(pen, -120, 30, 2.5, adjust_outside=15)


class TowardLeftHookRight(BottomEnding):

    pronunciation = ('f,-', 'v-')

    def draw(self, pen):
        if self.straight:
            pen.line_to_y(
                BOTTOM + self.width / 2,
                end_slant=-45,
            )
            pen.turn_to(-45)
            pen.move_forward(self.width * slant45 / 2)
            hook(pen, -120, -45, 3.0, adjust_outside=5)
        else:
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
        if self.straight:
            pen.line_to_y(BOTTOM + self.width / 2)
            pen.turn_to(0)
            pen.line_forward(2.0, end_slant=45)
            pen.turn_to(-135)
            pen.move_forward(self.width * slant45)
            pen.turn_to(0)
            pen.line_forward(2.0, start_slant=45, end_slant=45)
        else:
            pen.line_to_y(BOTTOM + slant45, end_slant=0)
            pen.turn_to(-135)
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(self.width * slant45)
            pen.turn_to(-135)
            pen.line_forward(2.0, start_slant=0, end_slant=0)


class Scythe(BottomEnding):

    pronunciation = '-w'

    def draw(self, pen):
        #TODO: this is not very good.

        def scythe_cap(pen, end):
            start_heading = pen.heading

            switch = False
            if self.character.mirrored_x:
                switch = not switch
            if self.flipped:
                switch = not switch

            if not switch:
                top = end
                bottom = pen.position
            else:
                top = pen.position
                bottom = end

            # Trace the curves with a temporary pen.
            temp_pen = pen.copy()
            if not switch:
                arc = temp_pen.arc_right
            else:
                arc = temp_pen.arc_left

            temp_pen.move_to(top)
            temp_pen.turn_to(start_heading)
            outer_arcs = [
                (2.4, 1.6),
                (1.0, 2.8),
            ]
            outer_points = []
            for radius, distance in outer_arcs:
                circumference = radius * 2 * math.pi
                circle_ratio = distance / circumference
                angle = circle_ratio * 360
                arc(angle, radius)
                outer_points.append(temp_pen.position)
            outer_tip_angle = temp_pen.heading

            temp_pen.move_to(bottom)
            temp_pen.turn_to(start_heading)
            temp_pen.move_forward(0.5)
            inner_forward = temp_pen.position
            temp_pen.arc_to(outer_points[-1])
            inner_tip_angle = temp_pen.heading

            # Draw with the real pen.
            if not switch:
                pen.line_to(inner_forward)
                pen.arc_to(outer_points[-1])
                pen.turn_to(outer_tip_angle + 180)
                for p in reversed(outer_points[:-1]):
                    pen.arc_to(p)
                pen.arc_to(top)
            else:
                for p in outer_points:
                    pen.arc_to(p)
                pen.turn_to(inner_tip_angle + 180)
                pen.arc_to(inner_forward)
                pen.line_to(bottom)

        pen.line_to_y(BOTTOM + self.width / 2)
        pen.turn_to(0)

        # See how far forward we have to go to make the top of the stroke
        # zero-length.
        temp_pen = pen.copy(paper=True)
        temp_pen.line_forward(self.width, end_slant=90)
        seg = temp_pen.last_segment()
        extra_left = vec.mag(vec.vfrom(seg.a_left, seg.b_left))
        extra_right = vec.mag(vec.vfrom(seg.a_right, seg.b_right))
        extra = min(extra_left, extra_right)
        dist = self.width - extra
        pen.line_forward(dist, end_slant=90)
        pen.last_segment().end_cap = scythe_cap


class AcuteFold(Acute):

    pronunciation = '-y'

    def draw(self, pen):
        if self.straight:
            self.straight_acute(pen, 30)
            pen.line_forward(2.0, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(pen.last_slant_width() / 2 + self.width / slant60)
            pen.turn_to(-90)
            pen.line_forward(2.0, start_slant=-45, end_slant=-45)
        else:
            pen.line_to_y(BOTTOM + self.width / 2)
            pen.turn_to(180)
            pen.line_forward(3.0, end_slant=45)
            pen.turn_to(-135)
            pen.move_forward(self.width * slant45)
            pen.turn_to(-90)
            pen.line_forward(2.0, start_slant=45, end_slant=45)


class BendBarb(BottomEnding):

    pronunciation = '-l'

    def draw(self, pen):
        if self.straight:
            pen.line_to_y(BOTTOM + self.width / 2, end_slant=0)
            pen.turn_to(0)
            pen.line_forward(2.5, end_slant=45)

        else:
            temp_pen = pen.copy()
            temp_pen.line_to_y(BOTTOM, end_slant=0)
            temp_pen.turn_to(-90)
            temp_pen.line_forward(2.0)
            seg = temp_pen.last_segment()
            offset_y = seg.a_right.y - BOTTOM

            pen.line_to_y(BOTTOM - offset_y)
            pen.turn_to(-90)
            pen.line_forward(2.5, end_slant=45)

        old_mode = pen.mode
        new_mode = old_mode.copy()
        new_mode.width = self.width / 2
        pen.set_mode(new_mode)

        if self.straight:
            pen.turn_to(135)
            pen.line_forward(2 * self.width)
            seg = pen.last_segment()
            seg.b_left = seg.a_left
        else:
            pen.turn_to(45)
            pen.line_forward(2 * self.width)
            seg = pen.last_segment()
            seg.b_left = seg.a_left

        pen.set_mode(old_mode)


class AcuteBarb(Acute):

    pronunciation = '-r'

    def draw(self, pen):
        if self.straight:
            self.straight_acute(pen, 30)
            pen.line_forward(2.0, end_slant=90)
        else:
            pen.line_to_y(BOTTOM + self.width / 2)
            pen.turn_to(180)
            pen.line_forward(2.5, end_slant=-45)

        old_mode = pen.mode
        new_mode = old_mode.copy()
        new_mode.width = self.width / 2
        pen.set_mode(new_mode)

        if self.straight:
            pen.turn_to(-90)
            pen.line_forward(2 * self.width)
            seg = pen.last_segment()
            seg.b_right = seg.a_right
        else:
            pen.turn_to(-45)
            pen.line_forward(2 * self.width)
            seg = pen.last_segment()
            seg.b_left = seg.a_left

        pen.set_mode(old_mode)


class AcuteBreak(Acute):

    pronunciation = '-r^'

    def draw(self, pen):
        if self.straight:
            self.straight_acute(pen, 30)
            pen.line_forward(2.0, end_slant=90)
            pen.turn_to(-90)
            pen.move_forward(pen.last_slant_width())
            pen.turn_to(30)
            pen.line_forward(2.0, start_slant=90, end_slant=90)
        else:
            pen.line_to_y(BOTTOM + self.width / 2)
            pen.turn_to(180)
            pen.line_forward(2.0, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(self.width * slant45)
            pen.turn_to(180)
            pen.line_forward(2.0, start_slant=-45, end_slant=-45)


class AcuteLineHigh(Acute):

    pronunciation = '-m'

    def draw(self, pen):
        if self.straight:
            self.straight_acute(pen, 22.5)
            x = self.width / math.tan(pen.heading.rad)
            pen.line_to_x(pen.position.x - self.width / 2 + x, end_slant=90)
            pen.turn_to(-90)
            pen.move_to_y(BOTTOM + self.width / 2)
            pen.turn_to(0)
            pen.line_forward(2.0, start_slant=90, end_slant=-45)
        else:
            pen.line_to_y(BOTTOM + self.width / 2)
            pen.turn_to(180)
            pen.line_forward(2.0, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(self.width * slant45 / 2 + self.width / 2)
            pen.turn_to(-135)
            pen.line_forward(2.0, start_slant=-45, end_slant=90)


class AcuteLineLow(Acute):

    pronunciation = '-n'

    def draw(self, pen):
        if self.straight:
            self.straight_acute(pen, 30)
            x = self.width * slant60 / math.tan(pen.heading.rad)
            pen.line_to_x(pen.position.x - self.width / 2 + x, end_slant=90)
            pen.turn_to(-90)
            pen.move_to_y(BOTTOM + self.width * slant60 / 2)
            pen.turn_to(-30)
            pen.line_forward(2.0, start_slant=90, end_slant=90)
        else:
            pen.line_to_y(BOTTOM + self.width / 2)
            pen.turn_to(180)
            pen.line_forward(2.0, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(self.width * slant45)
            pen.turn_to(-90)
            pen.line_forward(2.0, start_slant=-45, end_slant=-45)


class DoubleBend(Acute):

    pronunciation = '-v'

    def draw(self, pen):
        if self.straight:
            self.straight_acute(pen, 22.5)
            pen.line_forward(1.5)
            pen.turn_to(-22.5)
            slant_width = self.predict_slant_width(pen, 90)
            pen.line_to_y(BOTTOM + slant_width / 2, end_slant=90)
        else:
            start_heading = pen.heading
            pen.line_to_y(BOTTOM + self.width / 2)
            pen.turn_to(180)
            pen.line_forward(2.0)
            pen.turn_to(start_heading)
            pen.line_to_y(BOTTOM - 1.0, end_slant=0)


class BreakSlightRight(BottomEnding):

    pronunciation = '-n^'

    def draw(self, pen):
        if self.straight:
            slant_width = self.predict_slant_width(pen, -45)
            pen.line_to_y(
                BOTTOM + slant_width / slant45 / 2,
                end_slant=-45,
            )

            pen.turn_to(135)
            pen.move_forward(pen.last_slant_width() / 2 + self.width / 2)
            pen.turn_to(-135)
            pen.line_to_y(BOTTOM, end_slant=0)
        else:
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(180)
            pen.move_forward(pen.last_slant_width() / 2 + self.width / 2)
            pen.turn_to(-90)
            pen.line_forward(2.0, start_slant=0, end_slant=-45)



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
