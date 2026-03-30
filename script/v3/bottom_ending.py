"""Ithkuil V3 bottom endings (attached to bottom of consonant body)."""

import math
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from .common import Ending, slant45, slant60, BOTTOM, UNDER
from .util import hook


class BottomEnding(Ending):

    pronunciation = NotImplemented

    def __init__(self, character, straight, flipped):
        super().__init__(character)
        self.straight = straight
        self.flipped = flipped

    def predict_slant_width(self, pen, end_angle):
        """Measure the slant cap width if a line were drawn to BOTTOM with end_slant=end_angle."""
        temp = pen.copy()
        temp.line_to_y(BOTTOM, end_slant=end_angle)
        return temp.last_slant_width()


class Normal(BottomEnding):
    pronunciation = ''

    def angle(self):
        if self.straight:
            return -45 if self.flipped else 45
        return 0

    def draw(self, pen):
        pen.line_to_y(BOTTOM, end_slant=self.angle())


class Long(Normal):
    pronunciation = 'l-'

    def draw(self, pen):
        pen.line_to_y(UNDER, end_slant=self.angle())


class DiagonalDownRightOnRight(BottomEnding):
    pronunciation = 'm-'

    def draw(self, pen):
        w = pen.width
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(BOTTOM + slant_width / slant45 / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + w / 2)
        pen.turn_to(-45)
        pen.line_to_y(BOTTOM, end_slant=0)


class DownOnRight(BottomEnding):
    pronunciation = 'r-'

    def draw(self, pen):
        w = pen.width
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(BOTTOM - slant_width / slant45 / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + w * slant45 / 2)
        pen.turn_to(-90)
        pen.line_forward(2.0, start_slant=45, end_slant=45)


class RightOnRight(BottomEnding):
    pronunciation = 'l,-'

    def draw(self, pen):
        w = pen.width
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(BOTTOM - slant_width / slant45 / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_to_y(BOTTOM + w / 2)
        pen.turn_to(0)
        pen.line_forward(2.0, start_slant=45, end_slant=45)


class DiagonalDownLeftOnRight(BottomEnding):
    pronunciation = 'n-'

    def draw(self, pen):
        w = pen.width
        pen.line_to_y(BOTTOM, end_slant=0)
        pen.turn_to(0)
        pen.move_forward(pen.last_slant_width() / 2 + w * slant45 / 2)
        pen.turn_to(-135)
        pen.line_forward(2.0, start_slant=0, end_slant=0)


class Bend(BottomEnding):
    pronunciation = ('s-', 'z-')

    def draw(self, pen):
        w = pen.width
        if self.straight:
            pen.line_to_y(BOTTOM + w / 2)
            pen.turn_to(0)
            pen.line_forward(2.5, end_slant=-45)
        else:
            temp = pen.copy()
            temp.line_to_y(BOTTOM)
            temp.turn_to(-90)
            temp.line_forward(2.0)
            seg = temp.last_segment()
            offset_y = seg.a_right[1] - BOTTOM
            pen.line_to_y(BOTTOM - offset_y)
            pen.turn_to(-90)
            pen.line_forward(2.5, end_slant=45)


class Fold(BottomEnding):
    pronunciation = ('s^-', 'z^-')

    def draw(self, pen):
        w = pen.width
        if self.straight:
            pen.line_to_y(BOTTOM + w / 2, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(w * slant45)
            pen.turn_to(0)
            pen.line_forward(2.0, start_slant=-45, end_slant=-45)
        else:
            slant_width = self.predict_slant_width(pen, 45)
            pen.line_to_y(BOTTOM + slant_width / slant45 / 2, end_slant=45)
            pen.turn_to(-135)
            pen.move_forward(pen.last_slant_width() / 2 + w * slant45 / 2)
            pen.turn_to(180)
            pen.line_forward(2.0, start_slant=45, end_slant=45)


class Barb(BottomEnding):
    pronunciation = 'n^-'

    def draw(self, pen):
        w = pen.width
        angle = Normal(self.character, self.straight, self.flipped).angle()
        pen.line_to_y(BOTTOM + w / 4, end_slant=angle)
        pen.set_width(w / 2)
        if self.straight:
            pen.turn_to(45)
            pen.line_forward(2 * pen.width)
            seg = pen.last_segment()
            seg.b_left = seg.a_left
        else:
            pen.turn_to(180)
            pen.line_forward(2 * pen.width)
            seg = pen.last_segment()
            seg.b_right = seg.a_right
        pen.set_width(w)


class DiagonalUpRight(BottomEnding):
    pronunciation = 'z-'

    def draw(self, pen):
        w = pen.width
        if self.straight:
            pen.line_to_y(BOTTOM + w / 2, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(pen.last_slant_width() / 2 + w / 2)
            pen.turn_to(45)
            pen.line_forward(2.0, start_slant=-45, end_slant=0)
        else:
            slant_width = self.predict_slant_width(pen, 90)
            pen.line_to_y(BOTTOM - slant_width / 2, end_slant=90)
            pen.turn_to(90)
            pen.move_to_y(w * slant60 / 2)
            pen.turn_to(30)
            pen.line_forward(2.0, start_slant=90, end_slant=90)


class Acute(BottomEnding):
    pronunciation = 'r^-'

    def straight_acute(self, pen, angle):
        temp = pen.copy()
        temp.line_to_y(BOTTOM)
        temp.turn_to(angle)
        temp.line_forward(2.0)
        seg = temp.last_segment()
        offset_y = seg.a_right[1] - BOTTOM
        pen.line_to_y(BOTTOM - offset_y)
        pen.turn_to(angle)

    def draw(self, pen):
        w = pen.width
        if self.straight:
            self.straight_acute(pen, 30)
            pen.line_forward(2.0, end_slant=90)
        else:
            pen.line_to_y(BOTTOM + w / 2)
            pen.turn_to(180)
            pen.line_forward(2.5, end_slant=-45)


class RightOnBottom(BottomEnding):
    pronunciation = 'c,-'

    def draw(self, pen):
        w = pen.width
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(BOTTOM + slant_width / slant45 / 2, end_slant=45)
        pen.turn_to(-135)
        pen.move_to_y(BOTTOM - w / 2)
        pen.turn_to(0)
        pen.line_forward(3, start_slant=45, end_slant=45)


class Break(BottomEnding):
    pronunciation = ('c-', 'dz-')

    def draw(self, pen):
        w = pen.width
        if self.straight:
            pen.line_to_y(BOTTOM - w / 2, end_slant=-45)
            pen.turn_to(135)
            pen.move_forward(w * slant45)
            pen.turn_to(-90)
            pen.line_forward(2.0, start_slant=-45, end_slant=-45)
        else:
            pen.line_to_y(BOTTOM, end_slant=0)
            original_heading = pen.heading
            pen.turn_to(180)
            pen.move_forward(pen.last_slant_width())
            pen.turn_to(float(original_heading))
            pen.line_forward(2.0, start_slant=0, end_slant=0)


class BreakTurnLeft(BottomEnding):
    pronunciation = ('c^-', 'j-')

    def draw(self, pen):
        w = pen.width
        if self.straight:
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(180)
            pen.move_forward(w / 2 + w * slant45 / 2)
            pen.turn_to(-45)
            pen.line_forward(2.0, start_slant=0, end_slant=0)
        else:
            slant_width = self.predict_slant_width(pen, 90)
            pen.line_to_y(BOTTOM + slant_width / 2, end_slant=90)
            pen.turn_to(-90)
            pen.move_to_y(BOTTOM - w * slant60 / 2)
            pen.turn_to(30)
            pen.line_forward(2.0, start_slant=90, end_slant=90)


class BreakTurnRight(BottomEnding):
    pronunciation = 'z^-'

    def draw(self, pen):
        w = pen.width
        if self.straight:
            pen.line_to_y(BOTTOM - w / 2, end_slant=-45)
            pen.turn_to(135)
            pen.move_forward(w * slant45)
            pen.turn_to(180)
            pen.line_forward(2.0, start_slant=-45, end_slant=-45)
        else:
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(180)
            pen.move_forward(pen.last_slant_width() / 2 + w * slant45 / 2)
            pen.turn_to(-135)
            pen.line_forward(2.0, start_slant=0, end_slant=0)


class HookLeftOnRight(BottomEnding):
    pronunciation = ('k-', 'g-')

    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(BOTTOM + slant_width / slant45 / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(slant_width / 2)
        hook(pen, -90, 90, 3.5, adjust_outside=15)


class HookRightOnRight(BottomEnding):
    pronunciation = ('p-', 'b-')

    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(BOTTOM + slant_width / slant45 / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(slant_width / 2)
        hook(pen, -90, -90, 3.5, adjust_outside=15)


class FoldHookRight(BottomEnding):
    pronunciation = 'q-'

    def draw(self, pen):
        w = pen.width
        if self.straight:
            pen.line_to_y(BOTTOM + w / 2, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(w * slant45 / 2)
            hook(pen, 90, -90, 3.0, adjust_outside=15)
        else:
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(pen.last_slant_width() / 2)
            hook(pen, 60, -90, 3.0, adjust_outside=15)


class FoldHookLeft(BottomEnding):
    pronunciation = ('t-', 'd-')

    def draw(self, pen):
        w = pen.width
        if self.straight:
            pen.line_to_y(BOTTOM + w / 2, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(w * slant45 / 2)
            hook(pen, 60, 90, 3.0, adjust_outside=15)
        else:
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(pen.last_slant_width() / 2)
            hook(pen, 60, 90, 3.0, adjust_outside=15)


class TowardRightHookLeft(BottomEnding):
    pronunciation = 'x-'

    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(BOTTOM + slant_width / slant45 / 2, end_slant=45)
        pen.turn_to(-135)
        pen.move_to_y(BOTTOM)
        hook(pen, 120, 90, 4.5, adjust_outside=5)


class TowardLeftHookLeft(BottomEnding):
    pronunciation = ('t,-', 'dh-')

    def draw(self, pen):
        w = pen.width
        if self.straight:
            pen.line_to_y(BOTTOM + w / 2, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(w * slant45 / 2)
            hook(pen, -120, 90, 4.0, adjust_outside=15)
        else:
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(pen.last_slant_width() / 2)
            hook(pen, -120, 90, 4.0, adjust_outside=15)


class TowardLeftHookRight(BottomEnding):
    pronunciation = ('f,-', 'v-')

    def draw(self, pen):
        w = pen.width
        if self.straight:
            pen.line_to_y(BOTTOM + w / 2, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(w * slant45 / 2)
            hook(pen, -120, -90, 4.0, adjust_outside=5)
        else:
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(pen.last_slant_width() / 2)
            hook(pen, -120, -90, 4.0, adjust_outside=5)


class TowardRightHookRight(BottomEnding):
    pronunciation = 'xh-'

    def draw(self, pen):
        slant_width = self.predict_slant_width(pen, 45)
        pen.line_to_y(BOTTOM + slant_width / slant45 / 2, end_slant=45)
        pen.turn_to(-135)
        pen.move_to_y(BOTTOM)
        hook(pen, 120, -90, 4.5, adjust_outside=15)


class BendBreak(BottomEnding):
    pronunciation = 'GEMINATE'

    def draw(self, pen):
        w = pen.width
        if self.straight:
            pen.line_to_y(BOTTOM + w / 2)
            pen.turn_to(0)
            pen.line_forward(2.0, end_slant=45)
            pen.turn_to(-135)
            pen.move_forward(w * slant45)
            pen.turn_to(0)
            pen.line_forward(2.0, start_slant=45, end_slant=45)
        else:
            pen.line_to_y(BOTTOM + slant45, end_slant=0)
            pen.turn_to(-135)
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(w * slant45)
            pen.turn_to(-135)
            pen.line_forward(2.0, start_slant=0, end_slant=0)


class AcuteFold(Acute):
    pronunciation = '-y'

    def draw(self, pen):
        w = pen.width
        if self.straight:
            self.straight_acute(pen, 30)
            pen.line_forward(2.0, end_slant=-45)
            pen.turn_to(-45)
            slant60_inv = 1 / math.sin(math.radians(60))
            pen.move_forward(pen.last_slant_width() / 2 + w / slant60_inv)
            pen.turn_to(-90)
            pen.line_forward(2.0, start_slant=-45, end_slant=-45)
        else:
            pen.line_to_y(BOTTOM + w / 2)
            pen.turn_to(180)
            pen.line_forward(3.0, end_slant=45)
            pen.turn_to(-135)
            pen.move_forward(w * slant45)
            pen.turn_to(-90)
            pen.line_forward(2.0, start_slant=45, end_slant=45)


class BendBarb(BottomEnding):
    pronunciation = '-l'

    def draw(self, pen):
        w = pen.width
        if self.straight:
            pen.line_to_y(BOTTOM + w / 2, end_slant=0)
            pen.turn_to(0)
            pen.line_forward(2.5, end_slant=45)
        else:
            temp = pen.copy()
            temp.line_to_y(BOTTOM, end_slant=0)
            temp.turn_to(-90)
            temp.line_forward(2.0)
            seg = temp.last_segment()
            offset_y = seg.a_right[1] - BOTTOM
            pen.line_to_y(BOTTOM - offset_y)
            pen.turn_to(-90)
            pen.line_forward(2.5, end_slant=45)

        pen.set_width(w / 2)
        if self.straight:
            pen.turn_to(135)
            pen.line_forward(2 * pen.width)
            seg = pen.last_segment()
            seg.b_left = seg.a_left
        else:
            pen.turn_to(45)
            pen.line_forward(2 * pen.width)
            seg = pen.last_segment()
            seg.b_left = seg.a_left
        pen.set_width(w)


class AcuteBarb(Acute):
    pronunciation = '-r'

    def draw(self, pen):
        w = pen.width
        if self.straight:
            self.straight_acute(pen, 30)
            pen.line_forward(2.0, end_slant=90)
        else:
            pen.line_to_y(BOTTOM + w / 2)
            pen.turn_to(180)
            pen.line_forward(2.5, end_slant=-45)

        pen.set_width(w / 2)
        if self.straight:
            pen.turn_to(-90)
            pen.line_forward(2 * pen.width)
            seg = pen.last_segment()
            seg.b_right = seg.a_right
        else:
            pen.turn_to(-45)
            pen.line_forward(2 * pen.width)
            seg = pen.last_segment()
            seg.b_left = seg.a_left
        pen.set_width(w)


class AcuteBreak(Acute):
    pronunciation = '-r^'

    def draw(self, pen):
        w = pen.width
        if self.straight:
            self.straight_acute(pen, 30)
            pen.line_forward(2.0, end_slant=90)
            pen.turn_to(-90)
            pen.move_forward(pen.last_slant_width())
            pen.turn_to(30)
            pen.line_forward(2.0, start_slant=90, end_slant=90)
        else:
            pen.line_to_y(BOTTOM + w / 2)
            pen.turn_to(180)
            pen.line_forward(2.0, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(w * slant45)
            pen.turn_to(180)
            pen.line_forward(2.0, start_slant=-45, end_slant=-45)


class AcuteLineHigh(Acute):
    pronunciation = '-m'

    def draw(self, pen):
        w = pen.width
        if self.straight:
            self.straight_acute(pen, 22.5)
            hr = math.radians(float(pen.heading))
            x = w / math.tan(hr) if abs(math.tan(hr)) > 1e-9 else 0
            pen.line_to_x(pen._x - w / 2 + x, end_slant=90)
            pen.turn_to(-90)
            pen.move_to_y(BOTTOM + w / 2)
            pen.turn_to(0)
            pen.line_forward(2.0, start_slant=90, end_slant=-45)
        else:
            pen.line_to_y(BOTTOM + w / 2)
            pen.turn_to(180)
            pen.line_forward(2.0, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(w * slant45 / 2 + w / 2)
            pen.turn_to(-135)
            pen.line_forward(2.0, start_slant=-45, end_slant=90)


class AcuteLineLow(Acute):
    pronunciation = '-n'

    def draw(self, pen):
        w = pen.width
        if self.straight:
            self.straight_acute(pen, 30)
            hr = math.radians(float(pen.heading))
            x = w * slant60 / math.tan(hr) if abs(math.tan(hr)) > 1e-9 else 0
            pen.line_to_x(pen._x - w / 2 + x, end_slant=90)
            pen.turn_to(-90)
            pen.move_to_y(BOTTOM + w * slant60 / 2)
            pen.turn_to(-30)
            pen.line_forward(2.0, start_slant=90, end_slant=90)
        else:
            pen.line_to_y(BOTTOM + w / 2)
            pen.turn_to(180)
            pen.line_forward(2.0, end_slant=-45)
            pen.turn_to(-45)
            pen.move_forward(w * slant45)
            pen.turn_to(-90)
            pen.line_forward(2.0, start_slant=-45, end_slant=-45)


class DoubleBend(Acute):
    pronunciation = '-v'

    def draw(self, pen):
        w = pen.width
        if self.straight:
            self.straight_acute(pen, 22.5)
            pen.line_forward(1.5)
            pen.turn_to(-22.5)
            slant_width = self.predict_slant_width(pen, 90)
            pen.line_to_y(BOTTOM + slant_width / 2, end_slant=90)
        else:
            start_heading = pen.heading
            pen.line_to_y(BOTTOM + w / 2)
            pen.turn_to(180)
            pen.line_forward(2.0)
            pen.turn_to(float(start_heading))
            pen.line_to_y(BOTTOM - 1.0, end_slant=0)


class BreakSlightRight(BottomEnding):
    pronunciation = '-n^'

    def draw(self, pen):
        w = pen.width
        if self.straight:
            slant_width = self.predict_slant_width(pen, -45)
            pen.line_to_y(BOTTOM + slant_width / slant45 / 2, end_slant=-45)
            pen.turn_to(135)
            pen.move_forward(pen.last_slant_width() / 2 + w / 2)
            pen.turn_to(-135)
            pen.line_to_y(BOTTOM, end_slant=0)
        else:
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(180)
            pen.move_forward(pen.last_slant_width() / 2 + w / 2)
            pen.turn_to(-90)
            pen.line_forward(2.0, start_slant=0, end_slant=-45)


# ---------------------------------------------------------------------------
# Bottom ending list and lookup tables
# ---------------------------------------------------------------------------

bottom_endings_by_pronunciation = {}
for _be in [
    Normal, Long,
    DiagonalDownRightOnRight, DownOnRight, RightOnRight, DiagonalDownLeftOnRight,
    Bend, Fold, Barb,
    DiagonalUpRight, Acute, RightOnBottom,
    Break, BreakTurnLeft, BreakTurnRight,
    HookLeftOnRight, HookRightOnRight,
    FoldHookRight, FoldHookLeft,
    TowardRightHookLeft, TowardLeftHookLeft, TowardLeftHookRight, TowardRightHookRight,
    BendBreak, AcuteFold, BendBarb, AcuteBarb, AcuteBreak,
    AcuteLineHigh, AcuteLineLow, DoubleBend, BreakSlightRight,
]:
    _p = _be.pronunciation
    if isinstance(_p, str):
        if _p:
            bottom_endings_by_pronunciation[_p] = _be
    else:
        for _key in _p:
            bottom_endings_by_pronunciation[_key] = _be

bottom_endings = [
    Normal, Long,
    DiagonalDownRightOnRight, DownOnRight, RightOnRight, DiagonalDownLeftOnRight,
    Bend, Fold, Barb,
    DiagonalUpRight, Acute, RightOnBottom,
    Break, BreakTurnLeft, BreakTurnRight,
    HookLeftOnRight, HookRightOnRight,
    FoldHookRight, FoldHookLeft,
    TowardRightHookLeft, TowardLeftHookLeft, TowardLeftHookRight, TowardRightHookRight,
    BendBreak, AcuteFold, BendBarb, AcuteBarb, AcuteBreak,
    AcuteLineHigh, AcuteLineLow, DoubleBend, BreakSlightRight,
]
