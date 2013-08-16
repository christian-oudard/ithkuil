import math

import vec
from canoepaddle.geometry import intersect_circle_line, closest_point_to
from canoepaddle.heading import Heading

from .common import (
    Ending,
    slant45,
    slant60,
    WIDTH,
    BOTTOM,
    UNDER,
)


# TODO: change base width based on angles so the hook looks more consistent.

def hook(pen, start_heading, end_heading, distance):
    start_heading = Heading(start_heading)
    end_heading = Heading(end_heading)

    # We drop into fill mode temporarily to draw the hook outline.
    old_mode = pen.mode
    pen.set_mode(old_mode.outliner_mode())

    # The pen starts at the "left" corner of the hook, facing along the
    # base of the hook.
    base_heading = pen.heading
    left_corner = pen.position
    pen.move_forward(WIDTH)
    right_corner = pen.position

    # Calculate the radius.
    arc_angle = end_heading - start_heading
    if arc_angle > 180:
        arc_angle -= 360
    circumference = distance / (arc_angle.theta / 360)
    radius = circumference / (2 * math.pi)

    # Find the tip of the hook.
    # From the center of the base, make an arc to the tip.
    # Positive arc angles curve to the left, and negative arc angles
    pen.move_forward(-WIDTH / 2)
    pen.turn_to(start_heading)
    pen.arc_left(arc_angle, radius)
    tip = pen.position
    pen.undo()

    # Find the correct headings for the side arcs.
    pen.move_to(left_corner)
    pen.turn_to(start_heading)
    pen.arc_to(tip)
    left_end_heading = pen.last_segment().end_heading
    pen.undo()

    pen.move_to(right_corner)
    pen.turn_to(start_heading)
    pen.arc_to(tip)
    right_start_heading = pen.last_segment().start_heading
    pen.undo()

    # Draw the hook.
    pen.move_to(left_corner)
    pen.line_to(right_corner)
    pen.turn_to(right_start_heading)
    pen.arc_to(tip)
    right_arc = pen.last_segment()
    pen.turn_to(left_end_heading + 180)
    pen.arc_to(left_corner)
    left_arc = pen.last_segment()

    # Figure out how wide the arc is.
    slant_angle = (start_heading - base_heading)
    if slant_angle > 180:
        slant_angle -= 360
    hook_deviation = abs(slant_angle) - 90
    right_arc_behind = hook_deviation < 0

    ##DEBUG
    if not right_arc_behind:
        pen.stroke_mode(0.03, '#faa')
    else:
        pen.stroke_mode(0.03, '#aaa')
    pen.move_to(left_arc.center)
    pen.circle(0.2)
    pen.circle(left_arc.radius)

    if right_arc_behind:
        pen.stroke_mode(0.03, '#faa')
    else:
        pen.stroke_mode(0.03, '#aaa')
    pen.move_to(right_arc.center)
    pen.circle(0.2)
    pen.circle(right_arc.radius)
    ##

    if right_arc_behind:
        c = left_arc.center
        a = right_corner
        other_arc = left_arc
    else:
        c = right_arc.center
        a = left_corner
        other_arc = right_arc

    points = intersect_circle_line(other_arc.center, other_arc.radius, a, c)
    b = closest_point_to(a, points)
    pen.stroke_mode(0.03, '#faa')
    pen.move_to(a)
    pen.line_to(b)

    pen.set_mode(old_mode)


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
        hook(pen, -90, -15, 2.5)



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
        hook(pen, -30, -105, 2.5)


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
            hook(pen, 45, -30, 2.5)
        elif self.character.bottom_slanted():
            pen.line_to_y(BOTTOM, end_slant=0)
            pen.turn_to(0)
            pen.move_forward(pen.last_slant_width() / 2)
            hook(pen, 75, 0, 2.5)


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
