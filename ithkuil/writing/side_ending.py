from .common import Ending, WIDTH, TOP, slant45, slant75
from .util import hook


class SideEnding(Ending):
    def offset_x(self):
        return 0

    def __str__(self):
        return 'side_ending.{}'.format(self.__class__.__name__)


class Normal(SideEnding):
    # Unframed Relation, Pattern 1, Stem 1
    def draw(self, pen):
        if self.character.side_flipped:
            pen.line_forward(WIDTH, end_slant=-45)
        else:
            pen.line_forward(WIDTH, end_slant=45)


class RightOnBottom(SideEnding):
    # Unframed Relation, Pattern 1, Stem 2
    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(0)
        pen.line_forward(2, start_slant=45, end_slant=45)


class DownOnBottom(SideEnding):
    # Unframed Relation, Pattern 1, Stem 3
    def draw(self, pen):
        pen.line_forward(2 * WIDTH, end_slant=45)
        pen.break_stroke()  # TEMP
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_forward(2, start_slant=45, end_slant=45)


class HookRightOnBottom(SideEnding):
    # Unframed Relation, Pattern 2, Stem 1
    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45 / 2)
        hook(pen, 120, -30, 2.5, adjust_inside=15)


class HookLeftOnBottom(SideEnding):
    # Unframed Relation, Pattern 2, Stem 2
    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45 / 2)
        hook(pen, 90, 30, 2.5, adjust_inside=15)


class DiagonalDownRightOnBottom(SideEnding):
    # Unframed Relation, Pattern 2, Stem 3
    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(-45)
        pen.line_forward(2, start_slant=45, end_slant=90)


class FoldDownHookRight(SideEnding):
    # Unframed Relation, Pattern 3, Stem 1
    def draw(self, pen):
        pen.line_forward(2 * WIDTH, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45 / 2)
        hook(pen, -90, -30, 2.5, adjust_inside=15)


class FoldDownHookLeft(SideEnding):
    # Unframed Relation, Pattern 3, Stem 2
    def draw(self, pen):
        pen.line_forward(2 * WIDTH, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45 / 2)
        hook(pen, -105, 30, 2.5, adjust_inside=15)


class DiagonalDownLeft(SideEnding):
    # Unframed Relation, Pattern 3, Stem 3
    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(-135)
        pen.line_forward(2, start_slant=-45, end_slant=90)


class DownOnRight(SideEnding):
    # Framed Relation, Pattern 1, Stem 1
    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_forward(2.5, start_slant=45, end_slant=45)


class DiagonalDownRightOnTop(SideEnding):
    # Framed Relation, Pattern 1, Stem 2
    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(-45)
        pen.line_to_y(TOP - WIDTH, start_slant=45, end_slant=0)


class FoldUp(SideEnding):
    # Framed Relation, Pattern 1, Stem 3
    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(90)
        pen.line_forward(2, start_slant=45, end_slant=45)


class UpOnRight(SideEnding):
    # Framed Relation, Pattern 2, Stem 1
    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(90)
        pen.line_forward(2.5, start_slant=-45, end_slant=-45)


class DiagonalUpRightOnTop(SideEnding):
    # Framed Relation, Pattern 2, Stem 2
    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(45)
        pen.line_to_y(TOP, start_slant=-45, end_slant=0)


class FoldDown(SideEnding):
    # Framed Relation, Pattern 2, Stem 3
    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_forward(2, start_slant=-45, end_slant=-45)


class FoldUpHookLeft(SideEnding):
    # Framed Relation, Pattern 3, Stem 1
    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2)
        hook(pen, 90, 30, 2.5, adjust_inside=15)


class FoldUpHookRight(SideEnding):
    # Framed Relation, Pattern 3, Stem 2
    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2)
        hook(pen, 105, -30, 2.5, adjust_inside=15)


class DiagonalUpLeft(SideEnding):
    # Framed Relation, Pattern 3, Stem 3
    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(135)
        pen.line_forward(2, start_slant=45, end_slant=0)


side_endings = [
    Normal,
    RightOnBottom,
    DownOnBottom,
    HookRightOnBottom,
    HookLeftOnBottom,
    DiagonalDownRightOnBottom,
    FoldDownHookRight,
    FoldDownHookLeft,
    DiagonalDownLeft,
    DownOnRight,
    DiagonalDownRightOnTop,
    FoldUp,
    UpOnRight,
    DiagonalUpRightOnTop,
    FoldDown,
    FoldUpHookLeft,
    FoldUpHookRight,
    DiagonalUpLeft,
]


class SideAll(SideEnding):
    """
    A debug ending showing every one of the endings superimposed.
    """
    def angle(self):
        return None

    def draw(self, pen):
        start_position = pen.position
        for side_ending_class in side_endings:
            if side_ending_class == SideAll:
                continue
            side_ending = side_ending_class(self.character)
            pen.move_to(start_position)
            pen.turn_to(0)
            pen.move_forward(side_ending.offset_x())
            side_ending.draw(pen)
        pen.move_to(start_position)
