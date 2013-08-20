from .common import Ending, WIDTH, TOP, slant45, slant75
from .util import hook


class SideEnding(Ending):

    pattern = NotImplemented
    stem = NotImplemented
    relation = NotImplemented

    def offset_x(self):
        return 0

    def __str__(self):
        return 'side_ending.{}'.format(self.__class__.__name__)


class Normal(SideEnding):

    pattern = 'P1'
    stem = 'S1'
    relation = 'UNFRAMED'

    def draw(self, pen):
        if self.character.side_flipped:
            pen.line_forward(WIDTH, end_slant=-45)
        else:
            pen.line_forward(WIDTH, end_slant=45)


class RightOnBottom(SideEnding):

    pattern = 'P1'
    stem = 'S2'
    relation = 'UNFRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(0)
        pen.line_forward(2, start_slant=45, end_slant=45)


class DownOnBottom(SideEnding):

    pattern = 'P1'
    stem = 'S3'
    relation = 'UNFRAMED'

    def draw(self, pen):
        pen.line_forward(2 * WIDTH, end_slant=45)
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_forward(2, start_slant=45, end_slant=45)


class HookRightOnBottom(SideEnding):

    pattern = 'P2'
    stem = 'S1'
    relation = 'UNFRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45 / 2)
        hook(pen, 120, -30, 2.5, adjust_inside=15)


class HookLeftOnBottom(SideEnding):

    pattern = 'P2'
    stem = 'S2'
    relation = 'UNFRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45 / 2)
        hook(pen, 90, 30, 2.5, adjust_inside=15)


class DiagonalDownRightOnBottom(SideEnding):

    pattern = 'P2'
    stem = 'S3'
    relation = 'UNFRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(-45)
        pen.line_forward(2, start_slant=45, end_slant=90)


class FoldDownHookRight(SideEnding):

    pattern = 'P3'
    stem = 'S1'
    relation = 'UNFRAMED'

    def draw(self, pen):
        pen.line_forward(2 * WIDTH, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45 / 2)
        hook(pen, -90, -30, 2.5, adjust_inside=15)


class FoldDownHookLeft(SideEnding):

    pattern = 'P3'
    stem = 'S2'
    relation = 'UNFRAMED'

    def draw(self, pen):
        pen.line_forward(2 * WIDTH, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45 / 2)
        hook(pen, -105, 30, 2.5, adjust_inside=15)


class DiagonalDownLeft(SideEnding):

    pattern = 'P3'
    stem = 'S3'
    relation = 'UNFRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(-135)
        pen.line_forward(2, start_slant=-45, end_slant=90)


class DownOnRight(SideEnding):

    pattern = 'P1'
    stem = 'S1'
    relation = 'FRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_forward(2.5, start_slant=45, end_slant=45)


class DiagonalDownRightOnTop(SideEnding):

    pattern = 'P1'
    stem = 'S2'
    relation = 'FRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(-45)
        pen.line_to_y(TOP - WIDTH, start_slant=45, end_slant=0)


class FoldUp(SideEnding):

    pattern = 'P1'
    stem = 'S3'
    relation = 'FRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(90)
        pen.line_forward(2, start_slant=45, end_slant=45)


class UpOnRight(SideEnding):

    pattern = 'P2'
    stem = 'S1'
    relation = 'FRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(90)
        pen.line_forward(2.5, start_slant=-45, end_slant=-45)


class DiagonalUpRightOnTop(SideEnding):

    pattern = 'P2'
    stem = 'S2'
    relation = 'FRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(45)
        pen.line_to_y(TOP, start_slant=-45, end_slant=0)


class FoldDown(SideEnding):

    pattern = 'P2'
    stem = 'S3'
    relation = 'FRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_forward(2, start_slant=-45, end_slant=-45)


class FoldUpHookLeft(SideEnding):

    pattern = 'P3'
    stem = 'S1'
    relation = 'FRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2)
        hook(pen, 90, 30, 2.5, adjust_inside=15)


class FoldUpHookRight(SideEnding):

    pattern = 'P3'
    stem = 'S2'
    relation = 'FRAMED'

    def draw(self, pen):
        pen.line_forward(WIDTH, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2)
        hook(pen, 105, -30, 2.5, adjust_inside=15)


class DiagonalUpLeft(SideEnding):

    pattern = 'P3'
    stem = 'S3'
    relation = 'FRAMED'

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
