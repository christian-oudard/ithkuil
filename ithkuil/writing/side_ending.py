from .common import Ending, WIDTH, HOOK_BASE_WIDTH, TOP, slant45, slant75 

class SideEnding(Ending):
    def offset_x(self):
        return 0

    def __str__(self):
        return 'side_ending.{}'.format(self.__class__.__name__)


class Normal(SideEnding):
    # Unframed Relation, Pattern 1, Stem 1
    def angle(self):
        if self.character.side_flipped:
            return -45
        else:
            return 45

    def offset_x(self):
        return 0

    def draw(self, pen):
        pen.line_forward(WIDTH / 2, end_angle=self.angle())


class RightOnBottom(SideEnding):
    # Unframed Relation, Pattern 1, Stem 2
    def angle(self):
        return 45

    def offset_x(self):
        return 0

    def draw(self, pen):
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(0)
        pen.line_forward(2, start_angle=45, end_angle=45)


class DownOnBottom(SideEnding):
    # Unframed Relation, Pattern 1, Stem 3
    def angle(self):
        return 45

    def offset_x(self):
        return +1

    def draw(self, pen):
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_forward(2, start_angle=45, end_angle=45)


class CurveDownOnBottom(SideEnding):
    # Unframed Relation, Pattern 2, Stem 1
    def angle(self):
        return 45

    def offset_x(self):
        return 0

    def draw(self, pen):
        old_mode = pen.mode
        pen.set_mode(old_mode.outliner_mode())

        # Mark top of hook base.
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45 / 2)
        a = pen.position

        # Mark tip of hook.
        pen.move_relative((2 * WIDTH, -2 * WIDTH))
        tip = pen.position

        # Mark the correct heading to leave the hook at.
        pen.move_to(a)
        pen.turn_to(0)
        pen.arc_to(tip)
        heading = pen.heading
        pen.undo()

        # Draw the hook.
        pen.move_to(a)
        pen.turn_to(-135)
        pen.line_forward(HOOK_BASE_WIDTH)
        pen.turn_to(0)
        pen.arc_to(tip)
        pen.turn_to(heading + 180)
        pen.arc_to(a)

        pen.set_mode(old_mode)


class CurveUpOnBottom(SideEnding):
    # Unframed Relation, Pattern 2, Stem 2
    def angle(self):
        return 45

    def offset_x(self):
        return 0

    def draw(self, pen):
        old_mode = pen.mode
        pen.set_mode(old_mode.outliner_mode())

        # Mark top of hook base.
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45 / 2)
        a = pen.position

        # Mark tip of hook.
        pen.line_forward(HOOK_BASE_WIDTH)
        pen.move_relative((3 * WIDTH, 0))
        tip = pen.position

        # Mark the correct heading to leave the hook at.
        pen.move_to(a)
        pen.turn_to(-45)
        pen.arc_to(tip)
        heading = pen.heading
        pen.undo()

        # Draw the hook.
        pen.move_to(a)
        pen.turn_to(-135)
        pen.line_forward(HOOK_BASE_WIDTH)
        pen.turn_to(-45)
        pen.arc_to(tip)
        pen.turn_to(heading + 180)
        pen.arc_to(a)

        pen.set_mode(old_mode)

class DiagonalDownRightOnBottom(SideEnding):
    # Unframed Relation, Pattern 2, Stem 3
    def angle(self):
        return 45

    def offset_x(self):
        return 0

    def draw(self, pen):
        pen.turn_to(-135)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(-45)
        pen.line_forward(2, start_angle=45, end_angle=90)


class FoldDownCurveUp(SideEnding):
    # Unframed Relation, Pattern 3, Stem 1
    def angle(self):
        return -45

    def offset_x(self):
        return +1

    def draw(self, pen):
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH * slant75 / 2)
        pen.turn_to(-120)
        pen.line_forward(1, start_angle=-45)
        pen.turn_right(30)
        pen.stroke_mode((3 / 4) * WIDTH)
        pen.line_forward(1.25, end_angle=0)
        pen.stroke_mode(WIDTH)


class FoldDownCurveDown(SideEnding):
    # Unframed Relation, Pattern 3, Stem 2
    def angle(self):
        return -45

    def offset_x(self):
        return +1

    def draw(self, pen):
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH * slant75 / 2)
        pen.turn_to(-150)
        pen.line_forward(1, start_angle=-45)
        pen.turn_left(30)
        pen.stroke_mode((3 / 4) * WIDTH)
        pen.line_forward(1.25, end_angle=90)
        pen.stroke_mode(WIDTH)


class DiagonalDownLeft(SideEnding):
    # Unframed Relation, Pattern 3, Stem 3
    def angle(self):
        return -45

    def offset_x(self):
        return 0

    def draw(self, pen):
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(-135)
        pen.line_forward(2, start_angle=-45, end_angle=90)


class DownOnRight(SideEnding):
    # Framed Relation, Pattern 1, Stem 1
    def angle(self):
        return 45

    def offset_x(self):
        return -0.5

    def draw(self, pen):
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_forward(2.5, start_angle=45, end_angle=45)


class DiagonalDownRightOnTop(SideEnding):
    # Framed Relation, Pattern 1, Stem 2
    def angle(self):
        return 45

    def offset_x(self):
        return -0.5

    def draw(self, pen):
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(-45)
        pen.line_to_y(TOP - WIDTH, start_angle=45, end_angle=0)


class FoldUp(SideEnding):
    # Framed Relation, Pattern 1, Stem 3
    def angle(self):
        return 45

    def offset_x(self):
        return -0.5

    def draw(self, pen):
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(90)
        pen.line_forward(2, start_angle=45, end_angle=45)


class UpOnRight(SideEnding):
    # Framed Relation, Pattern 2, Stem 1
    def angle(self):
        return -45

    def offset_x(self):
        return -0.5

    def draw(self, pen):
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(90)
        pen.line_forward(2.5, start_angle=-45, end_angle=-45)


class DiagonalUpRightOnTop(SideEnding):
    # Framed Relation, Pattern 2, Stem 2
    def angle(self):
        return -45

    def offset_x(self):
        return -0.5

    def draw(self, pen):
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(45)
        pen.line_to_y(TOP, start_angle=-45, end_angle=0)


class FoldDown(SideEnding):
    # Framed Relation, Pattern 2, Stem 3
    def angle(self):
        return -45

    def offset_x(self):
        return -0.5

    def draw(self, pen):
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_forward(2, start_angle=-45, end_angle=-45)


class FoldUpCurveDown(SideEnding):
    # Framed Relation, Pattern 3, Stem 1
    def angle(self):
        return 45

    def offset_x(self):
        return 0

    def draw(self, pen):
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH * slant75 / 2)
        pen.turn_to(120)
        pen.line_forward(1, start_angle=45)
        pen.turn_left(30)
        pen.stroke_mode((3 / 4) * WIDTH)
        pen.line_forward(1.25, end_angle=0)
        pen.stroke_mode(WIDTH)


class FoldUpCurveUp(SideEnding):
    # Framed Relation, Pattern 3, Stem 2
    def angle(self):
        return 45

    def offset_x(self):
        return 0

    def draw(self, pen):
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH * slant75 / 2)
        pen.turn_to(150)
        pen.line_forward(1, start_angle=45)
        pen.turn_right(30)
        pen.stroke_mode((3 / 4) * WIDTH)
        pen.line_forward(1.25, end_angle=90)
        pen.stroke_mode(WIDTH)


class DiagonalUpLeft(SideEnding):
    # Framed Relation, Pattern 3, Stem 3
    def angle(self):
        return 45

    def offset_x(self):
        return 0

    def draw(self, pen):
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(135)
        pen.line_forward(2, start_angle=45, end_angle=90)


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
            before_position = pen.position
            side_ending.draw(pen)
        pen.move_to(start_position)


side_endings = [
    Normal,
    #SideAll, # DEBUG
    RightOnBottom,
    DownOnBottom,
    CurveDownOnBottom,
    CurveUpOnBottom,
    DiagonalDownRightOnBottom,
    FoldDownCurveUp,
    FoldDownCurveDown,
    DiagonalDownLeft,
    DownOnRight,
    DiagonalDownRightOnTop,
    FoldUp,
    UpOnRight,
    DiagonalUpRightOnTop,
    FoldDown,
    FoldUpCurveDown,
    FoldUpCurveUp,
    DiagonalUpLeft,
]
