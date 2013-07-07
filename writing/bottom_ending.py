from .common import Ending, WIDTH, BOTTOM, UNDER, slant60, slant45

# A small amount added to some offsets to prevent illegal line crossings due to
# round off error.
smidgen = 0.001


class BottomEnding(Ending):
    def offset_y(self, pen):
        return 0


class Normal(BottomEnding):
    def angle(self):
        if self.character.bottom_straight():
            return 45
        elif self.character.bottom_slanted():
            return 0


class Long(BottomEnding):
    # Consonant Prefix L
    def offset_y(self, pen):
        return UNDER - BOTTOM
    angle = Normal.angle


class DiagonalDownRightOnRight(BottomEnding):
    # Consonant Prefix M
    def angle(self):
        return 45
    def offset_y(self, pen):
        return +WIDTH + smidgen

    def draw(self, pen):
        pen.stroke_to_y(
            BOTTOM + pen.last_slant_width() / slant45 / 2,
            end_angle=45,
        )
        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH / 2)
        pen.turn_to(-45)
        pen.stroke_to_y(BOTTOM, end_angle=0)


class DownOnRight(BottomEnding):
    # Consonant Prefix R
    def angle(self):
        return 45

    def offset_y(self, pen):
        return +WIDTH / 2

    def draw(self, pen):
        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
        pen.turn_to(-90)
        pen.stroke_forward(2, start_angle=45, end_angle=45)


class RightOnRight(BottomEnding):
    # Consonant Prefix L Cedilla
    def angle(self):
        return 45

    def offset_y(self, pen):
        return +WIDTH

    def draw(self, pen):
        pen.stroke_to_y(
            BOTTOM - pen.last_slant_width() / slant45 / 2,
            end_angle=45,
        )
        pen.turn_to(45)
        pen.move_to_y(BOTTOM + WIDTH / 2)
        pen.turn_to(0)
        pen.stroke_forward(2, start_angle=45, end_angle=45)


class DiagonalDownLeftOnRight(BottomEnding):
    # Consonant Prefix N
    def angle(self):
        return 0

    def offset_y(self, pen):
        return 0

    def draw(self, pen):
        pen.turn_to(0)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
        pen.turn_to(-135)
        pen.stroke_forward(2, start_angle=0, end_angle=0)


class Bend(BottomEnding):
    # Consonant Prefix S
    def offset_y(self, pen):
        if self.character.bottom_straight():
            return +WIDTH / 2
        elif self.character.bottom_slanted():
            return 0

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.turn_to(0)
            pen.stroke_forward(2.5, end_angle=-45)
        elif self.character.bottom_slanted():
            pen.turn_to(-90)
            pen.stroke_forward(2, end_angle=45)


class Fold(BottomEnding):
    # Consonant Prefix S hacek
    # Consonant Prefix Z hacek
    def angle(self):
        if self.character.bottom_straight():
            return -45
        elif self.character.bottom_slanted():
            return 45

    def offset_y(self, pen):
        if self.character.bottom_straight():
            return +WIDTH / 2
        elif self.character.bottom_slanted():
            return +WIDTH + smidgen

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.turn_to(-45)
            pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
            pen.turn_to(0)
            pen.stroke_forward(2, start_angle=-45, end_angle=-45)
        elif self.character.bottom_slanted():
            pen.stroke_to_y(
                BOTTOM + pen.last_slant_width() / slant45 / 2,
                end_angle=45,
            )
            pen.turn_to(-135)
            pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
            pen.turn_to(180)
            pen.stroke_forward(2, start_angle=45, end_angle=45)


class Barb(BottomEnding):
    # Consonant Prefix N hacek
    angle = Normal.angle

    def offset_y(self, pen):
        return +WIDTH / 4

    def draw(self, pen):
        pen.set_width(WIDTH / 2)
        if self.character.bottom_straight():
            pen.turn_to(45)
            pen.stroke_forward(WIDTH * 1.5, end_angle=20)
        elif self.character.bottom_slanted():
            pen.turn_to(180)
            pen.stroke_forward(WIDTH * 1.5, end_angle=25)
        pen.set_width(WIDTH)


class DiagonalUpRight(BottomEnding):
    # Consonant Prefix Z
    def angle(self):
        if self.character.bottom_straight():
            return -45
        elif self.character.bottom_slanted():
            return 90

    def offset_y(self, pen):
        if self.character.bottom_straight():
            return +WIDTH / 2
        elif self.character.bottom_slanted():
            return 0

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.turn_to(-45)
            pen.move_forward(pen.last_slant_width() / 2 + WIDTH / 2)
            pen.turn_to(45)
            pen.stroke_forward(2, start_angle=-45, end_angle=0)
        elif self.character.bottom_slanted():
            pen.stroke_to_y(
                BOTTOM - pen.last_slant_width() / 2,
                end_angle=90
            )
            pen.turn_to(90)
            pen.move_to_y(WIDTH * slant60 / 2)
            pen.turn_to(30)
            pen.stroke_forward(2, start_angle=90, end_angle=90)


class Acute(BottomEnding):
    # Consonant Prefix R Hacek
    def offset_y(self, pen):
        if self.character.bottom_straight():
            return +WIDTH / slant60
        elif self.character.bottom_slanted():
            return +WIDTH / 2

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.turn_to(30)
            pen.stroke_forward(2.5, end_angle=90)
        elif self.character.bottom_slanted():
            pen.turn_to(180)
            pen.stroke_forward(3, end_angle=-45)


class RightOnBottom(BottomEnding):
    # Consonant Prefix C Cedilla
    def angle(self):
        return 45

    def offset_y(self, pen):
        return +WIDTH + smidgen

    def draw(self, pen):
        pen.stroke_to_y(
            BOTTOM + pen.last_slant_width() / slant45 / 2,
            end_angle=45,
        )
        pen.turn_to(-135)
        pen.move_to_y(BOTTOM - WIDTH / 2)
        pen.turn_to(0)
        pen.stroke_forward(3, start_angle=45, end_angle=45)


class StraightOnLeft(BottomEnding):
    # Consonant Prefix C
    # Consonant Prefix Z Dot
    def angle(self):
        if self.character.bottom_straight():
            return -45
        elif self.character.bottom_slanted():
            return 0

    def offset_y(self, pen):
        if self.character.bottom_straight():
            return -WIDTH / 2
        elif self.character.bottom_slanted():
            return 0

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.turn_to(135)
            pen.move_forward(WIDTH * slant45)
            pen.turn_to(-90)
            pen.stroke_forward(2, start_angle=-45, end_angle=-45)
        elif self.character.bottom_slanted():
            original_heading = pen.heading
            pen.turn_to(180)
            pen.move_forward(pen.last_slant_width())
            pen.turn_to(original_heading)
            pen.stroke_forward(2, start_angle=0, end_angle=0)


class BreakRightBendLeft(BottomEnding):
    def angle(self):
        if self.character.bottom_straight():
            return 0
        elif self.character.bottom_slanted():
            return 90

    def offset_y(self, pen):
        if self.character.bottom_straight():
            return 0
        elif self.character.bottom_slanted():
            if pen.heading % 360 in [360 - 45, 360 - 135]:
                return +0.75 * WIDTH * slant45 + smidgen
            else:
                return +1.75 * WIDTH + smidgen

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.turn_to(180)
            pen.move_forward(WIDTH / 2 + WIDTH * slant45 / 2)
            pen.turn_to(-45)
            pen.stroke_forward(2, start_angle=0, end_angle=0)
        elif self.character.bottom_slanted():
            pen.stroke_to_y(
                BOTTOM + pen.last_slant_width() / 2,
                end_angle=90
            )
            pen.turn_to(-90)
            pen.move_to_y(BOTTOM - WIDTH * slant60 / 2)
            pen.turn_to(30)
            pen.stroke_forward(2, start_angle=90, end_angle=90)


class BreakRightBendRight(BottomEnding):
    def angle(self):
        if self.character.bottom_straight():
            return -45
        elif self.character.bottom_slanted():
            return 0

    def offset_y(self, pen):
        if self.character.bottom_straight():
            return -WIDTH / 2
        elif self.character.bottom_slanted():
            return 0

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.turn_to(135)
            pen.move_forward(WIDTH * slant45)
            pen.turn_to(180)
            pen.stroke_forward(2, start_angle=-45, end_angle=-45)
        elif self.character.bottom_slanted():
            pen.turn_to(180)
            pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
            pen.turn_to(-135)
            pen.stroke_forward(2, start_angle=0, end_angle=0)


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
            pen.stroke_to_y(bottom_ending.offset_y(pen), end_angle=bottom_ending.angle())
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
]
