from common import Ending, WIDTH, BOTTOM, UNDER, slant60, slant45


class BottomEnding(Ending):
    def offset_y(self, pen):
        return 0


class BottomNormal(BottomEnding):
    def angle(self):
        if self.character.bottom_straight():
            return 45
        elif self.character.bottom_slanted():
            return 0


class BottomLong(BottomEnding):
    # Consonant Prefix L
    def offset_y(self, pen):
        return UNDER - BOTTOM
    angle = BottomNormal.angle


class BottomDiagonalDownRightOnRight(BottomEnding):
    # Consonant Prefix M
    def angle(self):
        return 45
    def offset_y(self, pen):
        return +1.1 * WIDTH

    def draw(self, pen):
        pen.stroke_to_y(
            BOTTOM + pen.last_slant_width() / slant45 / 2,
            end_angle=45,
        )
        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH / 2)
        pen.turn_to(-45)
        pen.stroke_to_y(BOTTOM, end_angle=0)


class BottomDownOnRight(BottomEnding):
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


class BottomRightOnRight(BottomEnding):
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


class BottomDiagonalDownLeftOnRight(BottomEnding):
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


class BottomBend(BottomEnding):
    # Consonant Prefix S
    def offset_y(self, pen):
        if self.character.bottom_straight():
            return +WIDTH / 2
        elif self.character.bottom_slanted():
            return 0

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.turn_to(0)
            pen.stroke_forward(2, end_angle=-45)
        elif self.character.bottom_slanted():
            pen.turn_to(-90)
            pen.stroke_forward(2, end_angle=45)


class BottomFold(BottomEnding):
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
            return +1.1 * WIDTH

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


class BottomBarb(BottomEnding):
    # Consonant Prefix N hacek
    angle = BottomNormal.angle

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


class BottomDiagonalUpRight(BottomEnding):
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


class BottomAcute(BottomEnding):
    # Consonant Prefix R Hacek
    def offset_y(self, pen):
        if self.character.bottom_straight():
            return +WIDTH / slant60
        elif self.character.bottom_slanted():
            return +WIDTH / 2

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.turn_to(30)
            pen.stroke_forward(2, end_angle=90)
        elif self.character.bottom_slanted():
            pen.turn_to(180)
            pen.stroke_forward(2.5, end_angle=-45)


bottom_endings = [
    BottomNormal,
    BottomLong,
    BottomDiagonalDownRightOnRight,
    BottomDownOnRight,
    BottomRightOnRight,
    BottomDiagonalDownLeftOnRight,
    BottomBend,
    BottomFold,
    BottomBarb,
    BottomDiagonalUpRight,
    BottomAcute,
]
