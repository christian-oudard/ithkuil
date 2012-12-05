# Constants.

WIDTH = 1.0
HALFWIDTH = WIDTH / 2

LEFT = 0.0
CENTER = 3.0
RIGHT = 6.0

OVER = 8.5
TOP = 7.0
MIDDLE = 3.5
BOTTOM = 0.0
UNDER = -1.5


class ConsonantCharacter:
    bottom_type = NotImplemented # 'straight' or 'slanted'
    bottom_orientation = NotImplemented # 'left' or 'right'
    side_flipped = NotImplemented # True for 45 angle, False for -45 angle.

    def __init__(self, side_ending_class, bottom_ending_class):
        self.side_ending = side_ending_class(self)
        self.bottom_ending = bottom_ending_class(self)

    def bottom_straight(self):
        return self.bottom_type == 'straight'

    def bottom_slanted(self):
        return self.bottom_type == 'slanted'

    def draw(self, pen):
        pass


class ConsT(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.move_to((RIGHT + self.side_ending.offset_x(), TOP))
        pen.turn_to(0)
        self.side_ending.draw(pen)
        pen.stroke_to(
            (LEFT, TOP),
            start_angle=self.side_ending.angle()
        )
        pen.turn_to(270)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(),
            end_angle=self.bottom_ending.angle(),
        )
        self.bottom_ending.draw(pen)


class ConsK(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'right'
    side_flipped = False
    def draw(self, pen):
        pen.move_to((RIGHT + self.side_ending.offset_x(), TOP))
        pen.turn_to(0)
        self.side_ending.draw(pen)
        pen.stroke_to(
            (LEFT, TOP),
            start_angle=self.side_ending.angle()
        )
        pen.turn_toward((CENTER, BOTTOM))
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(),
            end_angle=self.bottom_ending.angle(),
        )
        self.bottom_ending.draw(pen)


class Ending:
    def __init__(self, character):
        self.character = character

    def angle(self):
        return None

    def draw(self, pen):
        return


class BottomEnding(Ending):
    def offset_y(self):
        return 0


class BottomNormal(BottomEnding):
    def angle(self):
        if self.character.bottom_straight():
            return 45
        elif self.character.bottom_slanted():
            return 0


class BottomLong(BottomEnding):
    # Consonant Prefix L
    def offset_y(self):
        return UNDER - BOTTOM
    angle = BottomNormal.angle


class BottomBend(BottomEnding):
    # Consonant Prefix S
    def offset_y(self):
        return +HALFWIDTH

    def draw(self, pen):
        if self.character.bottom_straight():
            pen.turn_to(0)
            pen.stroke_forward(2, end_angle=-45)
        elif self.character.bottom_slanted():
            pen.turn_to(270)
            pen.stroke_forward(2, end_angle=45)


class SideEnding(Ending):
    def offset_x(self):
        return 0


class SideNormal(SideEnding):
    def angle(self):
        if self.character.side_flipped:
            return -45
        else:
            return 45


if __name__ == '__main__':
    from canoepaddle import Pen
    pen = Pen()
    c = ConsT(SideNormal, BottomBend)
    c = ConsK(SideNormal, BottomNormal)
    c = ConsK(SideNormal, BottomLong)
    c = ConsK(SideNormal, BottomBend)
    c.draw(pen)

    path_data = pen.paper.to_svg_path_thick()

    from string import Template
    with open('template.svg') as f:
        t = Template(f.read())
    print(t.substitute(path_data=path_data))
