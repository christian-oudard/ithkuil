from common import (
    WIDTH,
    TOP,
    MIDDLE,
    BOTTOM,
    bevel_distance,
    slant45,
    slant60,
    flip_consonant_horizontal,
    flip_ending_horizontal,
    Character,
)


class ConsonantCharacter(Character):
    bottom_type = NotImplemented # 'straight' or 'slanted'
    bottom_orientation = NotImplemented # 'left' or 'right'
    side_flipped = NotImplemented # True for 45 angle, False for -45 angle.

    def __init__(self, side_ending_class, bottom_ending_class):
        self.side_ending = side_ending_class(self)
        if self.bottom_slanted() and self.bottom_orientation == 'left':
            bottom_ending_class = flip_ending_horizontal(bottom_ending_class)
        self.bottom_ending = bottom_ending_class(self)

    def bottom_straight(self):
        return self.bottom_type == 'straight'

    def bottom_slanted(self):
        return self.bottom_type == 'slanted'

    def draw_character(self, pen):
        endpoint = (0, TOP - WIDTH / 2)
        pen.move_to(endpoint)
        self.side_ending.draw(pen)
        pen.move_to(endpoint)
        self.draw(pen)
        self.bottom_ending.draw(pen)
        pen.paper.center_on_x(0)


class ConsP(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            4.5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
        )

        pen.turn_to(-45)
        pen.stroke_to_y(MIDDLE + WIDTH)
        pen.turn_to(-90)

        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsT(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(5.5 + self.side_ending.offset_x(), start_angle=self.side_ending.angle())
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsK(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'right'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(5.5 + self.side_ending.offset_x(), start_angle=self.side_ending.angle())
        pen.turn_to(-65)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsQ(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(5.5 + self.side_ending.offset_x(), start_angle=self.side_ending.angle())
        pen.turn_to(-45)
        pen.stroke_to_y(MIDDLE, end_angle=0)
        pen.turn_to(180)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant60 / 2)
        pen.turn_left(60)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            start_angle=0,
            end_angle=self.bottom_ending.angle(),
        )


class ConsC(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(6 + self.side_ending.offset_x(), start_angle=self.side_ending.angle())
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE + WIDTH / 2)
        pen.turn_to(0)
        pen.stroke_forward(3)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsCHacek(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(6 + self.side_ending.offset_x(), start_angle=self.side_ending.angle())
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE + WIDTH / 2)
        pen.turn_to(0)
        pen.stroke_forward(4)
        pen.turn_to(-135)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsL(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'right'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(5.5 + self.side_ending.offset_x(), start_angle=self.side_ending.angle())
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE)
        pen.turn_to(-45)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsL(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'right'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(5.5 + self.side_ending.offset_x(), start_angle=self.side_ending.angle())
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE)
        pen.turn_to(-45)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsH(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            4 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
            end_angle=45,
        )
        pen.turn_left(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH / 2)
        pen.turn_to(-45)
        pen.stroke_to_y(MIDDLE)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsPStop(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            4.5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
        )
        pen.turn_to(-60)
        pen.stroke_to_y(MIDDLE - pen.last_slant_width() / slant45 / 2, end_angle=45)
        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            start_angle=45,
            end_angle=self.bottom_ending.angle(),
        )


class ConsTStop(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = True
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
            end_angle=-45,
        )
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            start_angle=-45,
            end_angle=self.bottom_ending.angle(),
        )


class ConsKStop(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = True
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
            end_angle=-45,
        )
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(180)
        pen.stroke_forward(WIDTH / 2 + bevel_distance / 2, start_angle=-45)
        pen.turn_left(45)
        pen.stroke_forward(bevel_distance)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsQStop(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'right'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
        )
        pen.turn_to(-60)
        pen.stroke_to_y(MIDDLE, end_angle=0)
        pen.turn_to(180)
        pen.move_forward(WIDTH * slant60)
        pen.turn_to(-60)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            start_angle=0,
            end_angle=self.bottom_ending.angle(),
        )


class ConsCStop(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'right'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
        )
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE + WIDTH / 2, end_angle=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH * slant75 / 2)
        pen.turn_to(-60)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            start_angle=45,
            end_angle=self.bottom_ending.angle(),
        )


class ConsCHacekStop(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'left'
    side_flipped = True
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
            end_angle=-45,
        )
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE - WIDTH / 2, start_angle=-45)
        pen.turn_to(0)
        pen.stroke_forward(2)
        pen.turn_to(-135)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            start_angle=-45,
            end_angle=self.bottom_ending.angle(),
        )


class ConsF(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = True
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            4.5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
        )
        pen.turn_to(-45)
        pen.stroke_to_y(TOP - 2 * WIDTH, end_angle=0)
        pen.turn_to(180)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            start_angle=0,
            end_angle=self.bottom_ending.angle(),
        )


class ConsTCedilla(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
        )
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE - WIDTH / 2, end_angle=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            start_angle=45,
            end_angle=self.bottom_ending.angle(),
        )


class ConsX(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5.5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
        )
        pen.turn_to(-60)
        pen.stroke_to_y(MIDDLE, end_angle=0)
        pen.turn_to(0)
        pen.move_forward(WIDTH * slant60)
        pen.turn_to(-120)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            start_angle=0,
            end_angle=self.bottom_ending.angle(),
        )


class ConsS(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
        )
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE - WIDTH / 2, end_angle=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(0)
        pen.stroke_forward(WIDTH / 2 + 1 + bevel_distance / 2, start_angle=45)
        pen.turn_right(45)
        pen.stroke_forward(bevel_distance)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsSHacek(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = True
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
            end_angle=-45,
        )
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE - WIDTH / 2, start_angle=-45)
        pen.turn_to(0)
        pen.stroke_forward(2)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            start_angle=-45,
            end_angle=self.bottom_ending.angle(),
        )


class ConsR(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5.5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
        )
        pen.turn_to(-45)
        pen.stroke_to_y(MIDDLE + WIDTH / 2)
        pen.turn_to(180)
        pen.stroke_forward(3.5)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsW(ConsonantCharacter):
    bottom_type = 'straight'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            4.5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
            end_angle=45,
        )
        pen.turn_left(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
            start_angle=45,
        )


class ConsL(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'right'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
        )
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE)
        pen.turn_to(-45)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsM(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'left'
    side_flipped = False
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5.5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
        )
        pen.turn_to(-45)
        pen.stroke_to_y(MIDDLE)
        pen.turn_to(-120)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


class ConsNHacek(ConsonantCharacter):
    bottom_type = 'slanted'
    bottom_orientation = 'right'
    side_flipped = True
    def draw(self, pen):
        pen.turn_to(180)
        pen.stroke_forward(
            5 + self.side_ending.offset_x(),
            start_angle=self.side_ending.angle(),
            end_angle=-45,
        )
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.stroke_to_y(MIDDLE - WIDTH / 2, start_angle=-45)
        pen.turn_to(-45)
        pen.stroke_to_y(
            BOTTOM + self.bottom_ending.offset_y(pen),
            end_angle=self.bottom_ending.angle(),
        )


ConsB = flip_consonant_horizontal(ConsP)
ConsD = flip_consonant_horizontal(ConsT)
ConsG = flip_consonant_horizontal(ConsK)
ConsRHacek = flip_consonant_horizontal(ConsQ)
ConsZDot = flip_consonant_horizontal(ConsC)
ConsJ = flip_consonant_horizontal(ConsCHacek)
ConsStop = flip_consonant_horizontal(ConsH)
ConsPH = flip_consonant_horizontal(ConsPStop)
ConsTH = flip_consonant_horizontal(ConsTStop)
ConsKH = flip_consonant_horizontal(ConsKStop)
ConsQH = flip_consonant_horizontal(ConsQStop)
ConsCH = flip_consonant_horizontal(ConsCStop)
ConsCHacekH = flip_consonant_horizontal(ConsCHacekStop)
ConsV = flip_consonant_horizontal(ConsF)
ConsDh = flip_consonant_horizontal(ConsTCedilla)
ConsXh = flip_consonant_horizontal(ConsX)
ConsZ = flip_consonant_horizontal(ConsS)
ConsZHacek = flip_consonant_horizontal(ConsSHacek)
ConsY = flip_consonant_horizontal(ConsW)
ConsLCedilla = flip_consonant_horizontal(ConsL)
ConsCCedilla = flip_consonant_horizontal(ConsR)
ConsN = flip_consonant_horizontal(ConsM)
ConsTLCedilla = flip_consonant_horizontal(ConsNHacek)


consonants = [
    ConsP,
    ConsT,
    ConsK,
    ConsQ,
    ConsC,
    ConsCHacek,
    ConsB,
    ConsD,
    ConsG,
    ConsStop,
    ConsZDot,
    ConsJ,
    ConsPStop,
    ConsTStop,
    ConsKStop,
    ConsQStop,
    ConsCStop,
    ConsCHacekStop,
    ConsPH,
    ConsTH,
    ConsKH,
    ConsQH,
    ConsCH,
    ConsCHacekH,
    ConsF,
    ConsTCedilla,
    ConsX,
    ConsXh,
    ConsS,
    ConsSHacek,
    ConsV,
    ConsDh,
    ConsH,
    ConsRHacek,
    ConsZ,
    ConsZHacek,
    ConsW,
    ConsL,
    ConsY,
    ConsLCedilla,
    ConsR,
    ConsCCedilla,
    ConsM,
    ConsN,
    ConsNHacek,
    ConsTLCedilla,
]
