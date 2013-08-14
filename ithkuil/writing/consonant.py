#TODO: refactor into type={straight, left, right}
from .common import (
    Character,
    WIDTH,
    TOP,
    MIDDLE,
    BOTTOM,
    bevel_distance,
    slant45,
    slant60,
    slant75,
    flip_consonant_horizontal as flip,
    flip_ending_horizontal,
)
from canoepaddle import Pen, Paper


def flip_angle_x(angle):
    return 180 - angle


class ConsonantCharacter(Character):

    bottom_type = NotImplemented  # 'straight' or 'slanted'
    bottom_orientation = NotImplemented  # 'left' or 'right'
    side_flipped = False
    bottom_flipped = False

    def __init__(self, side_ending_class, bottom_ending_class):
        self.side_ending = side_ending_class(self)
        if self.bottom_slanted() and self.bottom_orientation == 'left':
            bottom_ending_class = flip_ending_horizontal(bottom_ending_class)
        self.bottom_ending = bottom_ending_class(self)

    def __str__(self):
        return 'consonant.{}({}, {})'.format(
            self.__class__.__name__,
            self.side_ending,
            self.bottom_ending,
        )

    def bottom_straight(self):
        return self.bottom_type == 'straight'

    def bottom_slanted(self):
        return self.bottom_type == 'slanted'

    def draw_character(self, mode):
        paper = Paper()

        # When drawing the body of the consonant, subclasses will start
        # where the side ending is, and end where the bottom ending is.
        pen = Pen()
        pen.set_mode(mode)
        pen.move_to((0, TOP - WIDTH / 2))
        side_ending_position = pen.position
        self.draw(pen)
        bottom_ending_position = pen.position
        bottom_ending_heading = pen.heading
        paper.merge(pen.paper)

        # Draw the side ending.
        pen = Pen()
        pen.set_mode(mode)
        pen.move_to(side_ending_position)
        pen.turn_to(0)
        self.side_ending.draw(pen)
        paper.merge(pen.paper)

        # Draw the bottom ending.
        pen = Pen()
        pen.set_mode(mode)
        pen.move_to(bottom_ending_position)

        # If the bottom orientation is slanted left, then we have to
        # start the bottom ending from a flipped heading so when it flips
        # again later it will be correct.
        if (
            self.bottom_type == 'slanted' and
            self.bottom_orientation == 'left'
        ):
            bottom_ending_heading = flip_angle_x(bottom_ending_heading)
        pen.turn_to(bottom_ending_heading)

        # Draw the ending, and maybe flip it horizontally.
        self.bottom_ending.draw(pen)
        if self.bottom_orientation == 'left':
            pen.paper.mirror_x(bottom_ending_position.x)
        paper.merge(pen.paper)

        paper.join_paths()
        paper.fuse_paths()

        return paper


class P(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)

        pen.turn_to(-45)
        pen.line_to_y(MIDDLE + WIDTH)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE)


class T(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_left(90)
        pen.line_to_y(MIDDLE)


class K(ConsonantCharacter):

    bottom_type = 'slanted'
    bottom_orientation = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.5)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE)


class Q(ConsonantCharacter):

    bottom_type = 'slanted'
    bottom_orientation = 'left'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.5)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE, end_angle=0)
        pen.turn_to(180)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant60 / 2)
        pen.turn_left(60)
        pen.line_forward(WIDTH, start_angle=0)


class C(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(5)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE + WIDTH / 2)
        pen.turn_to(0)
        pen.line_forward(3)
        pen.turn_to(-90)
        pen.line_forward(WIDTH)


class CHacek(ConsonantCharacter):

    bottom_type = 'slanted'
    bottom_orientation = 'left'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(5)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE + WIDTH / 2)
        pen.turn_to(0)
        pen.line_forward(4)
        pen.turn_to(-135)
        pen.line_forward(1.5 * WIDTH)


class L(ConsonantCharacter):

    bottom_type = 'slanted'
    bottom_orientation = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE)
        pen.turn_to(-45)
        pen.line_forward(WIDTH)


class H(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3, end_angle=45)
        pen.turn_left(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH / 2)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE)
        pen.turn_to(-90)
        pen.line_forward(WIDTH)


class PStop(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE - pen.last_slant_width() / slant45 / 2, end_angle=45)
        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
        pen.turn_to(-90)
        pen.line_forward(WIDTH, start_angle=45)


class TStop(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'
    side_flipped = True
    bottom_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_angle=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_angle=-45)


class KStop(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_angle=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(180)
        pen.line_forward(WIDTH / 2, start_angle=-45)
        pen.arc_left(90, WIDTH / 2)
        pen.line_to_y(MIDDLE)


class QStop(ConsonantCharacter):

    bottom_type = 'slanted'
    bottom_orientation = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE, end_angle=0)
        pen.turn_to(180)
        pen.move_forward(WIDTH * slant60)
        pen.turn_to(-60)
        pen.line_forward(WIDTH, start_angle=0)


class CStop(ConsonantCharacter):

    bottom_type = 'slanted'
    bottom_orientation = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE + WIDTH / 2, end_angle=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH * slant75 / 2)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE, start_angle=45)


class CHacekStop(ConsonantCharacter):

    bottom_type = 'slanted'
    bottom_orientation = 'left'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_angle=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - WIDTH / 2, start_angle=-45)
        pen.turn_to(0)
        pen.line_forward(2.5)
        pen.turn_to(-135)
        pen.line_forward(1.5 * WIDTH)


class F(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-45)
        pen.line_to_y(TOP - 2 * WIDTH, end_angle=0)
        pen.turn_to(180)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_angle=0)


class TCedilla(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - WIDTH / 2, end_angle=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_forward(WIDTH, start_angle=45)


class X(ConsonantCharacter):

    bottom_type = 'slanted'
    bottom_orientation = 'left'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.5)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE, end_angle=0)
        pen.turn_to(0)
        pen.move_forward(WIDTH * slant60)
        pen.turn_to(-120)
        pen.line_forward(WIDTH, start_angle=0)


class S(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - WIDTH / 2, end_angle=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(0)
        pen.line_forward(1.5 * WIDTH, start_angle=45)
        pen.arc_right(90, WIDTH / 2)
        pen.line_forward(WIDTH)


class SHacek(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_angle=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - WIDTH / 2, start_angle=-45)
        pen.turn_to(0)
        pen.line_forward(2)
        pen.turn_to(-90)
        pen.line_forward(WIDTH)


class R(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.5)
        a = pen.position
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE + WIDTH / 2)
        pen.turn_to(180)
        # Line up the left point of the top bar with the side of the
        # down stroke.
        pen.line_to_x(a.x - WIDTH * slant60 + WIDTH / 2)
        pen.turn_to(-90)
        pen.line_forward(WIDTH)


class W(ConsonantCharacter):

    bottom_type = 'straight'
    bottom_orientation = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5, end_angle=45)
        pen.turn_left(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_angle=45)


class M(ConsonantCharacter):

    bottom_type = 'slanted'
    bottom_orientation = 'left'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.5)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE)
        pen.turn_to(-120)
        pen.line_forward(WIDTH)


class NHacek(ConsonantCharacter):

    bottom_type = 'slanted'
    bottom_orientation = 'right'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_angle=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - WIDTH / 2, start_angle=-45)
        pen.turn_to(-45)
        pen.line_forward(WIDTH)


B = flip(P)
D = flip(T)
G = flip(K)
RHacek = flip(Q)
ZDot = flip(C)
J = flip(CHacek)
Stop = flip(H)
PH = flip(PStop)
TH = flip(TStop)
KH = flip(KStop)
QH = flip(QStop)
CH = flip(CStop)
CHacekH = flip(CHacekStop)
V = flip(F)
Dh = flip(TCedilla)
Xh = flip(X)
Z = flip(S)
ZHacek = flip(SHacek)
Y = flip(W)
LCedilla = flip(L)
CCedilla = flip(R)
N = flip(M)
TLCedilla = flip(NHacek)


consonants = [
    P,
    T,
    K,
    Q,
    C,
    CHacek,
    B,
    D,
    G,
    Stop,
    ZDot,
    J,
    PStop,
    TStop,
    KStop,
    QStop,
    CStop,
    CHacekStop,
    PH,
    TH,
    KH,
    QH,
    CH,
    CHacekH,
    F,
    TCedilla,
    X,
    Xh,
    S,
    SHacek,
    V,
    Dh,
    H,
    RHacek,
    Z,
    ZHacek,
    W,
    L,
    Y,
    LCedilla,
    R,
    CCedilla,
    M,
    N,
    NHacek,
    TLCedilla,
]
