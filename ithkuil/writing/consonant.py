#TODO: refactor into type={straight, left, right}
from .common import (
    Character,
    WIDTH,
    OVER,
    TOP,
    MIDDLE,
    BOTTOM,
    UNDER,
    bevel_distance,
    slant45,
    slant60,
    slant75,
    flip_consonant_horizontal as flip,
    flip_ending_horizontal,
)
from canoepaddle import Pen, Paper


class ConsonantCharacter(Character):

    pronunciation = NotImplemented
    bottom_type = NotImplemented  # 'straight', 'left' or 'right'.
    side_flipped = False
    bottom_flipped = False

    def __init__(self, side_ending_class, bottom_ending_class):
        self.side_ending = side_ending_class(self)
        if self.bottom_type == 'left':
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
        return self.bottom_type in ['left', 'right']

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
        if self.bottom_type == 'left':
            bottom_ending_heading = bottom_ending_heading.flipped_x()
        pen.turn_to(bottom_ending_heading)

        # Draw the ending, and maybe flip it horizontally.
        self.bottom_ending.draw(pen)
        if self.bottom_type == 'left':
            pen.paper.mirror_x(bottom_ending_position.x)
        paper.merge(pen.paper)

        paper.join_paths()
        paper.fuse_paths()

        # Override the bounds so that the letter reaches the full line height.
        bounds = paper.bounds()
        bounds.bottom = UNDER
        bounds.top = OVER
        paper.override_bounds(bounds)

        # We need to center on x=0 here because otherwise flipped
        # consonants wouldn't flip at the right x value.
        paper.center_on_x(0)

        return paper


class P(ConsonantCharacter):

    pronunciation = 'p'
    bottom_type = 'straight'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)

        pen.turn_to(-45)
        pen.line_to_y(MIDDLE + WIDTH)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE)


class T(ConsonantCharacter):

    pronunciation = 't'
    bottom_type = 'straight'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_left(90)
        pen.line_to_y(MIDDLE)


class K(ConsonantCharacter):

    pronunciation = 'k'
    bottom_type = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.5)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE)


class Q(ConsonantCharacter):

    pronunciation = 'q'
    bottom_type = 'left'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.5)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE, end_slant=0)
        pen.turn_to(180)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant60 / 2)
        pen.turn_left(60)
        pen.line_forward(WIDTH, start_slant=0)


class C(ConsonantCharacter):

    pronunciation = 'c'
    bottom_type = 'straight'

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

    pronunciation = 'c^'
    bottom_type = 'left'

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

    pronunciation = 'l'
    bottom_type = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE)
        pen.turn_to(-45)
        pen.line_forward(WIDTH)


class H(ConsonantCharacter):

    pronunciation = 'h'
    bottom_type = 'straight'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3, end_slant=45)
        pen.turn_left(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH / 2)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE)
        pen.turn_to(-90)
        pen.line_forward(WIDTH)


class PStop(ConsonantCharacter):

    pronunciation = "p'"
    bottom_type = 'straight'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE - pen.last_slant_width() / slant45 / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + WIDTH * slant45 / 2)
        pen.turn_to(-90)
        pen.line_forward(WIDTH, start_slant=45)


class TStop(ConsonantCharacter):

    pronunciation = "t'"
    bottom_type = 'straight'
    side_flipped = True
    bottom_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_slant=-45)


class KStop(ConsonantCharacter):

    pronunciation = "k'"
    bottom_type = 'straight'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(180)
        pen.line_forward(WIDTH / 2, start_slant=-45)
        pen.arc_left(90, WIDTH / 2)
        pen.line_to_y(MIDDLE)


class QStop(ConsonantCharacter):

    pronunciation = "q'"
    bottom_type = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE, end_slant=0)
        pen.turn_to(180)
        pen.move_forward(WIDTH * slant60)
        pen.turn_to(-60)
        pen.line_forward(WIDTH, start_slant=0)


class CStop(ConsonantCharacter):

    pronunciation = "c'"
    bottom_type = 'right'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE + WIDTH / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH * slant75 / 2)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE, start_slant=45)


class CHacekStop(ConsonantCharacter):

    pronunciation = "c^'"
    bottom_type = 'left'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - WIDTH / 2, start_slant=-45)
        pen.turn_to(0)
        pen.line_forward(2.5)
        pen.turn_to(-135)
        pen.line_forward(1.5 * WIDTH)


class F(ConsonantCharacter):

    pronunciation = 'f'
    bottom_type = 'straight'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-45)
        pen.line_to_y(TOP - 2 * WIDTH, end_slant=0)
        pen.turn_to(180)
        pen.move_forward(WIDTH * slant45 / 2 + WIDTH / 2)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_slant=0)


class TCedilla(ConsonantCharacter):

    pronunciation = 't,'
    bottom_type = 'straight'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - WIDTH / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_forward(WIDTH, start_slant=45)


class X(ConsonantCharacter):

    pronunciation = 'x'
    bottom_type = 'left'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.5)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE, end_slant=0)
        pen.turn_to(0)
        pen.move_forward(WIDTH * slant60)
        pen.turn_to(-120)
        pen.line_forward(WIDTH, start_slant=0)


class S(ConsonantCharacter):

    pronunciation = 's'
    bottom_type = 'straight'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - WIDTH / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(0)
        pen.line_forward(1.5 * WIDTH, start_slant=45)
        pen.arc_right(90, WIDTH / 2)
        pen.line_forward(WIDTH)


class SHacek(ConsonantCharacter):

    pronunciation = 's^'
    bottom_type = 'straight'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - WIDTH / 2, start_slant=-45)
        pen.turn_to(0)
        pen.line_forward(2)
        pen.turn_to(-90)
        pen.line_forward(WIDTH)


class R(ConsonantCharacter):

    pronunciation = 'r'
    bottom_type = 'straight'

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

    pronunciation = 'w'
    bottom_type = 'straight'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5, end_slant=45)
        pen.turn_left(45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_slant=45)


class M(ConsonantCharacter):

    pronunciation = 'm'
    bottom_type = 'left'

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.5)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE)
        pen.turn_to(-120)
        pen.line_forward(WIDTH)


class NHacek(ConsonantCharacter):

    pronunciation = 'n^'
    bottom_type = 'right'
    side_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(WIDTH * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - WIDTH / 2, start_slant=-45)
        pen.turn_to(-45)
        pen.line_forward(WIDTH)


B = flip(P)
B.pronunciation = 'b'
D = flip(T)
D.pronunciation = 'd'
G = flip(K)
G.pronunciation = 'g'
RHacek = flip(Q)
RHacek.pronunciation = 'r^'
ZDot = flip(C)
ZDot.pronunciation = 'dz'
J = flip(CHacek)
J.pronunciation = 'j'
Stop = flip(H)
Stop.pronunciation = "'"
PH = flip(PStop)
PH.pronunciation = 'ph'
TH = flip(TStop)
TH.pronunciation = 'th'
KH = flip(KStop)
KH.pronunciation = 'kh'
QH = flip(QStop)
QH.pronunciation = 'qh'
CH = flip(CStop)
CH.pronunciation = 'ch'
CHacekH = flip(CHacekStop)
CHacekH.pronunciation = "c^h"
V = flip(F)
V.pronunciation = 'v'
Dh = flip(TCedilla)
Dh.pronunciation = 'dh'
Xh = flip(X)
Xh.pronunciation = 'xh'
Z = flip(S)
Z.pronunciation = 'z'
ZHacek = flip(SHacek)
ZHacek.pronunciation = 'z^'
Y = flip(W)
Y.pronunciation = 'y'
LCedilla = flip(L)
LCedilla.pronunciation = 'l,'
CCedilla = flip(R)
CCedilla.pronunciation = 'c,'
N = flip(M)
N.pronunciation = 'n'
TLCedilla = flip(NHacek)
TLCedilla.pronunciation = 'tl,'


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


class SideEndingStub(ConsonantCharacter):

    def __init__(self, side_ending_class, bottom_ending_class):
        self.side_ending = side_ending_class(self)
        self.bottom_ending = None

    def draw_character(self, mode):
        paper = Paper()

        pen = Pen()

        pen.fill_mode('white')
        pen.move_to((0, TOP - WIDTH / 2))
        pen.square(mode.width * 2)
        paper.merge(pen.paper)

        pen = Pen()
        pen.set_mode(mode)
        pen.move_to((0, TOP - WIDTH / 2))
        pen.turn_to(0)
        pen.line_forward(WIDTH + 2.0)
        self.side_ending.draw(pen)
        paper.merge_under(pen.paper)

        bounds = paper.bounds()
        bounds.top = OVER
        bounds.bottom = MIDDLE
        bounds.left = 0
        paper.override_bounds(bounds)

        return paper


class VerticalBar(ConsonantCharacter):

    bottom_type = 'straight'

    def __init__(self, side_ending_class, bottom_ending_class):
        self.side_ending = None
        self.bottom_ending = bottom_ending_class(self)

    def draw_character(self, mode):
        pen = Pen()
        pen.set_mode(mode)
        pen.move_to((0, TOP))
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_slant=45)
        self.bottom_ending.draw(pen)
        pen.paper.fuse_paths()
        pen.paper.center_on_x(0)
        return pen.paper
