from canoepaddle import Pen, Paper
from .common import (
    Character,
    OVER,
    TOP,
    MIDDLE,
    UNDER,
    slant45,
    slant60,
    slant75,
    mirror_character_x,
)


class ConsonantCharacter(Character):

    pronunciation = NotImplemented
    side_flipped = False
    bottom_straight = NotImplemented
    bottom_flipped = False

    def __init__(self, side_ending_class, bottom_ending_class):
        super().__init__()
        self.side_ending_class = side_ending_class
        self.bottom_ending_class = bottom_ending_class

    def __str__(self):
        return self.format(
            side=self.side_ending_class,
            bottom=self.bottom_ending_class,
        )

    def draw_character(self, mode, fuse=True):
        side_ending = self.side_ending_class(self, self.side_flipped)
        bottom_ending = self.bottom_ending_class(
            self,
            self.bottom_straight,
            self.bottom_flipped,
        )

        paper = Paper()

        # When drawing the body of the consonant, subclasses will start
        # where the side ending is, and end where the bottom ending is.
        pen = Pen()
        pen.set_mode(mode)
        pen.move_to((0, TOP - pen.mode.width / 2))
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
        side_ending.draw(pen)
        paper.merge(pen.paper)

        # Draw the bottom ending.
        pen = Pen()
        pen.set_mode(mode)
        pen.move_to(bottom_ending_position)

        # If the bottom orientation is slanted left, then we have to
        # start the bottom ending from a flipped heading so when it flips
        # again later it will be correct.
        if not self.bottom_straight and self.bottom_flipped:
            bottom_ending_heading = bottom_ending_heading.flipped_x()
        pen.turn_to(bottom_ending_heading)

        # Draw the ending, and maybe flip it horizontally.
        bottom_ending.draw(pen)
        if not self.bottom_straight and self.bottom_flipped:
            pen.paper.mirror_x(bottom_ending_position.x)
        paper.merge(pen.paper)

        if fuse:
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
    side_flipped = True
    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)

        pen.turn_to(-45)
        pen.line_to_y(MIDDLE + 1.0)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE)


class T(ConsonantCharacter):

    pronunciation = 't'
    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_left(90)
        pen.line_to_y(MIDDLE)


class K(ConsonantCharacter):

    pronunciation = 'k'
    bottom_straight = False
    bottom_flipped = False

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.0)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE)


class Q(ConsonantCharacter):

    pronunciation = 'q'
    bottom_straight = False
    bottom_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.25)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE, end_slant=0)
        pen.turn_to(180)
        pen.move_forward(pen.last_slant_width() / 2 + pen.mode.width * slant60 / 2)
        pen.turn_left(60)
        pen.line_forward(pen.mode.width, start_slant=0)


class C(ConsonantCharacter):

    pronunciation = 'c'
    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.25)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE + pen.mode.width / 2)
        pen.turn_to(0)
        pen.line_forward(2.75)
        pen.turn_to(-90)
        pen.line_forward(pen.mode.width)


class CHacek(ConsonantCharacter):

    pronunciation = 'c^'
    bottom_straight = False
    bottom_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(5.0)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE + pen.mode.width / 2)
        pen.turn_to(0)
        pen.line_forward(3.5)
        pen.turn_to(-135)
        pen.line_forward(1.5 * pen.mode.width)


class L(ConsonantCharacter):

    pronunciation = 'l'
    bottom_straight = False
    bottom_flipped = False

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE)
        pen.turn_to(-45)
        pen.line_forward(pen.mode.width)


class H(ConsonantCharacter):

    pronunciation = 'h'
    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(2.5, end_slant=45)
        pen.turn_left(45)
        pen.move_forward(pen.last_slant_width() / 2 + pen.mode.width / 2)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE)
        pen.turn_to(-90)
        pen.line_forward(pen.mode.width)


class PStop(ConsonantCharacter):

    pronunciation = "p'"
    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE - pen.last_slant_width() / slant45 / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + pen.mode.width * slant45 / 2)
        pen.turn_to(-90)
        pen.line_forward(pen.mode.width, start_slant=45)


class TStop(ConsonantCharacter):

    pronunciation = "t'"
    side_flipped = True
    bottom_straight = True
    bottom_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(pen.mode.width * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_slant=-45)


class KStop(ConsonantCharacter):

    pronunciation = "k'"
    side_flipped = True
    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(pen.mode.width * slant45)
        pen.turn_to(180)
        pen.line_forward(pen.mode.width / 2, start_slant=-45)
        pen.arc_left(90, pen.mode.width / 2)
        pen.line_to_y(MIDDLE)


class QStop(ConsonantCharacter):

    pronunciation = "q'"
    bottom_straight = False
    bottom_flipped = False

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE, end_slant=0)
        pen.turn_to(180)
        pen.move_forward(pen.mode.width * slant60)
        pen.turn_to(-60)
        pen.line_forward(pen.mode.width, start_slant=0)


class CStop(ConsonantCharacter):

    pronunciation = "c'"
    bottom_straight = False
    bottom_flipped = False

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE + pen.mode.width / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(pen.mode.width * slant45 / 2 + pen.mode.width * slant75 / 2)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE, start_slant=45)


class CHacekStop(ConsonantCharacter):

    pronunciation = "c^'"
    side_flipped = True
    bottom_straight = False
    bottom_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.5, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(pen.mode.width * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - pen.mode.width / 2, start_slant=-45)
        pen.turn_to(0)
        pen.line_forward(2.5)
        pen.turn_to(-135)
        pen.line_forward(1.5 * pen.mode.width)


class F(ConsonantCharacter):

    pronunciation = 'f'
    side_flipped = True
    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-45)
        pen.line_to_y(TOP - 2 * pen.mode.width, end_slant=0)
        pen.turn_to(180)
        pen.move_forward(pen.mode.width * slant45 / 2 + pen.mode.width / 2)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_slant=0)


class TCedilla(ConsonantCharacter):

    pronunciation = 't,'
    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - pen.mode.width / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(pen.mode.width * slant45)
        pen.turn_to(-90)
        pen.line_forward(pen.mode.width, start_slant=45)


class X(ConsonantCharacter):

    pronunciation = 'x'
    bottom_straight = False
    bottom_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.0)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE, end_slant=0)
        pen.turn_to(0)
        pen.move_forward(pen.mode.width * slant60)
        pen.turn_to(-120)
        pen.line_forward(pen.mode.width, start_slant=0)


class S(ConsonantCharacter):

    pronunciation = 's'
    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - pen.mode.width / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(pen.mode.width * slant45)
        pen.turn_to(0)
        pen.line_forward(1.5 * pen.mode.width, start_slant=45)
        pen.arc_right(90, pen.mode.width / 2)
        pen.line_forward(pen.mode.width)


class SHacek(ConsonantCharacter):

    pronunciation = 's^'
    side_flipped = True
    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(pen.mode.width * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - pen.mode.width / 2, start_slant=-45)
        pen.turn_to(0)
        pen.line_forward(2)
        pen.turn_to(-90)
        pen.line_forward(pen.mode.width)


class R(ConsonantCharacter):

    pronunciation = 'r'
    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.0)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE + pen.mode.width / 2)
        pen.turn_to(180)
        pen.line_to_x(pen.paper.bounds().left + pen.mode.width / 2)
        pen.turn_to(-90)
        pen.line_forward(pen.mode.width)


class W(ConsonantCharacter):

    pronunciation = 'w'
    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(3.5, end_slant=45)
        pen.turn_left(45)
        pen.move_forward(pen.mode.width * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_slant=45)


class M(ConsonantCharacter):

    pronunciation = 'm'
    bottom_straight = False
    bottom_flipped = True

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4.0)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE)
        pen.turn_to(-120)
        pen.line_forward(pen.mode.width)


class NHacek(ConsonantCharacter):

    pronunciation = 'n^'
    side_flipped = True
    bottom_straight = False
    bottom_flipped = False

    def draw(self, pen):
        pen.turn_to(180)
        pen.line_forward(4, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(pen.mode.width * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - pen.mode.width / 2, start_slant=-45)
        pen.turn_to(-45)
        pen.line_forward(pen.mode.width)


# Mirrored characters.

mx = mirror_character_x

B = mx(P, 'B', pronunciation='b')
D = mx(T, 'D', pronunciation='d')
G = mx(K, 'G', pronunciation='g')
RHacek = mx(Q, 'RHacek', pronunciation='r^')
ZDot = mx(C, 'ZDot', pronunciation='dz')
J = mx(CHacek, 'J', pronunciation='j')
Stop = mx(H, 'Stop', pronunciation="'")
PH = mx(PStop, 'PH', pronunciation='ph')
TH = mx(TStop, 'TH', pronunciation='th')
KH = mx(KStop, 'KH', pronunciation='kh')
QH = mx(QStop, 'QH', pronunciation='qh')
CH = mx(CStop, 'CH', pronunciation='ch')
CHacekH = mx(CHacekStop, 'CHacekH', pronunciation="c^h")
V = mx(F, 'V', pronunciation='v')
Dh = mx(TCedilla, 'Dh', pronunciation='dh')
Xh = mx(X, 'Xh', pronunciation='xh')
Z = mx(S, 'Z', pronunciation='z')
ZHacek = mx(SHacek, 'ZHacek', pronunciation='z^')
Y = mx(W, 'Y', pronunciation='y')
LCedilla = mx(L, 'LCedilla', pronunciation='l,')
CCedilla = mx(R, 'CCedilla', pronunciation='c,')
N = mx(M, 'N', pronunciation='n')
TLCedilla = mx(NHacek, 'TLCedilla', pronunciation='tl,')


# Special characters.

class VerticalBar(ConsonantCharacter):

    bottom_straight = True

    def __init__(self, side_ending_class, bottom_ending_class):
        Character.__init__(self)
        self.side_ending_class = None
        self.bottom_ending_class = bottom_ending_class

    def draw_character(self, mode, fuse=True):
        bottom_ending = bottom_ending_class(
            self,
            self.bottom_straight,
            self.bottom_flipped,
        )

        pen = Pen()
        pen.set_mode(mode)
        pen.move_to((0, TOP))
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_slant=45)
        bottom_ending.draw(pen)
        if fuse:
            pen.paper.fuse_paths()
        pen.paper.center_on_x(0)
        return pen.paper


def stub_cap(pen, end):
    pen.move_to(end)


class SideEndingStub(ConsonantCharacter):

    def __init__(self, side_ending_class, bottom_ending_class):
        Character.__init__(self)
        self.side_ending_class = side_ending_class
        self.bottom_ending_class = None

    def draw_character(self, mode, **kwargs):
        side_ending = self.side_ending_class(
            self,
            self.side_flipped,
        )

        paper = Paper()

        pen = Pen()
        pen.set_mode(mode)
        pen.move_to((0, TOP - mode.width / 2))
        pen.turn_to(0)
        pen.line_forward(2.0)
        pen.last_segment().start_cap = stub_cap

        side_ending.draw(pen)
        paper.merge(pen.paper)

        bounds = paper.bounds()
        bounds.top = OVER
        bounds.bottom = MIDDLE
        bounds.left = 0
        paper.override_bounds(bounds)

        return paper


class BottomEndingStub(ConsonantCharacter):

    def __init__(self, side_ending_class, bottom_ending_class):
        Character.__init__(self)
        self.side_ending_class = None
        self.bottom_ending_class = bottom_ending_class

    def draw_character(self, mode, **kwargs):
        bottom_ending = self.bottom_ending_class(
            self,
            self.bottom_straight,
            self.bottom_flipped,
        )

        pen = Pen()
        pen.set_mode(mode)
        pen.move_to((0, MIDDLE - 0.5))
        self.draw(pen)
        bottom_ending.draw(pen)

        paper = pen.paper

        bounds = paper.bounds()
        bounds.top = MIDDLE
        bounds.bottom = UNDER
        paper.override_bounds(bounds)

        return paper

    def draw(self, pen):
        raise NotImplementedError()


class BottomEndingStraightStub(BottomEndingStub):

    bottom_straight = True

    def draw(self, pen):
        pen.turn_to(-90)
        pen.line_forward(1.0, start_slant=0)
        pen.last_segment().start_cap = stub_cap


class BottomEndingSlantedStub(BottomEndingStub):

    bottom_straight = False
    bottom_flipped = False

    def draw(self, pen):
        pen.turn_to(-45)
        pen.line_forward(1.0, start_slant=0)
        pen.last_segment().start_cap = stub_cap


# Character list.

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
