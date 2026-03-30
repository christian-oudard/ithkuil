"""
Ithkuil V3 consonant character bodies.

Each ConsonantCharacter has a body (drawn by draw()), a side ending (top),
and a bottom ending. The draw() method uses a pen starting at the top of
the body and ending at the bottom, where the bottom ending attaches.
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from pen import Paper, Pen
from .common import (
    Character, OVER, TOP, MIDDLE, UNDER,
    slant45, slant60, slant75,
    mirror_character_x,
)


class ConsonantCharacter(Character):

    pronunciation = NotImplemented
    side_flipped = False
    bottom_straight = NotImplemented
    bottom_flipped = False

    def __init__(self, side_ending_class, bottom_ending_class):
        self.side_ending_class = side_ending_class
        self.bottom_ending_class = bottom_ending_class

    def draw_character(self, width=0.5, fuse=True):
        side_ending = self.side_ending_class(self, self.side_flipped)
        bottom_ending = self.bottom_ending_class(
            self, self.bottom_straight, self.bottom_flipped)

        paper = Paper()

        # Draw body
        pen = Pen()
        pen.set_width(width)
        pen.move_to(0, TOP - width / 2)
        side_ending_position = pen.position
        self.draw(pen)
        bottom_ending_position = pen.position
        bottom_ending_heading = pen.heading
        paper.merge(pen.paper)

        # Draw side ending
        pen = Pen()
        pen.set_width(width)
        pen.move_to(side_ending_position)
        pen.turn_to(0)
        side_ending.draw(pen)
        paper.merge(pen.paper)

        # Draw bottom ending
        pen = Pen()
        pen.set_width(width)
        pen.move_to(bottom_ending_position)
        if not self.bottom_straight and self.bottom_flipped:
            bottom_ending_heading = bottom_ending_heading.flipped_x()
        pen.turn_to(bottom_ending_heading)
        bottom_ending.draw(pen)
        if not self.bottom_straight and self.bottom_flipped:
            pen.paper.mirror_x(float(bottom_ending_position.x))
        paper.merge(pen.paper)

        paper.center_on_x(0)
        return paper


# ---------------------------------------------------------------------------
# Consonant body shapes
# ---------------------------------------------------------------------------

class P(ConsonantCharacter):
    pronunciation = 'p'
    side_flipped = True
    bottom_straight = True

    def draw(self, pen):
        w = pen.width
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
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(4.25)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE, end_slant=0)
        pen.turn_to(180)
        pen.move_forward(pen.last_slant_width() / 2 + w * slant60 / 2)
        pen.turn_left(60)
        pen.line_forward(w, start_slant=0)


class C(ConsonantCharacter):
    pronunciation = 'c'
    bottom_straight = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(4.25)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE + w / 2)
        pen.turn_to(0)
        pen.line_forward(2.75)
        pen.turn_to(-90)
        pen.line_forward(w)


class CHacek(ConsonantCharacter):
    pronunciation = 'c^'
    bottom_straight = False
    bottom_flipped = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(5.0)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE + w / 2)
        pen.turn_to(0)
        pen.line_forward(3.5)
        pen.turn_to(-135)
        pen.line_forward(1.5 * w)


class L(ConsonantCharacter):
    pronunciation = 'l'
    bottom_straight = False
    bottom_flipped = False

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE)
        pen.turn_to(-45)
        pen.line_forward(w)


class H(ConsonantCharacter):
    pronunciation = 'h'
    bottom_straight = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(2.5, end_slant=45)
        pen.turn_left(45)
        pen.move_forward(pen.last_slant_width() / 2 + w / 2)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE)
        pen.turn_to(-90)
        pen.line_forward(w)


class PStop(ConsonantCharacter):
    pronunciation = "p'"
    bottom_straight = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE - pen.last_slant_width() / slant45 / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(pen.last_slant_width() / 2 + w * slant45 / 2)
        pen.turn_to(-90)
        pen.line_forward(w, start_slant=45)


class TStop(ConsonantCharacter):
    pronunciation = "t'"
    side_flipped = True
    bottom_straight = True
    bottom_flipped = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(4, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(w * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_slant=-45)


class KStop(ConsonantCharacter):
    pronunciation = "k'"
    side_flipped = True
    bottom_straight = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(3.5, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(w * slant45)
        pen.turn_to(180)
        pen.line_forward(w / 2, start_slant=-45)
        pen.arc_left(90, w / 2)
        pen.line_to_y(MIDDLE)


class QStop(ConsonantCharacter):
    pronunciation = "q'"
    bottom_straight = False
    bottom_flipped = False

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE, end_slant=0)
        pen.turn_to(180)
        pen.move_forward(w * slant60)
        pen.turn_to(-60)
        pen.line_forward(w, start_slant=0)


class CStop(ConsonantCharacter):
    pronunciation = "c'"
    bottom_straight = False
    bottom_flipped = False

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE + w / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(w * slant45 / 2 + w * slant75 / 2)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE, start_slant=45)


class CHacekStop(ConsonantCharacter):
    pronunciation = "c^'"
    side_flipped = True
    bottom_straight = False
    bottom_flipped = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(4.5, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(w * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - w / 2, start_slant=-45)
        pen.turn_to(0)
        pen.line_forward(2.5)
        pen.turn_to(-135)
        pen.line_forward(1.5 * w)


class F(ConsonantCharacter):
    pronunciation = 'f'
    side_flipped = True
    bottom_straight = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-45)
        pen.line_to_y(TOP - 2 * w, end_slant=0)
        pen.turn_to(180)
        pen.move_forward(w * slant45 / 2 + w / 2)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_slant=0)


class TCedilla(ConsonantCharacter):
    pronunciation = 't,'
    bottom_straight = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(3.5)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - w / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(w * slant45)
        pen.turn_to(-90)
        pen.line_forward(w, start_slant=45)


class X(ConsonantCharacter):
    pronunciation = 'x'
    bottom_straight = False
    bottom_flipped = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(4.0)
        pen.turn_to(-60)
        pen.line_to_y(MIDDLE, end_slant=0)
        pen.turn_to(0)
        pen.move_forward(w * slant60)
        pen.turn_to(-120)
        pen.line_forward(w, start_slant=0)


class S(ConsonantCharacter):
    pronunciation = 's'
    bottom_straight = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(4)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - w / 2, end_slant=45)
        pen.turn_to(45)
        pen.move_forward(w * slant45)
        pen.turn_to(0)
        pen.line_forward(1.5 * w, start_slant=45)
        pen.arc_right(90, w / 2)
        pen.line_forward(w)


class SHacek(ConsonantCharacter):
    pronunciation = 's^'
    side_flipped = True
    bottom_straight = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(4, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(w * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - w / 2, start_slant=-45)
        pen.turn_to(0)
        pen.line_forward(2)
        pen.turn_to(-90)
        pen.line_forward(w)


class R(ConsonantCharacter):
    pronunciation = 'r'
    bottom_straight = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(4.0)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE + w / 2)
        pen.turn_to(180)
        pen.line_to_x(pen.paper.bounds().left + w / 2)
        pen.turn_to(-90)
        pen.line_forward(w)


class W(ConsonantCharacter):
    pronunciation = 'w'
    bottom_straight = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(3.5, end_slant=45)
        pen.turn_left(45)
        pen.move_forward(w * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE, start_slant=45)


class M(ConsonantCharacter):
    pronunciation = 'm'
    bottom_straight = False
    bottom_flipped = True

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(4.0)
        pen.turn_to(-45)
        pen.line_to_y(MIDDLE)
        pen.turn_to(-120)
        pen.line_forward(w)


class NHacek(ConsonantCharacter):
    pronunciation = 'n^'
    side_flipped = True
    bottom_straight = False
    bottom_flipped = False

    def draw(self, pen):
        w = pen.width
        pen.turn_to(180)
        pen.line_forward(4, end_slant=-45)
        pen.turn_to(-45)
        pen.move_forward(w * slant45)
        pen.turn_to(-90)
        pen.line_to_y(MIDDLE - w / 2, start_slant=-45)
        pen.turn_to(-45)
        pen.line_forward(w)


# ---------------------------------------------------------------------------
# Mirrored consonants
# ---------------------------------------------------------------------------

mx = mirror_character_x

B         = mx(P,         'B',         pronunciation='b')
D         = mx(T,         'D',         pronunciation='d')
G         = mx(K,         'G',         pronunciation='g')
RHacek    = mx(Q,         'RHacek',    pronunciation='r^')
ZDot      = mx(C,         'ZDot',      pronunciation='dz')
J         = mx(CHacek,    'J',         pronunciation='j')
Stop      = mx(H,         'Stop',      pronunciation="'")
PH        = mx(PStop,     'PH',        pronunciation='ph')
TH        = mx(TStop,     'TH',        pronunciation='th')
KH        = mx(KStop,     'KH',        pronunciation='kh')
QH        = mx(QStop,     'QH',        pronunciation='qh')
CH        = mx(CStop,     'CH',        pronunciation='ch')
CHacekH   = mx(CHacekStop,'CHacekH',   pronunciation="c^h")
V         = mx(F,         'V',         pronunciation='v')
Dh        = mx(TCedilla,  'Dh',        pronunciation='dh')
Xh        = mx(X,         'Xh',        pronunciation='xh')
Z         = mx(S,         'Z',         pronunciation='z')
ZHacek    = mx(SHacek,    'ZHacek',    pronunciation='z^')
Y         = mx(W,         'Y',         pronunciation='y')
LCedilla  = mx(L,         'LCedilla',  pronunciation='l,')
CCedilla  = mx(R,         'CCedilla',  pronunciation='c,')
N         = mx(M,         'N',         pronunciation='n')
TLCedilla = mx(NHacek,    'TLCedilla', pronunciation='tl,')


# ---------------------------------------------------------------------------
# Consonant list (same order as reference)
# ---------------------------------------------------------------------------

consonants = [
    P, T, K, Q, C, CHacek,
    B, D, G, Stop, ZDot, J,
    PStop, TStop, KStop, QStop, CStop, CHacekStop,
    PH, TH, KH, QH, CH, CHacekH,
    F, TCedilla, X, Xh,
    S, SHacek, V, Dh,
    H, RHacek, Z, ZHacek,
    W, L, Y, LCedilla,
    R, CCedilla, M, N, NHacek, TLCedilla,
]
