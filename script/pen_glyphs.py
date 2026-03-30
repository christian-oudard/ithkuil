#!/usr/bin/env python3
"""
Ithkuil V4 Secondary Characters drawn with pen.py.

Each consonant function receives a blank Pen and draws into pen.paper.
The em-square is 0-500 wide, 0-1000 tall (Y-up).
Characters occupy roughly x=50-450, y=100-900.
SW = stroke width.

Arc geometry matches glyphs.py: arc centers and radii are identical;
angles in standard math convention (0=east, 90=north, Y-up).
"""
import sys, os, math
sys.path.insert(0, os.path.dirname(__file__))

from pen import Pen

SW = 65  # stroke width


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _arc_end(cx, cy, r, a_deg):
    """Point on circle at angle a_deg (degrees)."""
    a = math.radians(a_deg)
    return cx + r * math.cos(a), cy + r * math.sin(a)


# ---------------------------------------------------------------------------
# Draw functions  (one per consonant)
# ---------------------------------------------------------------------------

def draw_p(pen):
    """p: top horiz + vertical stem + CW foot arc."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(350, 800)          # top horiz
    pen.move_to(100, 800); pen.line_to(100, 350)          # stem
    pen.arc_to(*_arc_end(250, 350, 150, 270), cx=250, cy=350)  # CW foot to (250,200)


def draw_b(pen):
    """b: top horiz + right stem + CW foot arc."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(350, 800)          # top horiz
    pen.move_to(350, 800); pen.line_to(350, 350)          # right stem
    pen.arc_to(*_arc_end(200, 350, 150, 270), cx=200, cy=350)  # CW to (200,200)


def draw_t(pen):
    """t: Gamma shape (stem + top horiz)."""
    pen.set_width(SW)
    pen.move_to(100, 200); pen.line_to(100, 800); pen.line_to(400, 800)


def draw_d(pen):
    """d: reversed Gamma with bottom serif."""
    pen.set_width(SW)
    pen.move_to(250, 200); pen.line_to(400, 200)
    pen.move_to(400, 200); pen.line_to(400, 800); pen.line_to(100, 800)


def draw_k(pen):
    """k: top horiz + stub + diagonal."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(400, 800)
    pen.line_to(400, 500); pen.line_to(100, 200)


def draw_g(pen):
    """g: mirror of k."""
    pen.set_width(SW)
    pen.move_to(400, 800); pen.line_to(100, 800)
    pen.line_to(100, 500); pen.line_to(400, 200)


def draw_f(pen):
    """f: reversed-5 (top horiz, two verts, mid horiz, CCW foot arc)."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(400, 800)     # top horiz
    pen.move_to(100, 800); pen.line_to(100, 550)     # upper vert
    pen.line_to(350, 550); pen.line_to(350, 50)      # mid horiz + lower vert
    # foot: 270° CCW arc from (350,50) around (350,200) to (200,200)
    # at (350,50), heading east to start CCW arc
    pen.break_stroke()
    pen.move_to(350, 50); pen.turn_to(0)
    pen.arc_left(270, 150)


def draw_v(pen):
    """v: mirror of f."""
    pen.set_width(SW)
    pen.move_to(400, 800); pen.line_to(100, 800)
    pen.move_to(400, 800); pen.line_to(400, 550)
    pen.line_to(150, 550); pen.line_to(150, 50)
    # foot: 270° CW arc from (150,50) around (150,200) to (300,200)
    pen.break_stroke()
    pen.move_to(150, 50); pen.turn_to(180)
    pen.arc_right(270, 150)


def draw_s(pen):
    """s: zigzag."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(350, 550); pen.line_to(100, 300)


def draw_z(pen):
    """z: mirror zigzag."""
    pen.set_width(SW)
    pen.move_to(350, 800); pen.line_to(100, 550); pen.line_to(350, 300)


def draw_tc(pen):
    """ţ: C-curve opening right (semicircle)."""
    pen.set_width(SW)
    # Semicircle: center (350,500), r=250, from 90° to -90° (CW 180°)
    # top (350,750) → CW 180° → bottom (350,250)
    pen.move_to(350, 750); pen.turn_to(180)
    pen.arc_right(180, 250)


def draw_dh(pen):
    """ḑ: C-curve opening left (mirror of ţ)."""
    pen.set_width(SW)
    pen.move_to(150, 750); pen.turn_to(0)
    pen.arc_left(180, 250)


def draw_sh(pen):
    """š: 2-like shape."""
    pen.set_width(SW)
    # Top arc: CCW from (100,650) to (400,650), center (250,650), r=150
    pen.move_to(100, 650)
    pen.arc_to(400, 650, cx=250, cy=650)
    # Diagonal and bottom horiz: break then draw
    pen.move_to(100, 650); pen.line_to(350, 250); pen.line_to(100, 250)


def draw_zh(pen):
    """ž: mirror of š."""
    pen.set_width(SW)
    pen.move_to(400, 650)
    pen.arc_to(100, 650, cx=250, cy=650)
    pen.move_to(400, 650); pen.line_to(150, 250); pen.line_to(400, 250)


def draw_ch(pen):
    """č: square bracket [."""
    pen.set_width(SW)
    pen.move_to(350, 800); pen.line_to(100, 800)
    pen.line_to(100, 200); pen.line_to(350, 200)


def draw_j(pen):
    """j: reversed bracket ]."""
    pen.set_width(SW)
    pen.move_to(150, 800); pen.line_to(400, 800)
    pen.line_to(400, 200); pen.line_to(150, 200)


def draw_c(pen):
    """c: L-shape (vert + bottom horiz)."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(100, 200); pen.line_to(400, 200)


def draw_zd(pen):
    """ẓ: L with top serif."""
    pen.set_width(SW)
    pen.move_to(250, 800); pen.line_to(100, 800)
    pen.line_to(100, 200); pen.line_to(400, 200)


def draw_x(pen):
    """x: Z-shape."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(400, 800)
    pen.move_to(400, 800); pen.line_to(100, 200)
    pen.move_to(100, 200); pen.line_to(400, 200)


def draw_cy(pen):
    """ç: sigma shape (horiz-diag-diag-horiz)."""
    pen.set_width(SW)
    pen.move_to(400, 800); pen.line_to(100, 800)
    pen.line_to(250, 500); pen.line_to(100, 200)
    pen.line_to(400, 200)


def draw_h(pen):
    """h: 4-like shape."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(300, 450); pen.line_to(300, 200)
    pen.move_to(150, 450); pen.line_to(450, 450)


def draw_lh(pen):
    """ļ: V-chevron pointing down."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(250, 400); pen.line_to(400, 800)


def draw_c_cedilla(pen):
    """c, (c-cedilla): L-shape like c but with extra mark."""
    # Using same shape as c for now
    draw_c(pen)


def draw_rh(pen):
    """ř: loop at top + stem."""
    pen.set_width(SW)
    # Loop: CCW from (50, 650) around (200, 650) to (200, 500)
    # At (50, 650): angle from center (200,650) = 180°. End (200,500): angle 270°.
    # CCW from 180° to 270° = 90°
    pen.move_to(50, 650)
    pen.arc_to(200, 500, cx=200, cy=650)  # CCW 90°
    pen.line_to(200, 200)


def draw_m(pen):
    """m: diagonal with chisel ends."""
    pen.set_width(SW)
    # Chisel start = slant at start; chisel -25 at end
    pen.move_to(100, 800); pen.line_to(350, 200, start_slant=25, end_slant=-25)


def draw_n(pen):
    """n: diagonal with bottom serif."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(300, 200, start_slant=25)
    pen.line_to(450, 200)


def draw_ny(pen):
    """ň: diagonal with rightward kick."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(300, 350, start_slant=25)
    pen.line_to(300, 200)
    pen.move_to(300, 350); pen.line_to(450, 250)


def draw_l(pen):
    """l: diagonal + vertical + bottom horiz."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(250, 500)
    pen.line_to(250, 200); pen.line_to(450, 200)


def draw_r(pen):
    """r: step shape."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(350, 800)
    pen.line_to(350, 550); pen.line_to(150, 550)
    pen.line_to(150, 200)


def draw_w(pen):
    """w: diagonal + bottom-right curve."""
    pen.set_width(SW)
    pen.move_to(100, 800); pen.line_to(250, 450, start_slant=25)
    # CW quarter-arc from (250,450) to (400,300): center (250,300)
    pen.arc_to(400, 300, cx=250, cy=300)


def draw_y(pen):
    """y: diagonal from right + bottom-left curve."""
    pen.set_width(SW)
    pen.move_to(350, 800); pen.line_to(200, 450, start_slant=-25)
    # CCW quarter-arc from (200,450) to (50,300): center (200,300)
    pen.arc_to(50, 300, cx=200, cy=300)


def draw_glottal(pen):
    """' (glottal stop): short vertical tick."""
    pen.set_width(SW)
    pen.move_to(200, 700); pen.line_to(200, 400)


# ---------------------------------------------------------------------------
# Consonant → draw function map
# ---------------------------------------------------------------------------

PEN_CHARS = {
    'p': draw_p, 'b': draw_b, 't': draw_t, 'd': draw_d,
    'k': draw_k, 'g': draw_g, 'f': draw_f, 'v': draw_v,
    's': draw_s, 'z': draw_z, 'ţ': draw_tc, 'ḑ': draw_dh,
    'š': draw_sh, 'ž': draw_zh, 'č': draw_ch, 'j': draw_j,
    'c': draw_c, 'ẓ': draw_zd, 'x': draw_x,
    'ç': draw_cy, 'h': draw_h,
    'l': draw_l, 'r': draw_r, 'ļ': draw_lh, 'ř': draw_rh,
    'm': draw_m, 'n': draw_n, 'ň': draw_ny,
    'w': draw_w, 'y': draw_y, "'": draw_glottal,
}


def build_pen_secondary():
    """Build SECONDARY dict (path data in Y-up coordinates) using pen strokes."""
    from glyphs import CONSONANT_ORDER, _ASCII_NAMES, PUA_SECONDARY

    result = {}
    for cons in CONSONANT_ORDER:
        if cons not in PEN_CHARS:
            continue
        pen = Pen()
        pen.set_width(SW)
        PEN_CHARS[cons](pen)
        path_d = pen.paper.to_path_data()

        ascii_name = _ASCII_NAMES.get(cons, cons)
        result[cons] = {
            'name': f'sec_{ascii_name}',
            'consonant': cons,
            'width': 500,
            'path': path_d,
            'codepoint': PUA_SECONDARY + CONSONANT_ORDER.index(cons),
        }

    return result
