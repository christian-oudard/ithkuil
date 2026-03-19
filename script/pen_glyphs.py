#!/usr/bin/env python3
"""
Ithkuil V4 Secondary Characters defined with the pen library.

Each consonant is a function that draws with a Pen.
Connected strokes produce single contours with proper miter joins.
"""
import sys, os
sys.path.insert(0, os.path.dirname(__file__))

from pen import Pen, CAP_FLAT, CAP_POINTED, CAP_CHISEL, mirror_x

SW = 65  # standard stroke width (thicker to match reference weight)


# ============================================================================
# Secondary Character Draw Functions
# ============================================================================

def draw_p(pen):
    """p: Top horizontal, vertical down, bottom curve right."""
    pen.set_width(SW)
    pen.move_to(100, 800)
    pen.line_to(350, 800)
    pen.move_to(100, 800)
    pen.line_to(100, 350)
    pen.arc_to(350, 350, radius=150, clockwise=True)

def draw_b(pen):
    """b: Mirror of p."""
    pen.set_width(SW)
    pen.move_to(350, 800)
    pen.line_to(100, 800)
    pen.move_to(350, 800)
    pen.line_to(350, 350)
    pen.arc_to(100, 350, radius=150, clockwise=False)

def draw_t(pen):
    """t: Gamma shape."""
    pen.set_width(SW)
    pen.move_to(100, 200)
    pen.line_to(100, 800)
    pen.line_to(400, 800)

def draw_d(pen):
    """d: Reversed Gamma with serif."""
    pen.set_width(SW)
    pen.move_to(250, 200)
    pen.line_to(400, 200)
    pen.line_to(400, 800)
    pen.line_to(100, 800)

def draw_k(pen):
    """k: Top horizontal, stub down, diagonal."""
    pen.set_width(SW)
    pen.move_to(100, 800)
    pen.line_to(400, 800)
    pen.line_to(400, 500)
    pen.line_to(100, 200)

def draw_g(pen):
    """g: Mirror of k."""
    pen.set_width(SW)
    pen.move_to(400, 800)
    pen.line_to(100, 800)
    pen.line_to(100, 500)
    pen.line_to(400, 200)

def draw_f(pen):
    """f: S-shape (reversed 5)."""
    pen.set_width(SW)
    pen.move_to(100, 800)
    pen.line_to(400, 800)
    pen.move_to(100, 800)
    pen.line_to(100, 550)
    pen.line_to(350, 550)
    pen.line_to(350, 200)
    pen.set_end_cap(CAP_FLAT)
    pen.arc_to(200, 200, radius=150, clockwise=True)

def draw_v(pen):
    """v: Mirror of f (like 5)."""
    pen.set_width(SW)
    pen.move_to(400, 800)
    pen.line_to(100, 800)
    pen.move_to(400, 800)
    pen.line_to(400, 550)
    pen.line_to(150, 550)
    pen.line_to(150, 200)
    pen.arc_to(300, 200, radius=150, clockwise=False)

def draw_s(pen):
    """s: Zigzag."""
    pen.set_width(SW)
    pen.move_to(100, 800)
    pen.line_to(350, 550)
    pen.line_to(100, 300)

def draw_z(pen):
    """z: Mirror zigzag."""
    pen.set_width(SW)
    pen.move_to(350, 800)
    pen.line_to(100, 550)
    pen.line_to(350, 300)

def draw_tc(pen):
    """ţ: C-curve opening right."""
    pen.set_width(SW)
    pen.set_caps(CAP_POINTED)
    pen.move_to(350, 750)
    pen.arc_to(350, 250, radius=250, clockwise=False)

def draw_dh(pen):
    """ḑ: C-curve opening left."""
    pen.set_width(SW)
    pen.set_caps(CAP_POINTED)
    pen.move_to(150, 750)
    pen.arc_to(150, 250, radius=250, clockwise=True)

def draw_sh(pen):
    """š: 2-like shape (arc + diagonal + horizontal)."""
    pen.set_width(SW)
    pen.move_to(100, 650)
    pen.arc_to(400, 650, radius=150, clockwise=False)
    pen.move_to(100, 650)
    pen.line_to(350, 250)
    pen.line_to(100, 250)

def draw_zh(pen):
    """ž: Mirror 2."""
    pen.set_width(SW)
    pen.move_to(400, 650)
    pen.arc_to(100, 650, radius=150, clockwise=True)
    pen.move_to(400, 650)
    pen.line_to(150, 250)
    pen.line_to(400, 250)

def draw_ch(pen):
    """č: Square bracket [."""
    pen.set_width(SW)
    pen.move_to(350, 800)
    pen.line_to(100, 800)
    pen.line_to(100, 200)
    pen.line_to(350, 200)

def draw_j(pen):
    """j: Reversed bracket ]."""
    pen.set_width(SW)
    pen.move_to(150, 800)
    pen.line_to(400, 800)
    pen.line_to(400, 200)
    pen.line_to(150, 200)

def draw_c(pen):
    """c: L-shape."""
    pen.set_width(SW)
    pen.move_to(100, 800)
    pen.line_to(100, 200)
    pen.line_to(400, 200)

def draw_zd(pen):
    """ẓ: L with top serif."""
    pen.set_width(SW)
    pen.move_to(250, 800)
    pen.line_to(100, 800)
    pen.line_to(100, 200)
    pen.line_to(400, 200)

def draw_x(pen):
    """x: Z-shape."""
    pen.set_width(SW)
    pen.move_to(100, 800)
    pen.line_to(400, 800)
    pen.move_to(400, 800)
    pen.line_to(100, 200)
    pen.move_to(100, 200)
    pen.line_to(400, 200)

def draw_cy(pen):
    """ç: Sigma shape."""
    pen.set_width(SW)
    pen.move_to(400, 800)
    pen.line_to(100, 800)
    pen.line_to(250, 500)
    pen.line_to(100, 200)
    pen.line_to(400, 200)

def draw_h(pen):
    """h: 4-like shape."""
    pen.set_width(SW)
    pen.move_to(100, 800)
    pen.line_to(300, 450)
    pen.line_to(300, 200)
    pen.move_to(150, 450)
    pen.line_to(450, 450)

def draw_l(pen):
    """l: Diagonal + vertical + horizontal."""
    pen.set_width(SW)
    pen.move_to(100, 800)
    pen.line_to(250, 500)
    pen.line_to(250, 200)
    pen.line_to(450, 200)

def draw_r(pen):
    """r: Step shape."""
    pen.set_width(SW)
    pen.move_to(100, 800)
    pen.line_to(350, 800)
    pen.line_to(350, 550)
    pen.line_to(150, 550)
    pen.line_to(150, 200)

def draw_lh(pen):
    """ļ: V-chevron pointing down."""
    pen.set_width(SW)
    pen.move_to(100, 800)
    pen.line_to(250, 400)
    pen.line_to(400, 800)

def draw_rh(pen):
    """ř: Loop at top + stem."""
    pen.set_width(SW)
    pen.move_to(50, 650)
    pen.arc_to(200, 500, radius=150, clockwise=False)
    pen.move_to(200, 500)
    pen.line_to(200, 200)

def draw_m(pen):
    """m: Single diagonal with chisel ends."""
    pen.set_width(SW)
    pen.set_start_cap(CAP_CHISEL, 25)
    pen.set_end_cap(CAP_CHISEL, -25)
    pen.move_to(100, 800)
    pen.line_to(350, 200)

def draw_n(pen):
    """n: Diagonal with serif."""
    pen.set_width(SW)
    pen.set_start_cap(CAP_CHISEL, 25)
    pen.move_to(100, 800)
    pen.line_to(300, 200)
    pen.line_to(450, 200)

def draw_ny(pen):
    """ň: Diagonal with rightward kick."""
    pen.set_width(SW)
    pen.set_start_cap(CAP_CHISEL, 25)
    pen.move_to(100, 800)
    pen.line_to(300, 350)
    pen.line_to(300, 200)
    pen.move_to(300, 350)
    pen.line_to(450, 250)

def draw_w(pen):
    """w: Diagonal + bottom-right curve."""
    pen.set_width(SW)
    pen.set_start_cap(CAP_CHISEL, 25)
    pen.move_to(100, 800)
    pen.line_to(250, 450)
    pen.set_end_cap(CAP_POINTED)
    pen.arc_to(400, 300, radius=150, clockwise=True)

def draw_y(pen):
    """y: Diagonal from right + bottom-left curve."""
    pen.set_width(SW)
    pen.set_start_cap(CAP_CHISEL, -25)
    pen.move_to(350, 800)
    pen.line_to(200, 450)
    pen.set_end_cap(CAP_POINTED)
    pen.arc_to(50, 300, radius=150, clockwise=False)

def draw_glottal(pen):
    """': Short vertical tick."""
    pen.set_width(SW)
    pen.move_to(200, 700)
    pen.line_to(200, 400)


# Map consonants to draw functions
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
    """Build SECONDARY dict compatible with glyphs.py format, using pen strokes."""
    from glyphs import CONSONANT_ORDER, _ASCII_NAMES, PUA_SECONDARY

    result = {}
    for cons in CONSONANT_ORDER:
        if cons not in PEN_CHARS:
            continue
        pen = Pen(width=SW)
        PEN_CHARS[cons](pen)
        path_d = pen.to_path()

        ascii_name = _ASCII_NAMES.get(cons, cons)
        result[cons] = {
            'name': f'sec_{ascii_name}',
            'consonant': cons,
            'width': 500,
            'path': path_d,
        }
        result[cons]['codepoint'] = PUA_SECONDARY + CONSONANT_ORDER.index(cons)

    return result


def render_comparison():
    """Render pen vs outline characters side by side."""
    from glyphs import SECONDARY, CONSONANT_ORDER

    page_w, page_h = 950, 600
    svg = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{page_w}" height="{page_h}" '
        f'viewBox="0 0 {page_w} {page_h}">',
        '<rect width="100%" height="100%" fill="white"/>',
        '<text x="20" y="20" font-size="11" font-family="sans-serif" fill="#333">'
        'Top: pen strokes (joined contours, caps).  '
        'Bottom: outline parallelograms (separate shapes).</text>',
    ]

    scale = 0.065
    chars = [c for c in CONSONANT_ORDER if c in PEN_CHARS and c in SECONDARY]

    cols = 11
    for idx, ch in enumerate(chars):
        col = idx % cols
        row = idx // cols
        x_off = 20 + col * 85
        label = ch if ch != "'" else "'"

        # Pen version (top)
        pen = Pen(width=SW)
        PEN_CHARS[ch](pen)
        path_d = pen.to_path()
        y_top = 35 + row * 260
        transform = f'translate({x_off},{y_top + 55}) scale({scale},{-scale})'
        svg.append(f'<g transform="{transform}">'
                   f'<path d="{path_d}" fill="#2a4858" fill-rule="nonzero"/></g>')

        # Outline version (bottom)
        glyph = SECONDARY[ch]
        y_bot = 35 + row * 260 + 100
        transform = f'translate({x_off},{y_bot + 55}) scale({scale},{-scale})'
        svg.append(f'<g transform="{transform}">'
                   f'<path d="{glyph["path"]}" fill="#8b4513" fill-rule="nonzero"/></g>')

        svg.append(f'<text x="{x_off + 15}" y="{y_top + 68}" text-anchor="middle" '
                   f'font-size="8" fill="#666">{label}</text>')

    svg.append('</svg>')

    output = 'script/pen_comparison.svg'
    with open(output, 'w') as f:
        f.write('\n'.join(svg))
    print(f'Wrote {output} ({len(chars)} characters)')


if __name__ == '__main__':
    render_comparison()
