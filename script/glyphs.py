#!/usr/bin/env python3
"""
Ithkuil V4 Script - Vector Glyph Definitions

All paths use a 1000-unit em square:
- Origin (0,0) at bottom-left, y increases upward
- Character body: roughly x=50-450, y=100-900
- Stroke width: ~50 units (paths are filled outlines)

For SVG rendering, y is flipped (y-down). The render functions handle this.
"""
import math

SW = 50  # stroke width half = 25 on each side

def _outline(x1, y1, x2, y2, w=SW):
    """Create a filled parallelogram outline for a line segment."""
    dx, dy = x2 - x1, y2 - y1
    length = math.sqrt(dx*dx + dy*dy)
    if length < 1:
        return ""
    hw = w / 2
    nx, ny = -dy/length * hw, dx/length * hw
    return (f"M{x1+nx:.0f},{y1+ny:.0f} L{x2+nx:.0f},{y2+ny:.0f} "
            f"L{x2-nx:.0f},{y2-ny:.0f} L{x1-nx:.0f},{y1-ny:.0f} Z")

def _arc(cx, cy, r, a1_deg, a2_deg, w=SW):
    """Create a filled arc outline (annular sector)."""
    a1, a2 = math.radians(a1_deg), math.radians(a2_deg)
    ro, ri = r + w/2, max(1, r - w/2)
    # Start/end points for outer and inner arcs
    x1o, y1o = cx + ro * math.cos(a1), cy + ro * math.sin(a1)
    x2o, y2o = cx + ro * math.cos(a2), cy + ro * math.sin(a2)
    x1i, y1i = cx + ri * math.cos(a1), cy + ri * math.sin(a1)
    x2i, y2i = cx + ri * math.cos(a2), cy + ri * math.sin(a2)
    sweep_angle = abs(a2_deg - a1_deg)
    large = 1 if sweep_angle > 180 else 0
    sweep = 1 if a2_deg > a1_deg else 0
    return (f"M{x1o:.0f},{y1o:.0f} A{ro:.0f},{ro:.0f} 0 {large},{sweep} {x2o:.0f},{y2o:.0f} "
            f"L{x2i:.0f},{y2i:.0f} A{ri:.0f},{ri:.0f} 0 {large},{1-sweep} {x1i:.0f},{y1i:.0f} Z")


# ============================================================================
# Secondary Characters - Print Forms
# ============================================================================
# Each glyph: list of path-data strings, combined into one 'd' attribute.
# Coordinates: bottom-left origin, y-up. Body in ~500w x 800h box.
# Top attachment point: ~(250, 850), Bottom: ~(250, 100)

# Map consonants to ASCII-safe glyph names (for font post table)
_ASCII_NAMES = {
    'ţ': 'tc', 'ḑ': 'dh', 'š': 'sh', 'ž': 'zh', 'ç': 'cy',
    'ļ': 'lh', 'ẓ': 'zd', 'č': 'ch', 'ň': 'ny', 'ř': 'rh',
    "'": 'glottal',
}

def _glyph(name, cons, parts, w=500):
    ascii_name = _ASCII_NAMES.get(name, name)
    return {'name': f'sec_{ascii_name}', 'consonant': cons, 'width': w,
            'path': ' '.join(p for p in parts if p)}

# Shorthand
L = _outline  # line
A = _arc      # arc

SECONDARY = {}

# p: Top horizontal, vertical down, bottom curves right (like Γ with J-foot)
SECONDARY['p'] = _glyph('p', 'p', [
    L(100, 800, 350, 800),       # top horizontal
    L(100, 800, 100, 350),       # vertical stem down
    A(250, 350, 150, 180, 270),  # bottom curve sweeping right
])

# b: Mirror of p (top horizontal, vertical right side, bottom curves left)
SECONDARY['b'] = _glyph('b', 'b', [
    L(100, 800, 350, 800),       # top horizontal
    L(350, 800, 350, 350),       # vertical stem down (right)
    A(200, 350, 150, 270, 360),  # bottom curve sweeping left
])

# f: S-shape / reversed-5. Top horizontal, down left, horizontal mid, down right
SECONDARY['f'] = _glyph('f', 'f', [
    L(100, 800, 400, 800),       # top horizontal
    L(100, 800, 100, 550),       # left vertical upper
    L(100, 550, 350, 550),       # middle horizontal
    L(350, 550, 350, 200),       # right vertical lower
    A(350, 200, 150, -90, 180),  # bottom curve
])

# v: Mirror of f (like a 5)
SECONDARY['v'] = _glyph('v', 'v', [
    L(100, 800, 400, 800),       # top horizontal
    L(400, 800, 400, 550),       # right vertical upper
    L(150, 550, 400, 550),       # middle horizontal
    L(150, 550, 150, 200),       # left vertical lower
    A(150, 200, 150, 0, 270),    # bottom curve
])

# s: Zigzag/chevron - diagonal down-right, then diagonal down-left
SECONDARY['s'] = _glyph('s', 's', [
    L(100, 800, 350, 550),       # upper diagonal down-right
    L(350, 550, 100, 300),       # lower diagonal down-left
])

# z: Mirror zigzag - diagonal down-left, then diagonal down-right
SECONDARY['z'] = _glyph('z', 'z', [
    L(350, 800, 100, 550),       # upper diagonal down-left
    L(100, 550, 350, 300),       # lower diagonal down-right
])

# c: L-shape (vertical + horizontal at bottom)
SECONDARY['c'] = _glyph('c', 'c', [
    L(100, 800, 100, 200),       # vertical stem
    L(100, 200, 400, 200),       # bottom horizontal
])

# ẓ: L-shape with top serif (like c but with mark at top)
SECONDARY['ẓ'] = _glyph('ẓ', 'ẓ', [
    L(100, 800, 100, 200),       # vertical stem
    L(100, 200, 400, 200),       # bottom horizontal
    L(100, 800, 250, 800),       # top serif (shorter)
])

# t: Γ shape (vertical + horizontal at top)
SECONDARY['t'] = _glyph('t', 't', [
    L(100, 800, 100, 200),       # vertical stem
    L(100, 800, 400, 800),       # top horizontal
])

# d: Reversed Γ with thicker strokes / diagonal feel
SECONDARY['d'] = _glyph('d', 'd', [
    L(400, 800, 400, 200),       # vertical stem (right side)
    L(100, 800, 400, 800),       # top horizontal
    L(400, 200, 250, 200),       # bottom serif
])

# ţ: C-curve opening right (like left parenthesis)
SECONDARY['ţ'] = _glyph('ţ', 'ţ', [
    A(350, 500, 250, 120, 240),  # arc opening right, ~240 degrees
])

# ḑ: C-curve opening left (like right parenthesis)
SECONDARY['ḑ'] = _glyph('ḑ', 'ḑ', [
    A(150, 500, 250, -60, 60),   # arc opening left
])

# š: "2"-like shape - arc at top-right, diagonal down-left, horizontal bottom
SECONDARY['š'] = _glyph('š', 'š', [
    A(250, 650, 150, 30, 180),   # top-right arc
    L(100, 650, 350, 250),       # diagonal down-right
    L(100, 250, 350, 250),       # bottom horizontal
])

# ž: Mirror "2" - arc at top-left, diagonal down-right
SECONDARY['ž'] = _glyph('ž', 'ž', [
    A(250, 650, 150, 0, 150),    # top-left arc
    L(400, 650, 150, 250),       # diagonal down-left
    L(150, 250, 400, 250),       # bottom horizontal
])

# č: Square bracket [ shape
SECONDARY['č'] = _glyph('č', 'č', [
    L(100, 800, 100, 200),       # vertical left
    L(100, 800, 350, 800),       # top horizontal
    L(100, 200, 350, 200),       # bottom horizontal
])

# j: Reversed bracket ] shape
SECONDARY['j'] = _glyph('j', 'j', [
    L(400, 800, 400, 200),       # vertical right
    L(150, 800, 400, 800),       # top horizontal
    L(150, 200, 400, 200),       # bottom horizontal
])

# k: Angled shape - horizontal top, diagonal down-left, vertical down
SECONDARY['k'] = _glyph('k', 'k', [
    L(100, 800, 400, 800),       # top horizontal
    L(400, 800, 400, 500),       # right vertical stub
    L(400, 500, 100, 200),       # diagonal down-left
])

# g: Mirror of k
SECONDARY['g'] = _glyph('g', 'g', [
    L(100, 800, 400, 800),       # top horizontal
    L(100, 800, 100, 500),       # left vertical stub
    L(100, 500, 400, 200),       # diagonal down-right
])

# x: Z-shape (top horiz, diagonal, bottom horiz)
SECONDARY['x'] = _glyph('x', 'x', [
    L(100, 800, 400, 800),       # top horizontal
    L(400, 800, 100, 200),       # diagonal
    L(100, 200, 400, 200),       # bottom horizontal
])

# l: Diagonal top-right to mid, vertical mid to bottom, horizontal right at bottom
SECONDARY['l'] = _glyph('l', 'l', [
    L(100, 800, 250, 500),       # upper diagonal
    L(250, 500, 250, 200),       # vertical stem
    L(250, 200, 450, 200),       # bottom horizontal
])

# r: Step shape - like reversed Z or staircase
SECONDARY['r'] = _glyph('r', 'r', [
    L(100, 800, 350, 800),       # top horizontal
    L(350, 800, 350, 550),       # right vertical upper
    L(150, 550, 350, 550),       # middle horizontal
    L(150, 550, 150, 200),       # left vertical lower
])

# ļ: V-chevron pointing down
SECONDARY['ļ'] = _glyph('ļ', 'ļ', [
    L(100, 800, 250, 400),       # left diagonal down
    L(250, 400, 400, 800),       # right diagonal up
])

# ř: Loop/hook at top curving right, straight stem below
SECONDARY['ř'] = _glyph('ř', 'ř', [
    A(200, 650, 150, 0, 270),    # top loop (3/4 circle)
    L(200, 500, 200, 200),       # vertical stem below
])

# m: Single diagonal stroke (top-left to bottom-right)
SECONDARY['m'] = _glyph('m', 'm', [
    L(100, 800, 350, 200),       # diagonal
])

# n: Diagonal with bottom horizontal serif
SECONDARY['n'] = _glyph('n', 'n', [
    L(100, 800, 300, 200),       # diagonal
    L(300, 200, 450, 200),       # bottom serif
])

# ň: Diagonal with rightward kick at bottom
SECONDARY['ň'] = _glyph('ň', 'ň', [
    L(100, 800, 300, 350),       # upper diagonal
    L(300, 350, 300, 200),       # short vertical
    L(300, 350, 450, 250),       # rightward kick
])

# w: Diagonal with bottom-right curve
SECONDARY['w'] = _glyph('w', 'w', [
    L(100, 800, 250, 450),       # upper diagonal
    A(250, 300, 150, 90, 0),     # bottom-right quarter curve
])

# y: Diagonal with bottom-left curve
SECONDARY['y'] = _glyph('y', 'y', [
    L(350, 800, 200, 450),       # upper diagonal (from right)
    A(200, 300, 150, 90, 180),   # bottom-left quarter curve
])

# ç: Sigma shape (horiz, diag-in, diag-out, horiz)
SECONDARY['ç'] = _glyph('ç', 'ç', [
    L(100, 800, 400, 800),       # top horizontal
    L(100, 800, 250, 500),       # upper diagonal inward
    L(250, 500, 100, 200),       # lower diagonal outward
    L(100, 200, 400, 200),       # bottom horizontal
])

# h: Like a "4" - diagonal, vertical stem, crossbar
SECONDARY['h'] = _glyph('h', 'h', [
    L(100, 800, 300, 450),       # upper diagonal
    L(300, 450, 300, 200),       # vertical stem
    L(150, 450, 450, 450),       # crossbar
])

# ' (glottal stop): Short vertical tick
SECONDARY["'"] = _glyph('glottal', "'", [
    L(200, 700, 200, 400),       # short vertical
])

# Placeholder: Two parallel diagonals
SECONDARY['placeholder'] = _glyph('placeholder', '', [
    L(100, 800, 200, 200),       # left diagonal
    L(250, 800, 350, 200),       # right diagonal
])


# ============================================================================
# Unicode PUA Mapping
# ============================================================================
PUA_SECONDARY = 0xE000  # U+E000-E01F: secondary chars

CONSONANT_ORDER = [
    'p', 'b', 't', 'd', 'k', 'g',
    'f', 'v', 'ţ', 'ḑ', 's', 'z',
    'š', 'ž', 'ç', 'x', 'h', 'ļ',
    'c', 'ẓ', 'č', 'j', 'm', 'n',
    'ň', 'r', 'l', 'w', 'y', 'ř', "'",
]

for i, cons in enumerate(CONSONANT_ORDER):
    if cons in SECONDARY:
        SECONDARY[cons]['codepoint'] = PUA_SECONDARY + i

SECONDARY['placeholder']['codepoint'] = PUA_SECONDARY + 0x1F


# ============================================================================
# SVG Rendering
# ============================================================================

def render_glyph_svg(glyph, x_off=0, y_off=0, scale=0.1):
    """Render glyph as SVG group. Flips y for SVG (y-down)."""
    transform = f'translate({x_off},{y_off}) scale({scale},{-scale}) translate(0,-1000)'
    return f'<g transform="{transform}"><path d="{glyph["path"]}" fill="black" fill-rule="nonzero"/></g>'


def render_test_sheet(filename='script/secondary_chars_test.svg'):
    """Render all secondary characters to SVG test sheet."""
    cols, cell_w, cell_h, margin = 8, 100, 130, 15
    rows = (len(CONSONANT_ORDER) + cols - 1) // cols
    W = cols * cell_w + 2 * margin
    H = rows * cell_h + 2 * margin

    svg = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" '
           f'viewBox="0 0 {W} {H}">', '<rect width="100%" height="100%" fill="white"/>']

    for idx, cons in enumerate(CONSONANT_ORDER):
        if cons not in SECONDARY:
            continue
        col, row = idx % cols, idx // cols
        x, y = margin + col * cell_w, margin + row * cell_h
        label = cons if cons != "'" else "&#x2019;"
        svg.append(f'<rect x="{x}" y="{y}" width="{cell_w}" height="{cell_h}" '
                   f'fill="none" stroke="#ddd" stroke-width="0.5"/>')
        svg.append(f'<text x="{x+cell_w//2}" y="{y+12}" text-anchor="middle" '
                   f'font-size="10" fill="#666">{label}</text>')
        svg.append(render_glyph_svg(SECONDARY[cons], x + 5, y + 18 + 90, 0.09))

    svg.append('</svg>')
    with open(filename, 'w') as f:
        f.write('\n'.join(svg))
    print(f'Wrote {filename} ({len(CONSONANT_ORDER)} glyphs)')


# ============================================================================
# Quaternary Character Shapes (Case Type + Case Number)
# ============================================================================
# From reference image 12_4_quaternary_chars.png:
# Case Type (top extension): 8 distinct shapes for 8 case groups
# Case Number (bottom extension): 9 shapes for cases 1-9 within group
# Illocution (top, for Vk): 9 shapes
# Validation (bottom, for Vk with ASR): 9 shapes
#
# Base form is a vertical stem. Extensions branch from top or bottom.

QUATERNARY_CASE_TYPE = {}
# Each case type is a top extension shape on a vertical stem
# The shapes from the reference: increasingly complex curves/hooks

for i, name in enumerate(['TRANSRELATIVE', 'APPOSITIVE', 'ASSOCIATIVE', 'ADVERBIAL',
                          'RELATIONAL', 'AFFINITIVE', 'SPATIO_TEMP_I', 'SPATIO_TEMP_II']):
    # Simple representation: vertical stem + angled extension
    angle = 30 + i * 15  # Different angles for different types
    ext_len = 200
    ex = 250 + ext_len * math.cos(math.radians(angle))
    ey = 800 + ext_len * math.sin(math.radians(angle))
    QUATERNARY_CASE_TYPE[name] = _glyph(f'quat_type_{name}', '', [
        L(250, 200, 250, 800),       # vertical stem
        L(250, 800, ex, ey),          # top extension at angle
    ])

QUATERNARY_CASE_NUM = {}
for i in range(1, 10):
    # Bottom extensions: different curves/angles
    angle = -(30 + i * 12)
    ext_len = 180
    ex = 250 + ext_len * math.cos(math.radians(angle))
    ey = 200 + ext_len * math.sin(math.radians(angle))
    QUATERNARY_CASE_NUM[i] = _glyph(f'quat_num_{i}', '', [
        L(250, 200, ex, ey),          # bottom extension
    ])


# ============================================================================
# Tertiary Character Shapes (Valence/Phase/Aspect/Effect/Level)
# ============================================================================
# From reference: arrow-like shapes with horizontal valence bar as base.
# Aspect shapes are the most numerous (36 distinct forms).
# For now, define the base structure.

TERTIARY_VALENCE = {}
VALENCE_NAMES = ['MNO', 'PRL', 'CRO', 'RCP', 'CPL', 'DUP', 'DEM', 'CNG', 'PTI']
for i, name in enumerate(VALENCE_NAMES):
    # Horizontal bar with varying right-side arrow/decoration
    bar_y = 500
    arrow_dx = 50 + i * 30
    TERTIARY_VALENCE[name] = _glyph(f'tert_val_{name}', '', [
        L(50, bar_y, 400, bar_y),                    # horizontal bar
        L(400, bar_y, 400 + arrow_dx, bar_y + 80),   # arrow up
        L(400, bar_y, 400 + arrow_dx, bar_y - 80),   # arrow down
    ])


if __name__ == '__main__':
    render_test_sheet()
