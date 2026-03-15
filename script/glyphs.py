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
# Primary Character Components
# ============================================================================

# ---- Specification (4 forms, lower-left zone): U+E060-E063 ----
PRIMARY_SPEC = {}
_spec_shapes = {
    'BSC': [L(50, 200, 200, 600)],
    'CTE': [L(80, 250, 180, 500)],
    'CSV': [L(50, 400, 150, 250), L(150, 250, 50, 200)],
    'OBJ': [L(50, 250, 150, 400), L(150, 400, 50, 450)],
}
for i, (name, parts) in enumerate(_spec_shapes.items()):
    PRIMARY_SPEC[name] = _glyph(f'prim_spec_{name}', '', parts, 250)
    PRIMARY_SPEC[name]['codepoint'] = 0xE060 + i

# ---- Context (4 superposed diacritics): U+E064-E067 ----
PRIMARY_CTX = {}
_ctx_shapes = {
    'EXS': [L(190, 920, 210, 920), L(190, 900, 210, 900)],
    'FNC': [L(170, 910, 230, 910)],
    'RPS': [L(175, 920, 225, 900)],
    'AMG': [L(225, 920, 175, 900)],
}
for i, (name, parts) in enumerate(_ctx_shapes.items()):
    PRIMARY_CTX[name] = _glyph(f'prim_ctx_{name}', '', parts, 0)
    PRIMARY_CTX[name]['codepoint'] = 0xE064 + i

# ---- Perspective x Extension (24 forms, upper-left): U+E068-E07F ----
PERSP_NAMES = ['M', 'G', 'N', 'A']
EXT_NAMES = ['DEL', 'PRX', 'ICP', 'ATV', 'GRA', 'DPL']
PRIMARY_PERSP_EXT = {}
for pi, persp in enumerate(PERSP_NAMES):
    for ei, ext in enumerate(EXT_NAMES):
        key = f'{persp}_{ext}'
        idx = pi * 6 + ei
        base_x, base_y = 100, 700
        p_angle = [0, 30, -30, 15][pi]
        e_dx = [80, 60, 100, 70, 90, 50][ei]
        e_dy = [0, 40, -40, 60, -60, 80][ei]
        parts = [L(base_x, base_y, base_x + e_dx, base_y + e_dy + p_angle)]
        if ei >= 3:
            parts.append(L(base_x + e_dx, base_y + e_dy + p_angle,
                          base_x + e_dx + 20, base_y + e_dy + p_angle - 20))
        PRIMARY_PERSP_EXT[key] = _glyph(f'prim_pe_{idx}', '', parts, 0)
        PRIMARY_PERSP_EXT[key]['codepoint'] = 0xE068 + idx

# ---- Affiliation x Essence (8 forms, upper-right): U+E080-E087 ----
PRIMARY_AFFIL_ESS = {}
_ae_shapes = {
    ('CSL', 'NRM'): [L(350, 700, 420, 700)],
    ('ASO', 'NRM'): [L(350, 720, 420, 680)],
    ('COA', 'NRM'): [L(370, 720, 370, 680)],
    ('VAR', 'NRM'): [L(350, 710, 420, 710)],
    ('CSL', 'RPV'): [L(350, 700, 420, 700), L(400, 720, 420, 680)],
    ('ASO', 'RPV'): [L(370, 720, 370, 680), L(360, 720, 380, 720)],
    ('COA', 'RPV'): [A(385, 700, 30, 0, 180)],
    ('VAR', 'RPV'): [A(385, 700, 30, 0, 300)],
}
for idx, ((affil, ess), parts) in enumerate(_ae_shapes.items()):
    key = f'{affil}_{ess}'
    PRIMARY_AFFIL_ESS[key] = _glyph(f'prim_ae_{idx}', '', parts, 0)
    PRIMARY_AFFIL_ESS[key]['codepoint'] = 0xE080 + idx

# ---- Configuration (10 underposed marks): U+E088-E091 ----
CONFIG_NAMES_SHORT = ['PX', 'SS', 'SC', 'SF', 'DS', 'DC', 'DF', 'FS', 'FC', 'FF']
PRIMARY_CONFIG = {}
_cfg_shapes = {
    'PX': [L(180, 50, 220, 50)],
    'SS': [L(170, 50, 230, 50), L(170, 30, 230, 30)],
    'SC': [L(170, 50, 200, 30), L(200, 30, 230, 50)],
    'SF': [L(170, 40, 230, 40)],
    'DS': [L(180, 50, 200, 20), L(200, 20, 220, 50)],
    'DC': [A(200, 40, 25, 0, 180)],
    'DF': [A(200, 40, 25, 180, 360)],
    'FS': [L(170, 50, 230, 50), L(200, 50, 200, 20)],
    'FC': [L(170, 50, 230, 50), A(200, 30, 15, 0, 180)],
    'FF': [A(200, 40, 20, 0, 350)],
}
for i, name in enumerate(CONFIG_NAMES_SHORT):
    PRIMARY_CONFIG[name] = _glyph(f'prim_cfg_{name}', '', _cfg_shapes[name], 0)
    PRIMARY_CONFIG[name]['codepoint'] = 0xE088 + i

# ---- Stem/Function/Version/Plexity (64 forms, lower-right): U+E092-E0D1 ----
PRIMARY_SFVP = {}
_sfvp_idx = 0
for si, stem in enumerate(['S1', 'S2', 'S3', 'S0']):
    for fi, func in enumerate(['STA', 'DYN']):
        for vi, ver in enumerate(['PRC', 'CPT']):
            for pi, plex in enumerate(['UM', 'D']):
                key = f'{stem}_{func}_{ver}_{plex}'
                x0, y0 = 300, 200
                dx = 80 if func == 'STA' else 60
                dy = 80 if func == 'STA' else 100
                parts = [L(x0, y0, x0 + dx, y0 + dy)]
                for tick in range(si + 1 if si < 3 else 0):
                    tx = x0 + 15 + tick * 12
                    parts.append(L(tx, y0 + dy + 5, tx, y0 + dy + 15))
                if ver == 'CPT':
                    parts.append(L(x0 + dx + 5, y0 + dy - 5, x0 + dx + 15, y0 + dy - 5))
                if plex == 'D':
                    parts.append(L(x0 + 10, y0 + 5, x0 + dx - 10, y0 + 5))
                PRIMARY_SFVP[key] = _glyph(f'prim_sfvp_{_sfvp_idx}', '', parts, 0)
                PRIMARY_SFVP[key]['codepoint'] = 0xE092 + _sfvp_idx
                _sfvp_idx += 1

# ---- Relation (3 subscript marks): U+E0D2-E0D4 ----
PRIMARY_RELATION = {}
for i, (name, parts) in enumerate({
    'NOUN': [],
    'UNFRAMED_VERB': [L(190, -30, 210, -30), L(190, -50, 210, -50)],
    'FRAMED_VERB': [L(170, -40, 230, -40)],
}.items()):
    PRIMARY_RELATION[name] = _glyph(f'prim_rel_{name}', '', parts, 0)
    PRIMARY_RELATION[name]['codepoint'] = 0xE0D2 + i

# ---- Concatenation (2 subscript marks): U+E0D5-E0D6 ----
PRIMARY_CONCAT = {}
for i, (name, parts) in enumerate({
    'TYPE1': [L(185, -30, 200, -60), L(200, -60, 215, -30)],
    'TYPE2': [L(180, -30, 195, -50), L(195, -50, 210, -30), L(210, -30, 225, -50)],
}.items()):
    PRIMARY_CONCAT[name] = _glyph(f'prim_cat_{name}', '', parts, 0)
    PRIMARY_CONCAT[name]['codepoint'] = 0xE0D5 + i


# ============================================================================
# Tertiary Character Shapes (Valence/Phase/Aspect/Effect/Level)
# ============================================================================

# ---- Valence (9 forms): horizontal arrow bar, different arrowheads ----
# Each is a right-pointing arrow with a distinctive head shape.
# PUA: U+E100-E108
TERTIARY_VALENCE = {}
VALENCE_NAMES = ['MNO', 'PRL', 'CRO', 'RCP', 'CPL', 'DUP', 'DEM', 'CNG', 'PTI']
_val_base = lambda: L(50, 500, 350, 500)  # horizontal shaft

_val_heads = {
    'MNO': [L(350, 500, 450, 560), L(350, 500, 450, 440)],              # simple arrowhead
    'PRL': [L(350, 500, 450, 560), L(350, 500, 450, 440), L(300, 560, 300, 440)],  # arrow + back notch
    'CRO': [L(350, 500, 450, 560), L(350, 500, 450, 440), A(300, 500, 60, 90, 270)],  # arrow + hook
    'RCP': [L(350, 500, 430, 580), L(350, 500, 430, 420), L(430, 580, 430, 420)],  # triangle head
    'CPL': [L(350, 500, 450, 560), L(350, 500, 450, 440), L(380, 560, 380, 440)],  # arrow + bar
    'DUP': [L(350, 500, 450, 540), L(350, 500, 450, 460), L(320, 540, 320, 460)],  # narrow arrow + bar
    'DEM': [L(350, 500, 450, 560), L(350, 500, 450, 440), L(350, 440, 320, 400)],  # arrow + down-tick
    'CNG': [L(350, 500, 450, 560), L(350, 500, 450, 440), L(350, 560, 320, 600)],  # arrow + up-tick
    'PTI': [L(350, 500, 430, 570), L(350, 500, 430, 430), L(430, 570, 480, 540), L(430, 430, 480, 460)],  # double hook
}
for i, name in enumerate(VALENCE_NAMES):
    parts = [_val_base()] + _val_heads[name]
    TERTIARY_VALENCE[name] = _glyph(f'tert_val_{name}', '', parts)
    TERTIARY_VALENCE[name]['codepoint'] = 0xE100 + i


# ---- Aspect (36 forms): arrow-like shapes ----
# Each aspect is a distinctive arrow/chevron shape.
# PUA: U+E11B-E13E
ASPECT_NAMES = [
    'RTR', 'PRS', 'HAB', 'PRG', 'IMM', 'PCS', 'REG', 'SMM', 'ATP',    # column 1: timeline
    'RSM', 'CSS', 'PAU', 'RGR', 'PCL', 'CNT', 'ICS', 'EXP', 'IRP',    # column 2: shape/structure
    'PMP', 'CLM', 'DLT', 'TMP', 'XPD', 'LIM', 'EPD', 'PTC', 'PPR',    # column 3: consequence
    'DCL', 'CCL', 'CUL', 'IMD', 'TRD', 'TNS', 'ITC', 'MTV', 'SQN',    # column 4: misc
]

TERTIARY_ASPECT = {}
for i, name in enumerate(ASPECT_NAMES):
    # Generate distinct arrow shapes using systematic variations
    col = i // 9       # 0-3: which column (determines base direction)
    row = i % 9        # 0-8: which row (determines detail)

    # Base arrow direction varies by column
    base_dx = [350, 300, 350, 300][col]
    base_dy = [0, 50, -50, 0][col]
    y_mid = 500

    # Head shape varies by row
    head_spread = 40 + row * 8
    head_len = 60 + row * 5
    tail_mod = row * 15

    parts = [
        L(50, y_mid + base_dy, base_dx, y_mid),  # shaft
        L(base_dx, y_mid, base_dx + head_len, y_mid + head_spread),  # upper head
        L(base_dx, y_mid, base_dx + head_len, y_mid - head_spread),  # lower head
    ]

    # Add distinctive tail features per column
    if col == 0:  # timeline: back-pointing tail
        parts.append(L(50, y_mid + base_dy, 50 - tail_mod//2, y_mid + 30))
    elif col == 1:  # structure: notched tail
        parts.append(L(80, y_mid + base_dy + 30, 80, y_mid + base_dy - 30))
    elif col == 2:  # consequence: forked tail
        parts.append(L(50, y_mid + base_dy, 20, y_mid + 40))
        parts.append(L(50, y_mid + base_dy, 20, y_mid - 40))
    elif col == 3:  # misc: barred tail
        parts.append(L(70, y_mid + 25, 70, y_mid - 25))

    TERTIARY_ASPECT[name] = _glyph(f'tert_asp_{name}', '', parts)
    TERTIARY_ASPECT[name]['codepoint'] = 0xE11B + i


# ---- Phase (9 forms) ----
# PUA: U+E109-E111
PHASE_NAMES = ['PCT', 'ITR', 'REP', 'ITM', 'RCT', 'FRE', 'FRG', 'VAC', 'FLC']
TERTIARY_PHASE = {}
for i, name in enumerate(PHASE_NAMES):
    # Phase forms: vertical double-stroke patterns
    x_base = 200
    spread = 30 + i * 8
    TERTIARY_PHASE[name] = _glyph(f'tert_phs_{name}', '', [
        L(x_base - spread, 300, x_base - spread, 700),
        L(x_base + spread, 300, x_base + spread, 700),
        L(x_base - spread, 700, x_base, 750 + i * 5),  # varying bottom connection
        L(x_base + spread, 700, x_base, 750 + i * 5),
    ])
    TERTIARY_PHASE[name]['codepoint'] = 0xE109 + i


# ---- Effect (9 forms) ----
# PUA: U+E112-E11A
EFFECT_NAMES = ['neutral', '1BEN', '2BEN', '3BEN', 'SLF_BEN', 'UNKNOWN', 'SLF_DET', '3DET', '2DET', '1DET']
# Actually there are 10 effect forms per the reference, but the spec shows 9 distinct
# Using neutral + 4 benefactive + unknown + 4 detrimental = 10
TERTIARY_EFFECT = {}
for i, name in enumerate(EFFECT_NAMES[:9]):
    safe_name = name.replace('/', '_')
    # Effect forms: downward-pointing marks of varying size
    h = 100 + i * 20
    TERTIARY_EFFECT[name] = _glyph(f'tert_eff_{safe_name}', '', [
        L(180, 700, 220, 700),            # top bar
        L(200, 700, 200, 700 - h),        # vertical extent
    ])
    TERTIARY_EFFECT[name]['codepoint'] = 0xE112 + i


# ---- Level diacritics (9 forms, superposed or underposed on tertiary) ----
# PUA: U+E13F-E147
LEVEL_NAMES = ['MIN', 'SBE', 'IFR', 'DFT', 'EQU', 'SUR', 'SPL', 'SPQ', 'MAX']
TERTIARY_LEVEL = {}
for i, name in enumerate(LEVEL_NAMES):
    # Small marks: diamond, crescents, arrows of varying size
    TERTIARY_LEVEL[name] = _glyph(f'tert_lvl_{name}', '', [
        L(180, 900 + i * 3, 220, 900 + i * 3),
    ], 0)  # combining (0 width)
    TERTIARY_LEVEL[name]['codepoint'] = 0xE13F + i


# ============================================================================
# Quaternary Character Shapes (refined)
# ============================================================================

# Case Type top extensions (8): U+E180-E187
QUAT_CASE_TYPE = {}
_qcase_type_names = ['TRANS', 'APPOS', 'ASSOC', 'ADVERB', 'RELAT', 'AFFIN', 'SPAT1', 'SPAT2']
_qcase_type_shapes = {
    'TRANS':  [L(200, 200, 200, 900)],                                     # plain vertical
    'APPOS':  [L(200, 200, 200, 900), L(200, 900, 320, 820)],             # right hook
    'ASSOC':  [L(200, 200, 200, 900), L(200, 900, 300, 850), L(200, 900, 100, 850)],  # fork
    'ADVERB': [L(200, 200, 200, 900), A(200, 800, 100, 0, 90)],           # right curve
    'RELAT':  [L(200, 200, 200, 900), L(200, 900, 80, 820)],              # left hook
    'AFFIN':  [L(200, 200, 200, 900), L(100, 900, 350, 900)],             # T-bar
    'SPAT1':  [L(200, 200, 200, 900), L(100, 900, 350, 900), L(350, 900, 380, 830)],  # T + right hook
    'SPAT2':  [L(200, 200, 200, 900), L(100, 900, 350, 900), L(100, 900, 70, 830)],   # T + left hook
}
for i, name in enumerate(_qcase_type_names):
    QUAT_CASE_TYPE[name] = _glyph(f'quat_type_{name}', '', _qcase_type_shapes[name], 400)
    QUAT_CASE_TYPE[name]['codepoint'] = 0xE180 + i

# Case Number bottom extensions (9): U+E188-E190
QUAT_CASE_NUM = {}
for i in range(1, 10):
    # Different bottom extension angles/curves for cases 1-9
    angle = 210 + (i-1) * 15  # 210° to 330° (sweeping around bottom)
    elen = 180
    ex = int(200 + elen * math.cos(math.radians(angle)))
    ey = int(200 + elen * math.sin(math.radians(angle)))
    QUAT_CASE_NUM[i] = _glyph(f'quat_num_{i}', '', [L(200, 200, ex, ey)], 400)
    QUAT_CASE_NUM[i]['codepoint'] = 0xE188 + i - 1

# Illocution (9): U+E191-E199
ILLOCUTION_NAMES = ['ASR', 'DIR', 'DEC', 'IRG', 'VRF', 'ADM', 'POT', 'HOR', 'CNJ']
QUAT_ILLOCUTION = {}
for i, name in enumerate(ILLOCUTION_NAMES):
    # Tall vertical with distinctive top curve/hook
    hook_dx = [-20, 80, -80, 60, -60, 40, -40, 100, -100][i]
    hook_dy = [0, -40, -40, -60, -60, -30, -30, -50, -50][i]
    QUAT_ILLOCUTION[name] = _glyph(f'quat_illoc_{name}', '', [
        L(200, 100, 200, 900),
        L(200, 900, 200 + hook_dx, 900 + hook_dy),
    ], 400)
    QUAT_ILLOCUTION[name]['codepoint'] = 0xE191 + i

# Validation (9): U+E19A-E1A2
VALIDATION_NAMES = ['OBS', 'REC', 'PUP', 'RPR', 'IMA', 'CVN', 'ITU', 'INF', 'USP']
QUAT_VALIDATION = {}
for i, name in enumerate(VALIDATION_NAMES):
    hook_dx = [0, 60, -60, 40, -40, 80, -80, 50, -50][i]
    QUAT_VALIDATION[name] = _glyph(f'quat_valid_{name}', '', [
        L(200, 100, 200, 700),
        L(200, 100, 200 + hook_dx, 50),
    ], 400)
    QUAT_VALIDATION[name]['codepoint'] = 0xE19A + i


# ============================================================================
# Bias Characters (61 forms): U+E220-E25C
# ============================================================================
# Each bias is a small arrow/chevron mark. From the reference image,
# they come in 4 base forms (columns) with variations.
# The 4 base forms: ACC-like (diagonal), DCC-like (curved), FSC-like (angular), PSM-like (barred)

BIAS_NAMES = [
    'ACC', 'ACH', 'ADS', 'ANN', 'ANP', 'APB', 'APH', 'ARB', 'ATE',
    'CMD', 'CNV', 'COI', 'CRP', 'CRR', 'CTP', 'CTV', 'DCC', 'DEJ',
    'DES', 'DFD', 'DIS', 'DLC', 'DOL', 'DPB', 'DRS', 'DUB', 'EUH',
    'EUP', 'EXA', 'EXG', 'MNF', 'FOR', 'FSC', 'GRT', 'IDG', 'IFT',
    'IPL', 'IPT', 'IRO', 'ISP', 'IVD', 'MAN', 'OPT', 'PES', 'PPT',
    'PPX', 'PPV', 'PSC', 'PSM', 'RAC', 'RFL', 'RSG', 'RPU', 'RVL',
    'SAT', 'SGS', 'SKP', 'SOL', 'STU', 'TRP', 'VEX',
]

BIAS_GLYPHS = {}
for i, name in enumerate(BIAS_NAMES):
    # Generate systematic variations: 4 base shapes rotated/reflected
    base = i % 4
    variant = i // 4
    cx, cy = 200, 500
    # Base shape type
    if base == 0:   # diagonal stroke
        parts = [L(cx - 60, cy + 40, cx + 60, cy - 40)]
        parts.append(L(cx + 60 - variant*5, cy - 40, cx + 80, cy - 20 + variant*8))
    elif base == 1:  # curved stroke
        parts = [A(cx, cy, 50 + variant*3, 30, 210)]
    elif base == 2:  # angular chevron
        parts = [L(cx - 50, cy, cx, cy + 30 + variant*5), L(cx, cy + 30 + variant*5, cx + 50, cy)]
    else:            # barred stroke
        parts = [L(cx - 40, cy, cx + 40, cy), L(cx, cy - 30 - variant*4, cx, cy + 30 + variant*4)]
    BIAS_GLYPHS[name] = _glyph(f'bias_{name}', '', parts, 400)
    BIAS_GLYPHS[name]['codepoint'] = 0xE220 + i


# ============================================================================
# Register Symbols (24 forms: 6 registers x 4 modes): U+E260-E277
# ============================================================================
# Registers: Narrative, Discursive, Parenthetical, Cogitant, Exemplificative, Specificative
# Modes: Standard, Alphabetic, Transcriptive, Transliterative
# From reference: diamond-based shapes with varying marks

REGISTER_NAMES = ['NRR', 'DSV', 'PNT', 'CGT', 'EXM', 'SPF']
REGISTER_MODES = ['STD', 'ALP', 'TRN', 'TRL']
REGISTER_GLYPHS = {}
for ri, reg in enumerate(REGISTER_NAMES):
    for mi, mode in enumerate(REGISTER_MODES):
        key = f'{reg}_{mode}'
        idx = ri * 4 + mi
        cx, cy = 200, 500
        # Base: diamond shape, size varies by register
        sz = 40 + ri * 5
        parts = [
            L(cx, cy - sz, cx + sz, cy),  # top-right edge
            L(cx + sz, cy, cx, cy + sz),   # bottom-right edge
            L(cx, cy + sz, cx - sz, cy),   # bottom-left edge
            L(cx - sz, cy, cx, cy - sz),   # top-left edge
        ]
        # Mode decoration
        if mode == 'ALP':
            parts.append(L(cx + sz + 10, cy - 10, cx + sz + 10, cy + 10))  # dot right
        elif mode == 'TRN':
            parts.append(L(cx + sz + 5, cy, cx + sz + 20, cy))  # bar right
        elif mode == 'TRL':
            parts.append(L(cx + sz + 5, cy - 8, cx + sz + 15, cy + 8))  # slash right
        # Register-specific internal mark
        for tick in range(ri):
            parts.append(L(cx - 5 + tick * 8, cy + sz + 10, cx - 5 + tick * 8, cy + sz + 20))
        REGISTER_GLYPHS[key] = _glyph(f'reg_{key}', '', parts, 500)
        REGISTER_GLYPHS[key]['codepoint'] = 0xE260 + idx


# ============================================================================
# Numeral Characters (10 base digits): U+E270-E279
# ============================================================================
# From reference: cursive digit glyphs 0-9, each with distinctive shape.

NUMERAL_GLYPHS = {}
_numeral_shapes = {
    0: [A(200, 500, 150, 45, 315)],                                    # open curve (like a tilde)
    1: [L(250, 800, 200, 300), L(200, 300, 220, 250)],                # stroke with foot
    2: [L(150, 700, 300, 700), L(300, 700, 150, 300)],                # angle
    3: [L(150, 700, 300, 700), L(300, 700, 200, 500), L(200, 500, 300, 300)],  # zigzag
    4: [L(200, 800, 200, 300), L(200, 500, 350, 500)],                # cross
    5: [L(150, 800, 300, 500), L(300, 500, 150, 300)],                # V-shape
    6: [A(250, 500, 150, 90, 360), L(250, 650, 300, 700)],            # loop + stroke
    7: [L(150, 500, 250, 800), L(250, 800, 350, 500), L(250, 500, 250, 300)],  # N-shape
    8: [L(150, 500, 250, 750), L(250, 750, 350, 500), L(350, 500, 200, 250)],  # zigzag down
    9: [A(200, 600, 120, 0, 300), L(200, 480, 250, 300)],             # loop + tail
}
for digit, parts in _numeral_shapes.items():
    NUMERAL_GLYPHS[digit] = _glyph(f'num_{digit}', '', parts, 400)
    NUMERAL_GLYPHS[digit]['codepoint'] = 0xE270 + digit


if __name__ == '__main__':
    render_test_sheet()
    render_tertiary_test()

def render_tertiary_test(filename='script/tertiary_chars_test.svg'):
    """Render all tertiary characters to SVG test sheet."""
    cols, cell_w, cell_h, margin = 9, 80, 80, 15
    all_items = ([(n, TERTIARY_VALENCE[n]) for n in VALENCE_NAMES] +
                 [(n, TERTIARY_ASPECT[n]) for n in ASPECT_NAMES])
    rows = (len(all_items) + cols - 1) // cols
    W = cols * cell_w + 2 * margin
    H = rows * cell_h + 2 * margin

    svg = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" '
           f'viewBox="0 0 {W} {H}">', '<rect width="100%" height="100%" fill="white"/>']

    for idx, (name, glyph) in enumerate(all_items):
        col, row = idx % cols, idx // cols
        x, y = margin + col * cell_w, margin + row * cell_h
        svg.append(f'<rect x="{x}" y="{y}" width="{cell_w}" height="{cell_h}" '
                   f'fill="none" stroke="#ddd" stroke-width="0.5"/>')
        svg.append(f'<text x="{x+cell_w//2}" y="{y+10}" text-anchor="middle" '
                   f'font-size="7" fill="#666">{name}</text>')
        transform = f'translate({x+5},{y+15+55}) scale(0.065,-0.065)'
        svg.append(f'<g transform="{transform}"><path d="{glyph["path"]}" fill="black" fill-rule="nonzero"/></g>')

    svg.append('</svg>')
    with open(filename, 'w') as f:
        f.write('\n'.join(svg))
    print(f'Wrote {filename} ({len(all_items)} glyphs)')
