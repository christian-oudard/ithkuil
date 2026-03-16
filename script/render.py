#!/usr/bin/env python3
"""
Ithkuil V4 Script - SVG Renderer

Renders formatives and sentences as SVG using the glyph definitions.
Takes parsed formative data and produces properly laid-out SVG output.

Character types (left to right in a word):
  1. Primary   - thick diagonal bar encoding Vr/Vv/Ca info
  2. Secondary - consonant glyphs (Cr root, Cs affixes)
  3. Tertiary  - valence + aspect/phase/effect (arrow shapes)
  4. Quaternary - case or illocution/validation (vertical stem + extensions)
"""
import sys, os, math
sys.path.insert(0, os.path.dirname(__file__))

from glyphs import (SECONDARY, CONSONANT_ORDER, _outline as L, _arc as A, _glyph,
                     CONS_EXT_TOP, CONS_EXT_BOT)


# ============================================================================
# SVG Drawing Primitives
# ============================================================================

def svg_line(x1, y1, x2, y2, w=2.5, color='black', cap='round'):
    """SVG line element."""
    return (f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" '
            f'stroke="{color}" stroke-width="{w}" stroke-linecap="{cap}"/>')

def svg_path(d, w=2.5, color='black', fill='none', cap='round'):
    """SVG path element."""
    return (f'<path d="{d}" stroke="{color}" stroke-width="{w}" '
            f'fill="{fill}" stroke-linecap="{cap}" stroke-linejoin="round"/>')

def svg_circle(cx, cy, r, color='black'):
    """SVG filled circle."""
    return f'<circle cx="{cx:.1f}" cy="{cy:.1f}" r="{r}" fill="{color}"/>'

def svg_polygon(points, color='black'):
    """SVG filled polygon from list of (x,y) tuples."""
    pts = ' '.join(f'{x:.1f},{y:.1f}' for x, y in points)
    return f'<polygon points="{pts}" fill="{color}"/>'


# ============================================================================
# Primary Character Renderer
# ============================================================================

def draw_primary(x, y, w, h, spec='BSC', ctx='EXS', stem=1, func='STA',
                 ver='PRC', config='UNI', affil='CSL', ess='NRM',
                 persp='M', ext='DEL', relation='NOUN'):
    """Draw a primary character: thick diagonal bar with zone overlays.

    The primary character is a thick diagonal bar going from lower-left to
    upper-right, with morphological information encoded in surrounding zones.
    """
    elements = []

    # Bar geometry: lower-left to upper-right
    bar_w = w * 0.18  # bar thickness (fraction of cell width)
    bx1, by1 = x + w * 0.15, y + h * 0.85  # bottom-left
    bx2, by2 = x + w * 0.85, y + h * 0.15  # top-right

    # Draw the thick diagonal bar as a filled parallelogram
    dx = bx2 - bx1
    dy = by2 - by1
    length = math.sqrt(dx*dx + dy*dy)
    nx = -dy / length * bar_w / 2
    ny = dx / length * bar_w / 2

    bar_pts = [
        (bx1 + nx, by1 + ny),
        (bx2 + nx, by2 + ny),
        (bx2 - nx, by2 - ny),
        (bx1 - nx, by1 - ny),
    ]
    elements.append(svg_polygon(bar_pts))

    # --- Specification (lower-left zone) ---
    sx, sy = bx1, by1  # attachment point
    if spec == 'CTE':
        # Small curve/hook at lower portion
        elements.append(svg_line(sx - 4, sy - 6, sx + 6, sy - 12, 2))
    elif spec == 'CSV':
        # Angular mark
        elements.append(svg_line(sx - 2, sy - 4, sx + 5, sy - 12, 2))
        elements.append(svg_line(sx + 5, sy - 12, sx - 2, sy - 18, 2))
    elif spec == 'OBJ':
        # Reversed angular
        elements.append(svg_line(sx - 2, sy - 12, sx + 5, sy - 4, 2))
        elements.append(svg_line(sx + 5, sy - 4, sx - 2, sy + 2, 2))

    # --- Context (super-posed diacritic above the bar) ---
    cx_d, cy_d = (bx1 + bx2) / 2, by2 - 8
    if ctx == 'EXS':
        # Diamond/dot
        elements.append(svg_polygon([
            (cx_d, cy_d - 3), (cx_d + 2.5, cy_d),
            (cx_d, cy_d + 3), (cx_d - 2.5, cy_d),
        ]))
    elif ctx == 'FNC':
        # Horizontal bar
        elements.append(svg_line(cx_d - 5, cy_d, cx_d + 5, cy_d, 2))
    elif ctx == 'RPS':
        # Angled stroke (backslash)
        elements.append(svg_line(cx_d - 4, cy_d - 3, cx_d + 4, cy_d + 3, 2))
    elif ctx == 'AMG':
        # Reversed angled stroke (slash)
        elements.append(svg_line(cx_d + 4, cy_d - 3, cx_d - 4, cy_d + 3, 2))

    # --- Stem/Function marks (lower-right zone) ---
    # Small ticks alongside the lower-right portion of the bar
    mx = bx1 + (bx2 - bx1) * 0.35
    my = by1 + (by2 - by1) * 0.35
    tick_dx, tick_dy = ny * 0.6, -nx * 0.6  # perpendicular to bar

    # Stem count shown as small ticks
    for i in range(stem):
        tx = mx + (i + 1) * 5 * (bx2 - bx1) / length
        ty = my + (i + 1) * 5 * (by2 - by1) / length
        elements.append(svg_line(tx, ty, tx + tick_dx * 2, ty + tick_dy * 2, 1.5))

    # Function: DYN adds a parallel thin line alongside the bar
    if func == 'DYN':
        offset = bar_w * 0.4
        elements.append(svg_line(
            bx1 - nx * 0.6, by1 - ny * 0.6,
            bx2 - nx * 0.6, by2 - ny * 0.6, 1.5))

    # Version: CPT adds a small crossbar
    if ver == 'CPT':
        cmx = (bx1 + bx2) / 2
        cmy = (by1 + by2) / 2
        elements.append(svg_line(cmx - nx * 2, cmy - ny * 2,
                                  cmx + nx * 2, cmy + ny * 2, 1.5))

    # --- Perspective/Extension (upper-left zone) ---
    # Default M/DEL shows no mark; non-default values get marks
    px, py = bx2 - 10, by2 + 5
    if persp != 'M' or ext != 'DEL':
        _draw_persp_ext_mark(elements, px, py, persp, ext)

    # --- Affiliation/Essence (upper-right zone) ---
    ax, ay = bx2 + 5, by2 + 10
    if affil != 'CSL' or ess != 'NRM':
        _draw_affil_ess_mark(elements, ax, ay, affil, ess)

    # --- Configuration (under-posed mark below the bar) ---
    ux, uy = bx1 + 2, by1 + 6
    _draw_config_mark(elements, ux, uy, config)

    # --- Relation (subscript diacritic beneath) ---
    rx, ry = bx1, by1 + 12
    if relation == 'UNFRAMED_VERB':
        elements.append(svg_polygon([
            (rx, ry - 2), (rx + 2, ry), (rx, ry + 2), (rx - 2, ry),
        ]))
    elif relation == 'FRAMED_VERB':
        elements.append(svg_line(rx - 4, ry, rx + 4, ry, 2))

    return elements


def _draw_persp_ext_mark(elements, x, y, persp, ext):
    """Draw perspective x extension mark in upper-left zone of primary char.

    From reference 12_1_perspective_extension.png: 24 distinct small marks.
    Simplified: perspective controls direction, extension controls complexity.
    """
    sw = 1.5
    # Direction based on perspective
    dirs = {'M': (1, 0), 'G': (0, 1), 'N': (-1, 0), 'A': (0, -1)}
    dx, dy = dirs.get(persp, (1, 0))
    elen = 6

    # Base mark for extension
    ext_mods = {'DEL': 0, 'PRX': 1, 'ICP': 2, 'ATV': 3, 'GRA': 4, 'DPL': 5}
    mod = ext_mods.get(ext, 0)

    # Simple angled line for perspective
    elements.append(svg_line(x, y, x + dx * elen, y + dy * elen, sw))

    # Extension adds complexity
    if mod >= 1:
        # Additional tick perpendicular to the main stroke
        px, py = -dy, dx  # perpendicular
        elements.append(svg_line(x + dx * elen, y + dy * elen,
                                  x + dx * elen + px * 3, y + dy * elen + py * 3, 1))
    if mod >= 3:
        # Second tick
        elements.append(svg_line(x + dx * 3, y + dy * 3,
                                  x + dx * 3 + px * 3, y + dy * 3 + py * 3, 1))


def _draw_affil_ess_mark(elements, x, y, affil, ess):
    """Draw affiliation x essence mark in upper-right zone of primary char.

    From reference 12_1_config_affil_essence.png:
    NRM: CSL=thin_stroke, ASO=thick_diagonal, COA=vertical_wedge, VAR=horizontal_bar
    RPV: CSL=arrow, ASO=upward_wedge, COA=hook, VAR=crescent
    """
    sw = 1.5
    if ess == 'NRM':
        if affil == 'ASO':
            elements.append(svg_line(x - 2, y + 4, x + 4, y - 2, 2.5))
        elif affil == 'COA':
            elements.append(svg_line(x, y, x, y + 6, sw))
            elements.append(svg_line(x - 2, y + 3, x + 2, y + 3, sw))
        elif affil == 'VAR':
            elements.append(svg_line(x - 4, y + 2, x + 4, y + 2, sw))
    else:  # RPV
        if affil == 'CSL':
            elements.append(svg_line(x - 3, y + 3, x + 3, y - 3, sw))
            elements.append(svg_line(x + 3, y - 3, x + 1, y, 1))
        elif affil == 'ASO':
            elements.append(svg_line(x - 2, y + 3, x, y - 2, sw))
            elements.append(svg_line(x, y - 2, x + 2, y + 3, sw))
        elif affil == 'COA':
            elements.append(svg_path(
                f'M{x-3:.1f},{y:.1f} Q{x:.1f},{y+4:.1f} {x+3:.1f},{y:.1f}', sw))
        elif affil == 'VAR':
            elements.append(svg_path(
                f'M{x-4:.1f},{y+2:.1f} Q{x:.1f},{y-3:.1f} {x+4:.1f},{y+2:.1f}', sw))


def _draw_config_mark(elements, x, y, config):
    """Draw configuration underposed mark below the primary character bar.

    From reference 12_1_config_affil_essence.png:
    10 configurations with distinct marks.
    """
    sw = 1.5
    if config in ('UNI', 'UPX'):
        return  # default, no mark
    marks = {
        'DPX': lambda: [svg_line(x - 4, y, x + 4, y, sw),
                         svg_line(x - 4, y + 3, x + 4, y + 3, sw)],
        'DSS': lambda: [svg_line(x - 3, y + 4, x, y, sw),
                         svg_line(x, y, x + 3, y + 4, sw)],
        'DSC': lambda: [svg_line(x - 4, y + 2, x + 4, y + 2, sw)],
        'DSF': lambda: [svg_line(x - 4, y, x, y + 4, sw),
                         svg_line(x, y + 4, x + 4, y, sw)],
        'DDS': lambda: [svg_path(f'M{x-4:.1f},{y:.1f} Q{x:.1f},{y+5:.1f} {x+4:.1f},{y:.1f}', sw)],
        'DDC': lambda: [svg_path(f'M{x-4:.1f},{y+4:.1f} Q{x:.1f},{y-1:.1f} {x+4:.1f},{y+4:.1f}', sw)],
        'DDF': lambda: [svg_line(x - 4, y + 2, x + 4, y + 2, sw),
                         svg_line(x, y, x, y + 4, sw)],
        'DFS': lambda: [svg_line(x - 4, y, x + 4, y, sw),
                         svg_path(f'M{x-3:.1f},{y+3:.1f} Q{x:.1f},{y+6:.1f} {x+3:.1f},{y+3:.1f}', sw)],
        'DFC': lambda: [svg_path(f'M{x-3:.1f},{y:.1f} A3,3 0 1,0 {x+3:.1f},{y:.1f}', sw, fill='none')],
        'DFF': lambda: [svg_circle(x, y + 2, 2)],
    }
    # Also handle MSS/MSC/MSF/MDS/MDC/MDF/MFS/MFC/MFF names
    for key, fn in marks.items():
        if config == key:
            elements.extend(fn())
            return


# ============================================================================
# Quaternary Character Renderer
# ============================================================================

def draw_quaternary_case(x, y, w, h, case_type=0, case_num=1, mood=None):
    """Draw a quaternary character for case.

    Matches the shapes from reference image 12_4_quaternary_chars.png:
    - Vertical stem
    - Top extension encodes case type (8 shapes)
    - Bottom extension encodes case number (9 shapes)
    - Mood shown as superposed diacritic
    """
    elements = []
    cx = x + w / 2
    top = y + 8
    bot = y + h - 8
    sw = 2.5  # stroke width

    # Main vertical stem
    elements.append(svg_line(cx, top, cx, bot, sw))

    # --- Case Type (top extension) ---
    # From reference: 8 distinct top shapes
    if case_type == 0:
        # TRANSRELATIVE: plain vertical (no extra extension)
        pass
    elif case_type == 1:
        # APPOSITIVE: right curve/hook at top
        elements.append(svg_path(
            f'M{cx:.1f},{top:.1f} Q{cx+14:.1f},{top:.1f} {cx+12:.1f},{top+14:.1f}', sw))
    elif case_type == 2:
        # ASSOCIATIVE: Y-fork at top
        elements.append(svg_line(cx, top, cx + 10, top - 8, sw))
        elements.append(svg_line(cx, top, cx - 10, top - 8, sw))
    elif case_type == 3:
        # ADVERBIAL: right arc/hook
        elements.append(svg_path(
            f'M{cx:.1f},{top:.1f} Q{cx+16:.1f},{top+5:.1f} {cx+12:.1f},{top+16:.1f}', sw))
    elif case_type == 4:
        # RELATIONAL: left hook at top
        elements.append(svg_path(
            f'M{cx:.1f},{top:.1f} Q{cx-14:.1f},{top:.1f} {cx-12:.1f},{top+14:.1f}', sw))
    elif case_type == 5:
        # AFFINITIVE: T-bar at top
        elements.append(svg_line(cx - 12, top, cx + 12, top, sw))
    elif case_type == 6:
        # SPATIO-TEMPORAL I: T-bar + right hook
        elements.append(svg_line(cx - 12, top, cx + 12, top, sw))
        elements.append(svg_path(
            f'M{cx+12:.1f},{top:.1f} Q{cx+18:.1f},{top+2:.1f} {cx+16:.1f},{top+10:.1f}', 2))
    elif case_type == 7:
        # SPATIO-TEMPORAL II: T-bar + left hook
        elements.append(svg_line(cx - 12, top, cx + 12, top, sw))
        elements.append(svg_path(
            f'M{cx-12:.1f},{top:.1f} Q{cx-18:.1f},{top+2:.1f} {cx-16:.1f},{top+10:.1f}', 2))

    # --- Case Number (bottom extension) ---
    # From reference: 9 shapes with increasing complexity
    # 1=plain, 2=small right hook, 3=left hook, 4=right curve, 5=left curve,
    # 6=right S, 7=left S, 8=right double, 9=left double
    if case_num >= 1:
        _draw_case_num_extension(elements, cx, bot, case_num, sw)

    # --- Mood diacritic (superposed) ---
    if mood:
        my = top - 6
        _draw_mood_diac(elements, cx, my, mood)

    return elements


def _draw_case_num_extension(elements, cx, bot, num, sw):
    """Draw case number bottom extension (9 forms)."""
    elen = 14  # extension length
    if num == 1:
        # Plain short downward
        elements.append(svg_line(cx, bot, cx, bot + elen * 0.5, sw))
    elif num == 2:
        # Right hook
        elements.append(svg_path(
            f'M{cx:.1f},{bot:.1f} L{cx:.1f},{bot+6:.1f} Q{cx+8:.1f},{bot+12:.1f} {cx+12:.1f},{bot+6:.1f}', sw))
    elif num == 3:
        # Left hook
        elements.append(svg_path(
            f'M{cx:.1f},{bot:.1f} L{cx:.1f},{bot+6:.1f} Q{cx-8:.1f},{bot+12:.1f} {cx-12:.1f},{bot+6:.1f}', sw))
    elif num == 4:
        # Right curve
        elements.append(svg_path(
            f'M{cx:.1f},{bot:.1f} Q{cx+14:.1f},{bot+8:.1f} {cx+10:.1f},{bot+16:.1f}', sw))
    elif num == 5:
        # Serif / horizontal at bottom
        elements.append(svg_line(cx, bot, cx, bot + elen, sw))
        elements.append(svg_line(cx - 8, bot + elen, cx + 8, bot + elen, sw))
    elif num == 6:
        # Right S-curve
        elements.append(svg_path(
            f'M{cx:.1f},{bot:.1f} Q{cx+10:.1f},{bot+5:.1f} {cx:.1f},{bot+12:.1f} '
            f'Q{cx-8:.1f},{bot+17:.1f} {cx:.1f},{bot+20:.1f}', sw))
    elif num == 7:
        # Left flag
        elements.append(svg_line(cx, bot, cx, bot + elen, sw))
        elements.append(svg_line(cx, bot + 4, cx - 10, bot + 8, sw))
    elif num == 8:
        # Right flag
        elements.append(svg_line(cx, bot, cx, bot + elen, sw))
        elements.append(svg_line(cx, bot + 4, cx + 10, bot + 8, sw))
    elif num == 9:
        # Fork at bottom
        elements.append(svg_line(cx, bot, cx, bot + 8, sw))
        elements.append(svg_line(cx, bot + 8, cx + 8, bot + elen, sw))
        elements.append(svg_line(cx, bot + 8, cx - 8, bot + elen, sw))


def _draw_mood_diac(elements, cx, y, mood):
    """Draw mood diacritic above quaternary character."""
    if mood == 'FAC':
        pass
    elif mood == 'SUB':
        elements.append(svg_circle(cx, y, 2))
    elif mood == 'ASM':
        elements.append(svg_line(cx - 4, y, cx + 4, y, 1.5))
    elif mood == 'SPC':
        elements.append(svg_circle(cx - 2, y, 1.5))
        elements.append(svg_circle(cx + 2, y, 1.5))
    elif mood == 'COU':
        elements.append(svg_line(cx - 4, y - 1, cx + 4, y - 1, 1.5))
        elements.append(svg_line(cx - 4, y + 1, cx + 4, y + 1, 1.5))
    elif mood == 'HYP':
        elements.append(svg_path(
            f'M{cx-4:.1f},{y:.1f} Q{cx:.1f},{y-4:.1f} {cx+4:.1f},{y:.1f}', 1.5))


def draw_quaternary_vk(x, y, w, h, illoc='ASR', valid='OBS'):
    """Draw a quaternary character for illocution/validation (Vk).

    From reference: tall vertical stems with distinctive top curves (illocution)
    and bottom hooks (validation).
    """
    elements = []
    cx = x + w / 2
    top = y + 5
    bot = y + h - 5
    sw = 2.5

    # Main vertical stem
    elements.append(svg_line(cx, top, cx, bot, sw))

    # Illocution (top extension)
    illoc_shapes = {
        'ASR': [],  # plain
        'DIR': [('curve_r', 12)],
        'DEC': [('hook_r', 10), ('tick_down', 6)],
        'IRG': [('bar', 10)],
        'VRF': [('hook_l', 10), ('tick_down', 6)],
        'ADM': [('curve_l', 12)],
        'POT': [('bar', 8), ('hook_r', 6)],
        'HOR': [('curve_r', 16)],
        'CNJ': [('bar', 8), ('hook_l', 6)],
    }

    for shape, size in illoc_shapes.get(illoc, []):
        if shape == 'curve_r':
            elements.append(svg_path(
                f'M{cx:.1f},{top:.1f} Q{cx+size:.1f},{top:.1f} {cx+size-2:.1f},{top+size:.1f}', sw))
        elif shape == 'hook_r':
            elements.append(svg_line(cx, top, cx + size, top + size * 0.8, sw))
        elif shape == 'hook_l':
            elements.append(svg_line(cx, top, cx - size, top + size * 0.8, sw))
        elif shape == 'curve_l':
            elements.append(svg_path(
                f'M{cx:.1f},{top:.1f} Q{cx-size:.1f},{top:.1f} {cx-size+2:.1f},{top+size:.1f}', sw))
        elif shape == 'bar':
            elements.append(svg_line(cx - size, top, cx + size, top, sw))
        elif shape == 'tick_down':
            elements.append(svg_line(cx, top, cx, top + size, 1.5))

    # Validation (bottom extension, only with ASR illocution)
    if illoc == 'ASR':
        valid_idx = ['OBS', 'REC', 'PUP', 'RPR', 'IMA', 'CVN', 'ITU', 'INF', 'USP']
        vi = valid_idx.index(valid) if valid in valid_idx else 0
        if vi > 0:
            _draw_case_num_extension(elements, cx, bot, vi, sw)

    return elements


# ============================================================================
# Tertiary Character Renderer
# ============================================================================

def draw_tertiary(x, y, w, h, valence='MNO', aspect=None, phase=None, effect=None):
    """Draw a tertiary character.

    Structure from reference (12_3_tertiary_chars.png):
    - Left segment: horizontal arrow shaft (valence)
    - Right segment: arrowhead shape (aspect, phase, or effect)
    """
    elements = []
    cx = x + w / 2
    cy = y + h / 2
    sw = 2.5

    # Valence: horizontal arrow shaft
    shaft_x1 = x + 4
    shaft_x2 = x + w - 4
    shaft_y = cy

    # Draw the main arrow shaft
    elements.append(svg_line(shaft_x1, shaft_y, shaft_x2, shaft_y, sw))

    # Arrowhead based on valence type
    head_x = shaft_x2
    head_spread = 6
    head_len = 8

    val_idx = ['MNO', 'PRL', 'CRO', 'RCP', 'CPL', 'DUP', 'DEM', 'CNG', 'PTI']
    vi = val_idx.index(valence) if valence in val_idx else 0

    # Simple arrowhead (all valences get one, but with variations)
    elements.append(svg_line(head_x, shaft_y, head_x + head_len, shaft_y - head_spread, sw))
    elements.append(svg_line(head_x, shaft_y, head_x + head_len, shaft_y + head_spread, sw))

    # Additional valence markers (must produce 9 distinct forms)
    if vi == 1:  # PRL: single back notch
        elements.append(svg_line(shaft_x1 + 4, shaft_y - 3, shaft_x1 + 4, shaft_y + 3, 1.5))
    elif vi == 2:  # CRO: hook on arrowhead
        elements.append(svg_path(
            f'M{head_x + head_len:.1f},{shaft_y - head_spread:.1f} '
            f'Q{head_x + head_len + 3:.1f},{shaft_y:.1f} '
            f'{head_x + head_len:.1f},{shaft_y + head_spread:.1f}', 1.5))
    elif vi == 3:  # RCP: triangle head (closed)
        elements.append(svg_line(head_x + head_len, shaft_y - head_spread,
                                  head_x + head_len, shaft_y + head_spread, 1.5))
    elif vi == 4:  # CPL: bar behind arrowhead
        elements.append(svg_line(head_x - 4, shaft_y - 5, head_x - 4, shaft_y + 5, 1.5))
    elif vi == 5:  # DUP: double back notch
        elements.append(svg_line(shaft_x1 + 4, shaft_y - 3, shaft_x1 + 4, shaft_y + 3, 1.5))
        elements.append(svg_line(shaft_x1 + 8, shaft_y - 3, shaft_x1 + 8, shaft_y + 3, 1.5))
    elif vi == 6:  # DEM: arrowhead + down-tick
        elements.append(svg_line(head_x + head_len, shaft_y + head_spread,
                                  head_x + head_len - 3, shaft_y + head_spread + 4, 1.5))
    elif vi == 7:  # CNG: arrowhead + up-tick
        elements.append(svg_line(head_x + head_len, shaft_y - head_spread,
                                  head_x + head_len - 3, shaft_y - head_spread - 4, 1.5))
    elif vi == 8:  # PTI: double hook on head
        elements.append(svg_line(head_x + head_len, shaft_y - head_spread,
                                  head_x + head_len + 3, shaft_y - head_spread + 3, 1.5))
        elements.append(svg_line(head_x + head_len, shaft_y + head_spread,
                                  head_x + head_len + 3, shaft_y + head_spread - 3, 1.5))

    # Aspect (shown as a second arrow or mark above/below the shaft)
    if aspect:
        _draw_aspect_mark(elements, x, y, w, h, aspect)

    # Phase (shown as vertical double-stroke marks)
    if phase:
        _draw_phase_mark(elements, x, y, w, h, phase)

    # Effect (shown as V-shapes)
    if effect:
        _draw_effect_mark(elements, x, y, w, h, effect)

    return elements


def _draw_aspect_mark(elements, x, y, w, h, aspect):
    """Draw aspect indicator on tertiary character (upper-right zone)."""
    aspect_names = [
        'RTR', 'PRS', 'HAB', 'PRG', 'IMM', 'PCS', 'REG', 'SMM', 'ATP',
        'RSM', 'CSS', 'PAU', 'RGR', 'PCL', 'CNT', 'ICS', 'EXP', 'IRP',
        'PMP', 'CLM', 'DLT', 'TMP', 'XPD', 'LIM', 'EPD', 'PTC', 'PPR',
        'DCL', 'CCL', 'CUL', 'IMD', 'TRD', 'TNS', 'ITC', 'MTV', 'SQN',
    ]
    if aspect not in aspect_names:
        return
    idx = aspect_names.index(aspect)
    col = idx // 9
    row = idx % 9

    # Draw aspect as a small arrow/chevron in the upper-right portion
    ax = x + w * 0.6
    ay = y + h * 0.25
    aw = w * 0.35
    ah = h * 0.2
    sw = 1.5

    # Arrow direction varies by column
    if col == 0:
        # Right-pointing
        elements.append(svg_line(ax, ay, ax + aw, ay, sw))
        elements.append(svg_line(ax + aw, ay, ax + aw - 3, ay - 3, sw))
    elif col == 1:
        # Down-pointing
        elements.append(svg_line(ax + aw/2, ay - 3, ax + aw/2, ay + ah, sw))
        elements.append(svg_line(ax + aw/2, ay + ah, ax + aw/2 - 3, ay + ah - 3, sw))
    elif col == 2:
        # Left-pointing
        elements.append(svg_line(ax + aw, ay, ax, ay, sw))
        elements.append(svg_line(ax, ay, ax + 3, ay - 3, sw))
    elif col == 3:
        # Up-pointing
        elements.append(svg_line(ax + aw/2, ay + ah, ax + aw/2, ay - 3, sw))
        elements.append(svg_line(ax + aw/2, ay - 3, ax + aw/2 - 3, ay, sw))

    # Row adds tick marks
    for i in range(min(row, 3)):
        elements.append(svg_line(ax + 3 + i * 3, ay + ah + 2, ax + 3 + i * 3, ay + ah + 5, 1))


def _draw_phase_mark(elements, x, y, w, h, phase):
    """Draw phase indicator (double vertical strokes)."""
    phase_names = ['PCT', 'ITR', 'REP', 'ITM', 'RCT', 'FRE', 'FRG', 'VAC', 'FLC']
    if phase not in phase_names:
        return
    idx = phase_names.index(phase)
    px = x + w * 0.55
    py = y + h * 0.2
    ph = h * 0.25
    sw = 1.5

    # Two vertical bars with varying bottom connection
    elements.append(svg_line(px - 3, py, px - 3, py + ph, sw))
    elements.append(svg_line(px + 3, py, px + 3, py + ph, sw))
    # Connection varies by phase
    if idx >= 1:
        elements.append(svg_line(px - 3, py + ph, px + 3, py + ph, 1))
    if idx >= 4:
        elements.append(svg_line(px - 3, py, px + 3, py, 1))


def _draw_effect_mark(elements, x, y, w, h, effect):
    """Draw effect indicator (V-shape variants)."""
    ex = x + w * 0.55
    ey = y + h * 0.2
    sw = 1.5
    # Simple V-shape
    elements.append(svg_line(ex - 4, ey, ex, ey + 8, sw))
    elements.append(svg_line(ex + 4, ey, ex, ey + 8, sw))


# ============================================================================
# Formative Renderer
# ============================================================================

class FormativeRenderer:
    """Renders an Ithkuil formative as SVG."""

    CHAR_WIDTH = 50
    CHAR_HEIGHT = 100
    SPACING = 5
    GLYPH_SCALE = 0.08

    def __init__(self):
        self.elements = []
        self.x_cursor = 10

    def add_secondary(self, consonant, rotated=False, degree=None, affix_type=None):
        """Add a secondary (consonant) character."""
        glyph = SECONDARY.get(consonant)
        if not glyph:
            return
        self._draw_secondary_glyph(glyph, rotated)

        # Degree diacritic below
        if degree is not None:
            dy = 10 + self.CHAR_HEIGHT + 5
            _draw_degree_diac(self.elements,
                              self.x_cursor - self.SPACING - self.CHAR_WIDTH // 2, dy, degree)

        # Affix type diacritic above
        cx = self.x_cursor - self.SPACING - self.CHAR_WIDTH // 2
        if affix_type == 2:
            self.elements.append(svg_circle(cx, 10 - 3, 2))
        elif affix_type == 3:
            self.elements.append(svg_line(cx - 5, 10 - 3, cx + 5, 10 - 3, 1.5))

    def add_cluster(self, consonants, rotated=False, degree=None, affix_type=None):
        """Add a consonant cluster as a single composite character with extensions.

        For a 2-consonant cluster [C1, C2]: C1 is base, C2 is bottom extension.
        For a 3-consonant cluster [C1, C2, C3]: C2 is base, C1 top ext, C3 bottom ext.
        For gemination [C, C]: base C with gemination mark.
        """
        if not consonants:
            return
        if len(consonants) == 1:
            self.add_secondary(consonants[0], rotated=rotated, degree=degree, affix_type=affix_type)
            return

        # Check for gemination
        if len(consonants) == 2 and consonants[0] == consonants[1]:
            base = consonants[0]
            glyph = SECONDARY.get(base)
            if not glyph:
                return
            self._draw_secondary_glyph(glyph, rotated)
            # Draw gemination mark (small double tick below)
            cx = self.x_cursor - self.SPACING - self.CHAR_WIDTH // 2
            gy = 10 + self.CHAR_HEIGHT + 3
            self.elements.append(svg_line(cx - 3, gy, cx - 3, gy + 5, 1.5))
            self.elements.append(svg_line(cx + 3, gy, cx + 3, gy + 5, 1.5))
        elif len(consonants) == 2:
            base_c, ext_c = consonants[0], consonants[1]
            glyph = SECONDARY.get(base_c)
            if not glyph:
                return
            self._draw_secondary_glyph(glyph, rotated)
            # Draw bottom extension for second consonant
            ext = CONS_EXT_BOT.get(ext_c)
            if ext and ext.get('path', '').strip():
                self._draw_extension(ext, rotated)
        elif len(consonants) == 3:
            top_c, base_c, bot_c = consonants[0], consonants[1], consonants[2]
            glyph = SECONDARY.get(base_c)
            if not glyph:
                return
            self._draw_secondary_glyph(glyph, rotated)
            # Draw top extension for first consonant
            ext_top = CONS_EXT_TOP.get(top_c)
            if ext_top and ext_top.get('path', '').strip():
                self._draw_extension(ext_top, rotated)
            # Draw bottom extension for third consonant
            ext_bot = CONS_EXT_BOT.get(bot_c)
            if ext_bot and ext_bot.get('path', '').strip():
                self._draw_extension(ext_bot, rotated)
        else:
            # 4+ consonants: render first as cluster of 3, rest as separate chars
            self.add_cluster(consonants[:3], rotated=rotated)
            for c in consonants[3:]:
                self.add_secondary(c, rotated=rotated)

        # Degree diacritic below
        if degree is not None:
            dy = 10 + self.CHAR_HEIGHT + 5
            _draw_degree_diac(self.elements,
                              self.x_cursor - self.SPACING - self.CHAR_WIDTH // 2, dy, degree)
        # Affix type diacritic above
        cx = self.x_cursor - self.SPACING - self.CHAR_WIDTH // 2
        if affix_type == 2:
            self.elements.append(svg_circle(cx, 10 - 3, 2))
        elif affix_type == 3:
            self.elements.append(svg_line(cx - 5, 10 - 3, cx + 5, 10 - 3, 1.5))

    def _draw_secondary_glyph(self, glyph, rotated=False):
        """Draw a secondary character glyph at the current cursor position."""
        s = self.GLYPH_SCALE
        x = self.x_cursor
        y = 10 + self.CHAR_HEIGHT  # baseline

        transform = f'translate({x},{y}) scale({s},{-s})'
        if rotated:
            transform = f'translate({x},{y}) scale({s},{-s}) rotate(180 250 500)'

        self.elements.append(
            f'<g transform="{transform}">'
            f'<path d="{glyph["path"]}" fill="black" fill-rule="nonzero"/></g>'
        )
        self.x_cursor += self.CHAR_WIDTH + self.SPACING

    def _draw_extension(self, ext_glyph, rotated=False):
        """Draw a combining extension glyph on the last secondary character."""
        s = self.GLYPH_SCALE
        # Extension is drawn at the same x position as the base char (it's combining)
        x = self.x_cursor - self.CHAR_WIDTH - self.SPACING
        y = 10 + self.CHAR_HEIGHT

        transform = f'translate({x},{y}) scale({s},{-s})'
        if rotated:
            transform = f'translate({x},{y}) scale({s},{-s}) rotate(180 250 500)'

        self.elements.append(
            f'<g transform="{transform}">'
            f'<path d="{ext_glyph["path"]}" fill="black" fill-rule="nonzero"/></g>'
        )

    def add_primary(self, spec='BSC', ctx='EXS', stem=1, func='STA',
                    ver='PRC', config='UNI', affil='CSL', ess='NRM',
                    persp='M', ext='DEL', relation='NOUN'):
        """Add a primary character (thick diagonal bar with diacritics)."""
        x = self.x_cursor
        y = 10
        self.elements.extend(draw_primary(
            x, y, self.CHAR_WIDTH, self.CHAR_HEIGHT,
            spec=spec, ctx=ctx, stem=stem, func=func, ver=ver,
            config=config, affil=affil, ess=ess, persp=persp, ext=ext,
            relation=relation))
        self.x_cursor += self.CHAR_WIDTH + self.SPACING

    def add_quaternary(self, case_type=0, case_num=1, mood=None):
        """Add a quaternary character (case indicator)."""
        x = self.x_cursor
        y = 10
        self.elements.extend(draw_quaternary_case(
            x, y, self.CHAR_WIDTH, self.CHAR_HEIGHT,
            case_type=case_type, case_num=case_num, mood=mood))
        self.x_cursor += self.CHAR_WIDTH + self.SPACING

    def add_quaternary_vk(self, illoc='ASR', valid='OBS'):
        """Add a quaternary character for illocution/validation."""
        x = self.x_cursor
        y = 10
        self.elements.extend(draw_quaternary_vk(
            x, y, self.CHAR_WIDTH, self.CHAR_HEIGHT,
            illoc=illoc, valid=valid))
        self.x_cursor += self.CHAR_WIDTH + self.SPACING

    def add_tertiary(self, valence='MNO', aspect=None, phase=None, effect=None):
        """Add a tertiary character (valence + aspect/phase/effect)."""
        x = self.x_cursor
        y = 10
        self.elements.extend(draw_tertiary(
            x, y, self.CHAR_WIDTH, self.CHAR_HEIGHT,
            valence=valence, aspect=aspect, phase=phase, effect=effect))
        self.x_cursor += self.CHAR_WIDTH + self.SPACING

    def to_svg(self, width=None, height=130):
        """Generate complete SVG string."""
        if width is None:
            width = self.x_cursor + 10
        parts = [
            f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" '
            f'viewBox="0 0 {width} {height}">',
            '<rect width="100%" height="100%" fill="white"/>',
        ]
        parts.extend(e for e in self.elements if e)
        parts.append('</svg>')
        return '\n'.join(parts)


# ============================================================================
# Degree Diacritic Renderer
# ============================================================================

def _draw_degree_diac(elements, cx, y, degree):
    """Draw affix degree diacritic below the character.

    Matches reference 12_2_affix_degree_type_diacritics.png:
    1=dot, 2=hook, 3=slash, 4=curl, 5=bar/backslash, 6=crescent,
    7=wedge, 8=zigzag, 9=arc
    """
    sw = 1.5
    if degree == 1:
        elements.append(svg_circle(cx, y, 1.5))
    elif degree == 2:
        elements.append(svg_path(
            f'M{cx-3:.1f},{y:.1f} Q{cx:.1f},{y+5:.1f} {cx+3:.1f},{y-2:.1f}', sw))
    elif degree == 3:
        elements.append(svg_line(cx - 3, y + 2, cx + 3, y - 2, sw))
    elif degree == 4:
        elements.append(svg_path(
            f'M{cx-3:.1f},{y:.1f} Q{cx-3:.1f},{y+5:.1f} {cx+3:.1f},{y+5:.1f} '
            f'Q{cx+3:.1f},{y:.1f} {cx:.1f},{y-2:.1f}', sw))
    elif degree == 5:
        elements.append(svg_line(cx - 4, y, cx + 4, y, sw))
    elif degree == 6:
        elements.append(svg_path(
            f'M{cx-4:.1f},{y-2:.1f} Q{cx:.1f},{y+4:.1f} {cx+4:.1f},{y-2:.1f}', sw))
    elif degree == 7:
        elements.append(svg_line(cx - 3, y + 2, cx, y - 3, sw))
        elements.append(svg_line(cx, y - 3, cx + 3, y + 2, sw))
    elif degree == 8:
        elements.append(svg_line(cx - 4, y, cx - 1, y - 3, sw))
        elements.append(svg_line(cx - 1, y - 3, cx + 1, y + 3, sw))
        elements.append(svg_line(cx + 1, y + 3, cx + 4, y, sw))
    elif degree == 9:
        elements.append(svg_path(
            f'M{cx-4:.1f},{y+2:.1f} Q{cx:.1f},{y-4:.1f} {cx+4:.1f},{y+2:.1f}', sw))


# ============================================================================
# High-level rendering API
# ============================================================================

def render_consonant_cluster(cluster):
    """Split a consonant cluster into individual consonants for rendering."""
    consonants = []
    i = 0
    text = cluster.lower()
    while i < len(text):
        if i + 1 < len(text) and text[i:i+2] in SECONDARY:
            consonants.append(text[i:i+2])
            i += 2
        elif text[i] in SECONDARY or text[i:i+1] in SECONDARY:
            consonants.append(text[i])
            i += 1
        else:
            i += 1
    return consonants


def render_word(root_consonants, affixes=None, case_type=0, case_num=1,
                spec='BSC', ctx='EXS', stem=1, func='STA',
                valence='MNO', aspect=None, phase=None, effect=None):
    """Render a single word to SVG."""
    r = FormativeRenderer()

    # 1. Primary character
    r.add_primary(spec=spec, ctx=ctx, stem=stem, func=func)

    # 2. Root consonant(s) as a cluster
    root_cs = render_consonant_cluster(root_consonants)
    r.add_cluster(root_cs)

    # 3. Affixes as clusters
    if affixes:
        for cs, degree, atype, slot in affixes:
            rotated = (slot == 7)
            affix_cs = render_consonant_cluster(cs)
            r.add_cluster(affix_cs, rotated=rotated, degree=degree, affix_type=atype)

    # 4. Tertiary character (if non-default valence/aspect/phase/effect)
    has_tertiary = (valence != 'MNO' or aspect or phase or effect)
    if has_tertiary:
        r.add_tertiary(valence=valence, aspect=aspect, phase=phase, effect=effect)

    # 5. Quaternary character (case)
    r.add_quaternary(case_type=case_type, case_num=case_num)

    return r.to_svg()


def render_test_words():
    """Render comprehensive test sheet for the writing system."""
    page_w, page_h = 950, 1400
    page_parts = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{page_w}" height="{page_h}" '
        f'viewBox="0 0 {page_w} {page_h}">',
        f'<rect width="100%" height="100%" fill="#faf8f0"/>',
        '<text x="20" y="25" font-size="14" font-family="sans-serif" fill="#333">'
        'Ithkuil V4 Script - Comprehensive Test Sheet</text>',
    ]
    y = 45

    # --- Section 1: Sample Words ---
    page_parts.append(f'<text x="20" y="{y}" font-size="11" font-weight="bold" '
                      f'fill="#333">Sample Words</text>')
    y += 5

    words = [
        ('Malëuţřait (THM)',
         render_word('m', affixes=[('ţř', 5, 1, 5)], case_type=0, case_num=1)),
        ('talk-ERG (DYN)',
         render_word('l', case_type=0, case_num=7, func='DYN')),
        ('cat-THM (S2, geminate rr)',
         render_word('rr', case_type=0, case_num=1, stem=2)),
        ('clown+affix-ABS (CRO/HAB)',
         render_word('kš', affixes=[('r', 4, 2, 7)], case_type=0, case_num=3,
                     valence='CRO', aspect='HAB')),
    ]
    for label, svg in words:
        inner = _extract_svg_inner(svg)
        page_parts.append(f'<text x="30" y="{y + 10}" font-size="8" fill="#999">{label}</text>')
        page_parts.append(f'<g transform="translate(30,{y + 15})">{inner}</g>')
        y += 130

    # --- Section 2: All Secondary Characters ---
    page_parts.append(f'<text x="20" y="{y}" font-size="11" font-weight="bold" '
                      f'fill="#333">Secondary Characters (31 consonants)</text>')
    y += 10

    cols = 11
    cell_w, cell_h = 55, 70
    for idx, cons in enumerate(CONSONANT_ORDER):
        if cons not in SECONDARY:
            continue
        col = idx % cols
        row = idx // cols
        cx = 30 + col * cell_w
        cy = y + row * cell_h
        # Label
        label = cons if cons != "'" else "&#x2019;"
        page_parts.append(f'<text x="{cx + cell_w//2}" y="{cy + 10}" text-anchor="middle" '
                          f'font-size="8" fill="#666">{label}</text>')
        # Glyph
        glyph = SECONDARY[cons]
        s = 0.065
        gx, gy = cx + 5, cy + 15 + 45
        page_parts.append(
            f'<g transform="translate({gx},{gy}) scale({s},{-s})">'
            f'<path d="{glyph["path"]}" fill="black" fill-rule="nonzero"/></g>')

    y += (len(CONSONANT_ORDER) // cols + 1) * cell_h + 5

    # --- Section 3: Primary Character Variations ---
    page_parts.append(f'<text x="20" y="{y}" font-size="11" font-weight="bold" '
                      f'fill="#333">Primary Character Variations</text>')
    y += 10

    pri_examples = [
        ('BSC/EXS/S1/STA', dict(spec='BSC', ctx='EXS', stem=1, func='STA')),
        ('CTE/FNC/S2/STA', dict(spec='CTE', ctx='FNC', stem=2, func='STA')),
        ('CSV/RPS/S3/DYN', dict(spec='CSV', ctx='RPS', stem=3, func='DYN')),
        ('OBJ/AMG/S1/CPT', dict(spec='OBJ', ctx='AMG', stem=1, func='STA', ver='CPT')),
    ]
    for i, (label, kwargs) in enumerate(pri_examples):
        px = 30 + i * 130
        page_parts.append(f'<text x="{px + 25}" y="{y + 10}" text-anchor="middle" '
                          f'font-size="7" fill="#999">{label}</text>')
        elems = draw_primary(px, y + 15, 50, 80, **kwargs)
        page_parts.extend(elems)
    y += 110

    # --- Section 4: Quaternary Case Types ---
    page_parts.append(f'<text x="20" y="{y}" font-size="11" font-weight="bold" '
                      f'fill="#333">Quaternary: 8 Case Types</text>')
    y += 10

    case_type_names = ['TRANS', 'APPOS', 'ASSOC', 'ADVERB', 'RELAT', 'AFFIN', 'SPAT-1', 'SPAT-2']
    for ct in range(8):
        qx = 30 + ct * 60
        elems = draw_quaternary_case(qx, y, 50, 80, case_type=ct, case_num=ct + 1)
        page_parts.extend(elems)
        page_parts.append(f'<text x="{qx + 25}" y="{y + 95}" text-anchor="middle" '
                          f'font-size="6" fill="#999">{case_type_names[ct]}</text>')
    y += 110

    # --- Section 5: Case Numbers 1-9 ---
    page_parts.append(f'<text x="20" y="{y}" font-size="11" font-weight="bold" '
                      f'fill="#333">Quaternary: 9 Case Numbers</text>')
    y += 10

    for cn in range(1, 10):
        qx = 30 + (cn - 1) * 55
        elems = draw_quaternary_case(qx, y, 50, 80, case_type=0, case_num=cn)
        page_parts.extend(elems)
        page_parts.append(f'<text x="{qx + 25}" y="{y + 95}" text-anchor="middle" '
                          f'font-size="7" fill="#999">Case {cn}</text>')
    y += 110

    # --- Section 6: Degree Diacritics ---
    page_parts.append(f'<text x="20" y="{y}" font-size="11" font-weight="bold" '
                      f'fill="#333">Degree Diacritics (1-9)</text>')
    y += 10

    for deg in range(1, 10):
        dx = 30 + (deg - 1) * 55
        # Draw a sample consonant with the degree
        r = FormativeRenderer()
        r.x_cursor = 0
        r.add_secondary('m', degree=deg)
        inner = '\n'.join(e for e in r.elements if e)
        page_parts.append(f'<g transform="translate({dx},{y})">{inner}</g>')
        page_parts.append(f'<text x="{dx + 25}" y="{y + 95}" text-anchor="middle" '
                          f'font-size="7" fill="#999">Deg {deg}</text>')
    y += 110

    page_parts.append('</svg>')

    output = 'script/test_words.svg'
    with open(output, 'w') as f:
        f.write('\n'.join(page_parts))
    print(f'Wrote {output}')
    return output


def _extract_svg_inner(svg_string):
    """Extract inner content from an SVG string, removing wrapper tags."""
    return '\n'.join(line for line in svg_string.split('\n')
                      if not line.startswith('<svg') and not line.startswith('</svg')
                      and not line.startswith('<rect width="100%"'))


if __name__ == '__main__':
    render_test_words()
