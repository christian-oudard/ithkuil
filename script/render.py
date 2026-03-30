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

from glyphs import (SECONDARY as _SECONDARY_OUTLINE, CONSONANT_ORDER,
                     _outline as L, _arc as A, _glyph,
                     CONS_EXT_TOP, CONS_EXT_BOT)

# Use pen-based glyphs (proper joins, caps) with outline fallback
try:
    from pen_glyphs import build_pen_secondary
    SECONDARY = build_pen_secondary()
    # Fill in any missing characters from the outline-based definitions
    for c in CONSONANT_ORDER:
        if c not in SECONDARY and c in _SECONDARY_OUTLINE:
            SECONDARY[c] = _SECONDARY_OUTLINE[c]
except Exception:
    SECONDARY = _SECONDARY_OUTLINE


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
    # Reference shows a very thick bar, roughly 35% of character width
    bar_w = w * 0.35
    bx1, by1 = x + w * 0.10, y + h * 0.88  # bottom-left
    bx2, by2 = x + w * 0.90, y + h * 0.12  # top-right

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
    # Reference: BSC=plain, CTE=single stroke, CSV=zigzag, OBJ=reversed zigzag
    # Marks appear at the lower-left of the bar
    sx, sy = bx1 - nx * 0.6, by1 - ny * 0.6  # just outside lower-left edge
    spec_sw = 2.5
    if spec == 'CTE':
        elements.append(svg_line(sx - 6, sy - 4, sx + 2, sy - 14, spec_sw))
    elif spec == 'CSV':
        elements.append(svg_line(sx - 4, sy - 2, sx + 4, sy - 12, spec_sw))
        elements.append(svg_line(sx + 4, sy - 12, sx - 4, sy - 20, spec_sw))
    elif spec == 'OBJ':
        elements.append(svg_line(sx - 4, sy - 14, sx + 4, sy - 4, spec_sw))
        elements.append(svg_line(sx + 4, sy - 4, sx - 4, sy + 4, spec_sw))

    # --- Context (super-posed diacritic above the bar) ---
    cx_d, cy_d = (bx1 + bx2) / 2, by2 - 12
    if ctx == 'EXS':
        # Diamond
        elements.append(svg_polygon([
            (cx_d, cy_d - 4), (cx_d + 3.5, cy_d),
            (cx_d, cy_d + 4), (cx_d - 3.5, cy_d),
        ]))
    elif ctx == 'FNC':
        # Horizontal bar
        elements.append(svg_line(cx_d - 7, cy_d, cx_d + 7, cy_d, 2.5))
    elif ctx == 'RPS':
        # Angled stroke (backslash)
        elements.append(svg_line(cx_d - 6, cy_d - 4, cx_d + 6, cy_d + 4, 2.5))
    elif ctx == 'AMG':
        # Reversed angled stroke (slash)
        elements.append(svg_line(cx_d + 6, cy_d - 4, cx_d - 6, cy_d + 4, 2.5))

    # --- Stem/Function/Version marks (lower-right zone) ---
    # Reference 12_1_stem_func_version_plexity.png shows small angular marks
    # alongside the lower-right portion of the bar
    mx = bx1 + (bx2 - bx1) * 0.35
    my = by1 + (by2 - by1) * 0.35
    # Direction along the bar (toward upper-right)
    udx = dx / length
    udy = dy / length
    # Perpendicular outward from lower-right edge
    pdx = -ny / (bar_w / 2)  # normalized perpendicular
    pdy = nx / (bar_w / 2)
    mark_len = 8
    mark_sw = 2.0

    # Stem shown as small ticks perpendicular to bar
    for i in range(stem):
        tx = mx + (i + 1) * 6 * udx
        ty = my + (i + 1) * 6 * udy
        elements.append(svg_line(tx, ty, tx - pdx * mark_len, ty - pdy * mark_len, mark_sw))

    # Function: DYN adds a parallel thin line alongside the bar
    if func == 'DYN':
        elements.append(svg_line(
            bx1 - nx * 0.7, by1 - ny * 0.7,
            bx2 - nx * 0.7, by2 - ny * 0.7, 1.5))

    # Version: CPT adds a small crossbar at midpoint
    if ver == 'CPT':
        cmx = (bx1 + bx2) / 2
        cmy = (by1 + by2) / 2
        elements.append(svg_line(cmx - nx * 1.5, cmy - ny * 1.5,
                                  cmx + nx * 1.5, cmy + ny * 1.5, mark_sw))

    # --- Perspective/Extension (upper-left zone) ---
    px, py = bx2 + nx * 0.6 - 8, by2 + ny * 0.6 + 5
    if persp != 'M' or ext != 'DEL':
        _draw_persp_ext_mark(elements, px, py, persp, ext)

    # --- Affiliation/Essence (upper-right zone) ---
    ax, ay = bx2 - nx * 0.6 + 5, by2 - ny * 0.6 + 8
    if affil != 'CSL' or ess != 'NRM':
        _draw_affil_ess_mark(elements, ax, ay, affil, ess)

    # --- Configuration (under-posed mark below the bar) ---
    ux, uy = bx1 + 2, by1 + ny + 8
    _draw_config_mark(elements, ux, uy, config)

    # --- Relation (subscript diacritic beneath) ---
    rx, ry = bx1, by1 + ny + 16
    if relation == 'UNFRAMED_VERB':
        elements.append(svg_polygon([
            (rx, ry - 3), (rx + 3, ry), (rx, ry + 3), (rx - 3, ry),
        ]))
    elif relation == 'FRAMED_VERB':
        elements.append(svg_line(rx - 5, ry, rx + 5, ry, 2.5))

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

    Reference 12_4_quaternary_chars.png shows bold, thick strokes:
    - Thick vertical stem
    - Bold top extension for case type (8 shapes)
    - Bold bottom extension for case number (9 shapes)
    - Mood shown as superposed diacritic
    """
    elements = []
    cx = x + w / 2
    top = y + 8
    bot = y + h - 8
    sw = 4.5  # thick stroke width matching reference

    # Main vertical stem
    elements.append(svg_line(cx, top, cx, bot, sw))

    # --- Case Type (top extension) ---
    # From reference 12_4: 8 bold shapes
    if case_type == 0:
        # TRANSRELATIVE: plain vertical (no extra extension)
        pass
    elif case_type == 1:
        # APPOSITIVE: bold right curve/hook from top
        elements.append(svg_path(
            f'M{cx:.1f},{top:.1f} Q{cx+18:.1f},{top:.1f} {cx+14:.1f},{top+18:.1f}', sw))
    elif case_type == 2:
        # ASSOCIATIVE: bold Y-fork at top
        elements.append(svg_line(cx, top, cx + 12, top - 10, sw))
        elements.append(svg_line(cx, top, cx - 12, top - 10, sw))
    elif case_type == 3:
        # ADVERBIAL: bold left curve from top
        elements.append(svg_path(
            f'M{cx:.1f},{top:.1f} Q{cx+20:.1f},{top+6:.1f} {cx+14:.1f},{top+20:.1f}', sw))
    elif case_type == 4:
        # RELATIONAL: bold left hook at top
        elements.append(svg_path(
            f'M{cx:.1f},{top:.1f} Q{cx-18:.1f},{top:.1f} {cx-14:.1f},{top+18:.1f}', sw))
    elif case_type == 5:
        # AFFINITIVE: bold T-bar at top
        elements.append(svg_line(cx - 14, top, cx + 14, top, sw))
    elif case_type == 6:
        # SPATIO-TEMPORAL I: T-bar + right hook
        elements.append(svg_line(cx - 14, top, cx + 14, top, sw))
        elements.append(svg_path(
            f'M{cx+14:.1f},{top:.1f} Q{cx+22:.1f},{top+3:.1f} {cx+18:.1f},{top+14:.1f}', sw * 0.7))
    elif case_type == 7:
        # SPATIO-TEMPORAL II: T-bar + left hook
        elements.append(svg_line(cx - 14, top, cx + 14, top, sw))
        elements.append(svg_path(
            f'M{cx-14:.1f},{top:.1f} Q{cx-22:.1f},{top+3:.1f} {cx-18:.1f},{top+14:.1f}', sw * 0.7))

    # --- Case Number (bottom extension) ---
    if case_num >= 1:
        _draw_case_num_extension(elements, cx, bot, case_num, sw)

    # --- Mood diacritic (superposed) ---
    if mood:
        my = top - 8
        _draw_mood_diac(elements, cx, my, mood)

    return elements


def _draw_case_num_extension(elements, cx, bot, num, sw):
    """Draw case number bottom extension (9 bold forms).

    Reference 12_4: thick strokes matching the stem weight.
    """
    elen = 18  # extension length (bigger for visibility)
    if num == 1:
        # Plain short downward stub
        elements.append(svg_line(cx, bot, cx, bot + elen * 0.5, sw))
    elif num == 2:
        # Right hook (J-shape)
        elements.append(svg_path(
            f'M{cx:.1f},{bot:.1f} L{cx:.1f},{bot+8:.1f} '
            f'Q{cx+10:.1f},{bot+16:.1f} {cx+14:.1f},{bot+8:.1f}', sw))
    elif num == 3:
        # Left hook (reversed J)
        elements.append(svg_path(
            f'M{cx:.1f},{bot:.1f} L{cx:.1f},{bot+8:.1f} '
            f'Q{cx-10:.1f},{bot+16:.1f} {cx-14:.1f},{bot+8:.1f}', sw))
    elif num == 4:
        # Right curve/swoop
        elements.append(svg_path(
            f'M{cx:.1f},{bot:.1f} Q{cx+18:.1f},{bot+10:.1f} {cx+12:.1f},{bot+20:.1f}', sw))
    elif num == 5:
        # Inverted T (vertical + horizontal bar)
        elements.append(svg_line(cx, bot, cx, bot + elen, sw))
        elements.append(svg_line(cx - 10, bot + elen, cx + 10, bot + elen, sw))
    elif num == 6:
        # S-curve
        elements.append(svg_path(
            f'M{cx:.1f},{bot:.1f} Q{cx+12:.1f},{bot+6:.1f} {cx:.1f},{bot+14:.1f} '
            f'Q{cx-10:.1f},{bot+20:.1f} {cx:.1f},{bot+24:.1f}', sw))
    elif num == 7:
        # Left flag (vertical + left tick)
        elements.append(svg_line(cx, bot, cx, bot + elen, sw))
        elements.append(svg_line(cx, bot + 5, cx - 12, bot + 10, sw))
    elif num == 8:
        # Right flag (vertical + right tick)
        elements.append(svg_line(cx, bot, cx, bot + elen, sw))
        elements.append(svg_line(cx, bot + 5, cx + 12, bot + 10, sw))
    elif num == 9:
        # Fork at bottom (two diverging lines)
        elements.append(svg_line(cx, bot, cx, bot + 10, sw))
        elements.append(svg_line(cx, bot + 10, cx + 10, bot + elen, sw))
        elements.append(svg_line(cx, bot + 10, cx - 10, bot + elen, sw))


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

    Reference 12_4: bold vertical stems with thick top curves (illocution)
    and bottom hooks (validation).
    """
    elements = []
    cx = x + w / 2
    top = y + 5
    bot = y + h - 5
    sw = 4.5  # match case quaternary thickness

    # Main vertical stem
    elements.append(svg_line(cx, top, cx, bot, sw))

    # Illocution (top extension)
    illoc_shapes = {
        'ASR': [],  # plain
        'DIR': [('curve_r', 16)],
        'DEC': [('hook_r', 14), ('tick_down', 8)],
        'IRG': [('bar', 14)],
        'VRF': [('hook_l', 14), ('tick_down', 8)],
        'ADM': [('curve_l', 16)],
        'POT': [('bar', 12), ('hook_r', 8)],
        'HOR': [('curve_r', 20)],
        'CNJ': [('bar', 12), ('hook_l', 8)],
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
            elements.append(svg_line(cx, top, cx, top + size, sw * 0.7))

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
    """Draw a tertiary character as bold filled arrow shapes.

    Reference (12_3_tertiary_chars.png):
    - Valence: thick filled right-pointing arrow (9 forms)
    - Aspect/Phase/Effect: bold marks in upper-right portion
    """
    elements = []
    cy = y + h / 2

    # ---- Valence: bold filled arrow polygon ----
    # Arrow dimensions within the 50x100 cell
    left = x + 2
    right = x + w - 2
    shaft_h = 6      # half-height of shaft rectangle
    head_h = 14      # half-height of arrowhead tips
    neck = left + (right - left) * 0.55  # where shaft meets arrowhead

    val_names = ['MNO', 'PRL', 'CRO', 'RCP', 'CPL', 'DUP', 'DEM', 'CNG', 'PTI']
    vi = val_names.index(valence) if valence in val_names else 0

    # Base filled arrow polygon: shaft rectangle + arrowhead triangle
    arrow_pts = [
        (left, cy - shaft_h),
        (neck, cy - shaft_h),
        (neck, cy - head_h),
        (right, cy),
        (neck, cy + head_h),
        (neck, cy + shaft_h),
        (left, cy + shaft_h),
    ]
    elements.append(svg_polygon(arrow_pts))

    # Valence modifications (overlaid on the base arrow)
    msw = 2.5
    if vi == 1:  # PRL: vertical bar across shaft near back
        bx = left + 10
        elements.append(svg_line(bx, cy - shaft_h - 4, bx, cy + shaft_h + 4, msw))
    elif vi == 2:  # CRO: curve connecting arrowhead tips
        elements.append(svg_path(
            f'M{neck:.1f},{cy - head_h:.1f} '
            f'Q{right + 5:.1f},{cy:.1f} '
            f'{neck:.1f},{cy + head_h:.1f}', 2, fill='none'))
    elif vi == 3:  # RCP: closed triangle (bar connecting tips)
        elements.append(svg_line(neck + 2, cy - head_h + 1,
                                  neck + 2, cy + head_h - 1, msw))
    elif vi == 4:  # CPL: bar across shaft behind arrowhead
        bx = neck - 6
        elements.append(svg_line(bx, cy - shaft_h - 5, bx, cy + shaft_h + 5, msw))
    elif vi == 5:  # DUP: two bars across shaft
        elements.append(svg_line(left + 8, cy - shaft_h - 3,
                                  left + 8, cy + shaft_h + 3, msw))
        elements.append(svg_line(left + 16, cy - shaft_h - 3,
                                  left + 16, cy + shaft_h + 3, msw))
    elif vi == 6:  # DEM: lower tip extends down-left
        elements.append(svg_line(neck, cy + head_h,
                                  neck - 6, cy + head_h + 7, msw))
    elif vi == 7:  # CNG: upper tip extends up-left
        elements.append(svg_line(neck, cy - head_h,
                                  neck - 6, cy - head_h - 7, msw))
    elif vi == 8:  # PTI: tips curve outward
        elements.append(svg_line(neck, cy - head_h,
                                  neck + 5, cy - head_h - 5, 2))
        elements.append(svg_line(neck, cy + head_h,
                                  neck + 5, cy + head_h + 5, 2))

    # ---- Aspect mark (upper-right zone, bold filled chevron) ----
    if aspect:
        _draw_aspect_mark(elements, x, y, w, h, aspect)

    # ---- Phase mark (double vertical strokes in upper-right) ----
    if phase:
        _draw_phase_mark(elements, x, y, w, h, phase)

    # ---- Effect mark (V-shapes in upper-right) ----
    if effect:
        _draw_effect_mark(elements, x, y, w, h, effect)

    return elements


def _draw_filled_chevron(elements, cx, cy, direction, size=8, spread=5):
    """Draw a small bold filled chevron/arrow pointing in the given direction.

    Used for aspect marks. direction: 0=right, 1=down, 2=left, 3=up.
    """
    s, sp = size, spread
    if direction == 0:  # right
        pts = [(cx, cy - sp), (cx + s, cy), (cx, cy + sp),
               (cx + s * 0.4, cy)]
    elif direction == 1:  # down
        pts = [(cx - sp, cy), (cx, cy + s), (cx + sp, cy),
               (cx, cy + s * 0.4)]
    elif direction == 2:  # left
        pts = [(cx, cy - sp), (cx - s, cy), (cx, cy + sp),
               (cx - s * 0.4, cy)]
    elif direction == 3:  # up
        pts = [(cx - sp, cy), (cx, cy - s), (cx + sp, cy),
               (cx, cy - s * 0.4)]
    elements.append(svg_polygon(pts))


def _draw_aspect_mark(elements, x, y, w, h, aspect):
    """Draw aspect as bold filled chevron in upper-right zone.

    36 aspects: 4 rows of 9, each row points in a different direction.
    Row 0 (RTR-ATP): right, Row 1 (RSM-IRP): down,
    Row 2 (PMP-PPR): left, Row 3 (DCL-SQN): up.
    """
    aspect_names = [
        'RTR', 'PRS', 'HAB', 'PRG', 'IMM', 'PCS', 'REG', 'SMM', 'ATP',
        'RSM', 'CSS', 'PAU', 'RGR', 'PCL', 'CNT', 'ICS', 'EXP', 'IRP',
        'PMP', 'CLM', 'DLT', 'TMP', 'XPD', 'LIM', 'EPD', 'PTC', 'PPR',
        'DCL', 'CCL', 'CUL', 'IMD', 'TRD', 'TNS', 'ITC', 'MTV', 'SQN',
    ]
    if aspect not in aspect_names:
        return
    idx = aspect_names.index(aspect)
    row = idx // 9
    col = idx % 9

    # Position in upper-right quadrant
    ax = x + w * 0.72
    ay = y + h * 0.28

    # Draw base chevron
    _draw_filled_chevron(elements, ax, ay, direction=row, size=10, spread=6)

    # Row-specific distinguishing marks (small ticks)
    sw = 1.5
    if col >= 1:
        # Add small tick marks to distinguish the 9 forms within each row
        for i in range(min(col, 4)):
            if row == 0:
                elements.append(svg_line(ax - 4 + i * 3, ay + 9,
                                          ax - 4 + i * 3, ay + 12, sw))
            elif row == 1:
                elements.append(svg_line(ax + 9, ay - 4 + i * 3,
                                          ax + 12, ay - 4 + i * 3, sw))
            elif row == 2:
                elements.append(svg_line(ax + 4 - i * 3, ay + 9,
                                          ax + 4 - i * 3, ay + 12, sw))
            elif row == 3:
                elements.append(svg_line(ax - 9, ay + 4 - i * 3,
                                          ax - 12, ay + 4 - i * 3, sw))
    if col >= 5:
        # Additional mark for 5-8
        mx = ax + (6 if row in (0, 1) else -6)
        my = ay + (6 if row in (0, 2) else -6)
        elements.append(svg_line(mx - 2, my, mx + 2, my, sw))


def _draw_phase_mark(elements, x, y, w, h, phase):
    """Draw phase as bold double vertical strokes in upper-right zone."""
    phase_names = ['PCT', 'ITR', 'REP', 'ITM', 'RCT', 'FRE', 'FRG', 'VAC', 'FLC']
    if phase not in phase_names:
        return
    idx = phase_names.index(phase)
    px = x + w * 0.72
    py = y + h * 0.18
    ph = h * 0.22
    sw = 2.5

    # Two vertical bars
    elements.append(svg_line(px - 4, py, px - 4, py + ph, sw))
    elements.append(svg_line(px + 4, py, px + 4, py + ph, sw))
    # Connection varies by phase
    if idx >= 1:
        elements.append(svg_line(px - 4, py + ph, px + 4, py + ph, 2))
    if idx >= 4:
        elements.append(svg_line(px - 4, py, px + 4, py, 2))
    if idx >= 7:
        elements.append(svg_line(px - 4, py + ph / 2, px + 4, py + ph / 2, 1.5))


def _draw_effect_mark(elements, x, y, w, h, effect):
    """Draw effect as bold V-shape in upper-right zone."""
    ex = x + w * 0.72
    ey = y + h * 0.2
    sw = 2.5
    elements.append(svg_line(ex - 5, ey, ex, ey + 10, sw))
    elements.append(svg_line(ex + 5, ey, ex, ey + 10, sw))


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

    def add_bias(self, bias_name):
        """Add a bias character.

        From reference 12_5_bias_chars.png: 4 base forms with variant marks.
        Column 1 (ACC-CTV): S-curve ("3" shape)
        Column 2 (DCC-FOR): reversed S-curve
        Column 3 (FSC-PSC): sigma shape
        Column 4 (PSM-VEX): reversed sigma
        """
        x = self.x_cursor
        y = 10
        h = self.CHAR_HEIGHT
        w = self.CHAR_WIDTH
        cx = x + w / 2
        cy = y + h / 2
        sw = 2.5

        col1 = ['ACC','ACH','ADS','ANN','ANP','APB','APH','ARB','ATE',
                'CMD','CNV','COI','CRP','CRR','CTP','CTV']
        col2 = ['DCC','DEJ','DES','DFD','DIS','DLC','DOL','DPB','DRS',
                'DUB','EUH','EUP','EXA','EXG','MNF','FOR']
        col3 = ['FSC','GRT','IDG','IFT','IPL','IPT','IRO','ISP','IVD',
                'MAN','OPT','PES','PPT','PPX','PPV','PSC']
        col4 = ['PSM','RAC','RFL','RSG','RPU','RVL','SAT','SGS','SKP',
                'SOL','STU','TRP','VEX']

        if bias_name in col1:
            idx = col1.index(bias_name)
            # S-curve (like "3")
            self.elements.append(svg_path(
                f'M{cx-8:.1f},{y+10:.1f} Q{cx+12:.1f},{y+h*0.33:.1f} {cx-4:.1f},{cy:.1f} '
                f'Q{cx+12:.1f},{y+h*0.67:.1f} {cx-8:.1f},{y+h-10:.1f}', sw))
        elif bias_name in col2:
            idx = col2.index(bias_name)
            # Reversed S-curve
            self.elements.append(svg_path(
                f'M{cx+8:.1f},{y+10:.1f} Q{cx-12:.1f},{y+h*0.33:.1f} {cx+4:.1f},{cy:.1f} '
                f'Q{cx-12:.1f},{y+h*0.67:.1f} {cx+8:.1f},{y+h-10:.1f}', sw))
        elif bias_name in col3:
            idx = col3.index(bias_name)
            # Sigma shape (Σ)
            self.elements.append(svg_line(cx - 10, y + 10, cx + 10, y + 10, sw))
            self.elements.append(svg_line(cx - 10, y + 10, cx + 4, cy, sw))
            self.elements.append(svg_line(cx + 4, cy, cx - 10, y + h - 10, sw))
            self.elements.append(svg_line(cx - 10, y + h - 10, cx + 10, y + h - 10, sw))
        elif bias_name in col4:
            idx = col4.index(bias_name)
            # Reversed sigma
            self.elements.append(svg_line(cx - 10, y + 10, cx + 10, y + 10, sw))
            self.elements.append(svg_line(cx + 10, y + 10, cx - 4, cy, sw))
            self.elements.append(svg_line(cx - 4, cy, cx + 10, y + h - 10, sw))
            self.elements.append(svg_line(cx - 10, y + h - 10, cx + 10, y + h - 10, sw))
        else:
            idx = 0

        # Variant mark (small distinguishing stroke based on index within column)
        if idx > 0:
            mx = cx + 12
            my = cy - 8 + (idx % 4) * 4
            angle = (idx // 4) * 45
            dx = 4 * math.cos(math.radians(angle))
            dy = 4 * math.sin(math.radians(angle))
            self.elements.append(svg_line(mx, my, mx + dx, my + dy, 1.5))

        self.x_cursor += self.CHAR_WIDTH + self.SPACING

    def add_register(self, register, mode='open'):
        """Add a register marker (open or close).

        From reference 12_6_register_symbols.png:
        6 registers x 4 modes, shown as diamond-based marks.
        """
        x = self.x_cursor
        y = 10
        h = self.CHAR_HEIGHT
        w = self.CHAR_WIDTH * 0.6  # registers are narrower
        cx = x + w / 2
        cy = y + h / 2
        sw = 2

        reg_names = ['NRR', 'DSV', 'PNT', 'CGT', 'EXM', 'SPF']
        ri = reg_names.index(register) if register in reg_names else 0

        # Base: diamond shape, size varies by register
        sz = 6 + ri
        pts = [(cx, cy - sz), (cx + sz, cy), (cx, cy + sz), (cx - sz, cy)]
        self.elements.append(svg_polygon(pts))

        # Register count indicator (ticks below)
        for i in range(ri):
            self.elements.append(svg_line(cx - 4 + i * 3, cy + sz + 4,
                                          cx - 4 + i * 3, cy + sz + 8, 1.5))

        # Close marker: add horizontal bar through diamond
        if mode == 'close':
            self.elements.append(svg_line(cx - sz - 3, cy, cx + sz + 3, cy, 1.5))

        self.x_cursor += int(w) + self.SPACING

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
    page_w, page_h = 950, 1650
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

    # --- Section 7: Bias Characters (sample from each column) ---
    page_parts.append(f'<text x="20" y="{y}" font-size="11" font-weight="bold" '
                      f'fill="#333">Bias Characters (4 base forms)</text>')
    y += 10

    bias_samples = [('ACC', 'S-curve'), ('DCC', 'Rev S'), ('FSC', 'Sigma'), ('PSM', 'Rev Σ'),
                    ('IRO', 'Col3'), ('DOL', 'Col2'), ('CRP', 'Col1'), ('VEX', 'Col4')]
    for i, (bias, label) in enumerate(bias_samples):
        bx = 30 + i * 65
        r = FormativeRenderer()
        r.x_cursor = 0
        r.add_bias(bias)
        inner = '\n'.join(e for e in r.elements if e)
        page_parts.append(f'<g transform="translate({bx},{y})">{inner}</g>')
        page_parts.append(f'<text x="{bx + 25}" y="{y + 95}" text-anchor="middle" '
                          f'font-size="6" fill="#999">{bias} ({label})</text>')
    y += 110

    # --- Section 8: Register Markers ---
    page_parts.append(f'<text x="20" y="{y}" font-size="11" font-weight="bold" '
                      f'fill="#333">Register Markers</text>')
    y += 10

    reg_names = ['NRR', 'DSV', 'PNT', 'CGT', 'EXM', 'SPF']
    for i, reg in enumerate(reg_names):
        rx = 30 + i * 75
        r = FormativeRenderer()
        r.x_cursor = 0
        r.add_register(reg, 'open')
        r.add_register(reg, 'close')
        inner = '\n'.join(e for e in r.elements if e)
        page_parts.append(f'<g transform="translate({rx},{y})">{inner}</g>')
        page_parts.append(f'<text x="{rx + 20}" y="{y + 95}" text-anchor="middle" '
                          f'font-size="7" fill="#999">{reg}</text>')
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
