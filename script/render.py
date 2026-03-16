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

from glyphs import SECONDARY, CONSONANT_ORDER, _outline as L, _arc as A, _glyph


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

    # --- Configuration (under-posed mark below the bar) ---
    ux, uy = bx1 + 2, by1 + 6
    if config == 'UNI':
        pass  # default, no mark
    elif config == 'DPX':
        elements.append(svg_line(ux - 4, uy, ux + 4, uy, 1.5))
        elements.append(svg_line(ux - 4, uy + 3, ux + 4, uy + 3, 1.5))

    # --- Relation (subscript diacritic beneath) ---
    rx, ry = bx1, by1 + 12
    if relation == 'UNFRAMED_VERB':
        elements.append(svg_polygon([
            (rx, ry - 2), (rx + 2, ry), (rx, ry + 2), (rx - 2, ry),
        ]))
    elif relation == 'FRAMED_VERB':
        elements.append(svg_line(rx - 4, ry, rx + 4, ry, 2))

    return elements


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

    # Additional valence markers
    if vi >= 1:
        # Back notch/mark for non-MNO
        mark_x = shaft_x1 + 4
        mark_count = min(vi, 4)
        for i in range(mark_count):
            mx = mark_x + i * 4
            elements.append(svg_line(mx, shaft_y - 3, mx, shaft_y + 3, 1.5))
    if vi >= 5:
        # Extra bar behind arrowhead for higher valences
        bx = head_x - 4
        elements.append(svg_line(bx, shaft_y - 5, bx, shaft_y + 5, 1.5))

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
        s = self.GLYPH_SCALE
        x = self.x_cursor
        y = 10 + self.CHAR_HEIGHT  # baseline

        transform = f'translate({x},{y}) scale({s},{-s})'
        if rotated:
            cx_g, cy_g = 250, 500
            transform = f'translate({x},{y}) scale({s},{-s}) rotate(180 {cx_g} {cy_g})'

        self.elements.append(
            f'<g transform="{transform}">'
            f'<path d="{glyph["path"]}" fill="black" fill-rule="nonzero"/></g>'
        )

        # Degree diacritic below
        if degree is not None:
            dy = y + 5
            _draw_degree_diac(self.elements, x + self.CHAR_WIDTH // 2, dy, degree)

        # Affix type diacritic above
        if affix_type == 2:
            self.elements.append(svg_circle(x + self.CHAR_WIDTH // 2, 10 - 3, 2))
        elif affix_type == 3:
            self.elements.append(svg_line(
                x + self.CHAR_WIDTH // 2 - 5, 10 - 3,
                x + self.CHAR_WIDTH // 2 + 5, 10 - 3, 1.5))

        self.x_cursor += self.CHAR_WIDTH + self.SPACING

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

    # 2. Root consonant(s)
    for c in render_consonant_cluster(root_consonants):
        r.add_secondary(c)

    # 3. Slot V affixes (not rotated)
    if affixes:
        for cs, degree, atype, slot in affixes:
            rotated = (slot == 7)
            for c in render_consonant_cluster(cs):
                r.add_secondary(c, rotated=rotated, degree=degree, affix_type=atype)

    # 4. Tertiary character (if non-default valence/aspect/phase/effect)
    has_tertiary = (valence != 'MNO' or aspect or phase or effect)
    if has_tertiary:
        r.add_tertiary(valence=valence, aspect=aspect, phase=phase, effect=effect)

    # 5. Quaternary character (case)
    r.add_quaternary(case_type=case_type, case_num=case_num)

    return r.to_svg()


def render_test_words():
    """Render test words to verify the pipeline."""
    words = []

    # "Malëuţřait" - S1/BSC/EXS/STA, root -m-, affix -ţř- (degree 5), case THM
    svg1 = render_word('m', affixes=[('ţř', 5, 1, 5)], case_type=0, case_num=1,
                        spec='BSC', ctx='EXS', stem=1, func='STA')
    words.append(('Malëuţřait (THM)', svg1))

    # Simple word: root -l- (talk), DYN function, ERG case
    svg2 = render_word('l', case_type=0, case_num=7, func='DYN')
    words.append(('talk-ERG (DYN)', svg2))

    # Root -rr- (cat), THM case, stem 2
    svg3 = render_word('rr', case_type=0, case_num=1, stem=2)
    words.append(('cat-THM (S2)', svg3))

    # Root with affix and tertiary: -kš- + affix, ABS case, CRO valence, HAB aspect
    svg4 = render_word('kš', affixes=[('r', 4, 2, 7)], case_type=0, case_num=3,
                        valence='CRO', aspect='HAB')
    words.append(('complex+CRO/HAB-ABS', svg4))

    # All 8 case types demonstrated
    case_type_names = ['TRANS', 'APPOS', 'ASSOC', 'ADVERB', 'RELAT', 'AFFIN', 'SPAT1', 'SPAT2']
    case_svgs = []
    for ct in range(8):
        r = FormativeRenderer()
        r.add_quaternary(case_type=ct, case_num=ct + 1)
        case_svgs.append(r)

    # Compose into test page
    page_parts = [
        '<svg xmlns="http://www.w3.org/2000/svg" width="900" height="900" viewBox="0 0 900 900">',
        '<rect width="100%" height="100%" fill="#faf8f0"/>',
        '<text x="20" y="25" font-size="14" font-family="sans-serif" fill="#333">'
        'Ithkuil V4 Script - Test Words</text>',
    ]

    y_off = 40
    for label, svg_content in words:
        inner = '\n'.join(line for line in svg_content.split('\n')
                          if not line.startswith('<svg') and not line.startswith('</svg')
                          and not line.startswith('<rect width="100%"'))
        page_parts.append(f'<text x="20" y="{y_off + 10}" font-size="10" fill="#999">{label}</text>')
        page_parts.append(f'<g transform="translate(20,{y_off + 15})">{inner}</g>')
        y_off += 140

    # Case types showcase
    page_parts.append(f'<text x="20" y="{y_off + 10}" font-size="10" fill="#999">'
                      'Case Types (TRANS through SPAT2, cases 1-8):</text>')
    for i, r in enumerate(case_svgs):
        inner = '\n'.join(e for e in r.elements if e)
        page_parts.append(f'<g transform="translate({20 + i * 60},{y_off + 15})">{inner}</g>')
        page_parts.append(f'<text x="{20 + i * 60 + 25}" y="{y_off + 135}" '
                          f'text-anchor="middle" font-size="7" fill="#999">{case_type_names[i]}</text>')

    page_parts.append('</svg>')

    output = 'script/test_words.svg'
    with open(output, 'w') as f:
        f.write('\n'.join(page_parts))
    print(f'Wrote {output}')
    return output


if __name__ == '__main__':
    render_test_words()
