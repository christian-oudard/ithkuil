#!/usr/bin/env python3
"""
Ithkuil V4 Script - SVG Renderer

Renders formatives and sentences as SVG using the glyph definitions.
Takes parsed formative data and produces properly laid-out SVG output.
"""
import sys, os
sys.path.insert(0, os.path.dirname(__file__))

from glyphs import SECONDARY, CONSONANT_ORDER, _outline as L, _arc as A, _glyph

# ============================================================================
# Quaternary Character Glyphs
# ============================================================================
# Case Type (top extension on vertical stem): 8 case groups
# From reference: vertical stem with varying top hooks

CASE_TYPE_GLYPHS = {}
# Based on the reference image analysis:
# 1=TRANS: plain vertical
# 2=APPOS: right hook at top
# 3=ASSOC: fork at top
# 4=ADVERB: right curve
# 5=RELAT: left hook
# 6=AFFIN: T-bar
# 7=SPAT1: T with right hook
# 8=SPAT2: T with left hook

_case_type_shapes = {
    'TRANS':  [L(200, 200, 200, 900)],
    'APPOS':  [L(200, 200, 200, 900), L(200, 900, 350, 800)],
    'ASSOC':  [L(200, 200, 200, 900), L(200, 900, 350, 850), L(200, 900, 50, 850)],
    'ADVERB': [L(200, 200, 200, 900), A(200, 750, 150, 0, 90)],
    'RELAT':  [L(200, 200, 200, 900), L(200, 900, 50, 800)],
    'AFFIN':  [L(200, 200, 200, 900), L(100, 900, 350, 900)],
    'SPAT1':  [L(200, 200, 200, 900), L(100, 900, 350, 900), L(350, 900, 400, 800)],
    'SPAT2':  [L(200, 200, 200, 900), L(100, 900, 350, 900), L(100, 900, 50, 800)],
}
for name, parts in _case_type_shapes.items():
    CASE_TYPE_GLYPHS[name] = _glyph(f'qtype_{name}', '', parts, 400)

# Case Number (bottom extension): 9 case positions within group
CASE_NUM_GLYPHS = {}
_case_num_shapes = {}
for i in range(1, 10):
    # Bottom extensions: different angles/curves
    angle = -30 - (i-1) * 15  # -30 to -150 degrees
    elen = 200
    ex = 200 + elen * 0.8 * ((i % 2) * 2 - 1)  # alternate left/right
    ey = 200 - elen * (0.3 + i * 0.05)
    _case_num_shapes[i] = [L(200, 200, int(ex), int(ey))]
CASE_NUM_GLYPHS = {i: _glyph(f'qnum_{i}', '', parts, 400)
                    for i, parts in _case_num_shapes.items()}

# Illocution glyphs (9)
ILLOCUTION_GLYPHS = {}
_illoc_names = ['ASR', 'DIR', 'DEC', 'IRG', 'VRF', 'ADM', 'POT', 'HOR', 'CNJ']
for i, name in enumerate(_illoc_names):
    # Tall vertical with distinctive top shapes
    top_x = 200 + (i % 3 - 1) * 100
    ILLOCUTION_GLYPHS[name] = _glyph(f'illoc_{name}', '', [
        L(200, 100, 200, 900),
        L(200, 900, top_x, 950 + i * 10),
    ], 400)

# Validation glyphs (9)
VALIDATION_GLYPHS = {}
_valid_names = ['OBS', 'REC', 'PUP', 'RPR', 'IMA', 'CVN', 'ITU', 'INF', 'USP']
for i, name in enumerate(_valid_names):
    bot_x = 200 + (i % 3 - 1) * 80
    VALIDATION_GLYPHS[name] = _glyph(f'valid_{name}', '', [
        L(200, 100, 200, 700),
        L(200, 100, bot_x, 50 - i * 10),
    ], 400)


# ============================================================================
# Degree Diacritics (0-9 + Ca-stacking)
# ============================================================================
DEGREE_DIACRITICS = {}
_degree_shapes = {
    0: [L(180, 50, 220, 50)],                    # short dash (degree 0)
    1: [],                                         # dot
    2: [L(200, 70, 220, 30)],                     # hook
    3: [L(180, 50, 220, 30)],                     # slash
    4: [A(200, 50, 20, 0, 270)],                  # curl
    5: [L(180, 50, 220, 50), L(210, 70, 210, 30)],  # cross
    6: [A(200, 50, 25, 0, 180)],                  # crescent
    7: [L(190, 70, 200, 30), L(200, 30, 210, 70)],  # wedge
    8: [L(180, 50, 190, 30), L(190, 30, 200, 50), L(200, 50, 210, 30)],  # zigzag
    9: [A(200, 50, 25, 0, 300)],                  # arc
}
for deg, parts in _degree_shapes.items():
    DEGREE_DIACRITICS[deg] = _glyph(f'deg_{deg}', '', parts, 100)


# ============================================================================
# SVG Formative Renderer
# ============================================================================

class FormativeRenderer:
    """Renders an Ithkuil formative as SVG."""

    CHAR_WIDTH = 50   # width per character cell in output SVG
    CHAR_HEIGHT = 100  # height of character cell
    SPACING = 5        # inter-character spacing
    GLYPH_SCALE = 0.08  # scale from 1000-unit em to output pixels

    def __init__(self):
        self.elements = []
        self.x_cursor = 10  # current x position

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
            # Rotate 180 degrees around center
            cx, cy = 250 * s, 500 * s
            transform = f'translate({x},{y}) scale({s},{-s}) rotate(180 250 500)'

        self.elements.append(
            f'<g transform="{transform}">'
            f'<path d="{glyph["path"]}" fill="black" fill-rule="nonzero"/></g>'
        )

        # Add degree diacritic below if present
        if degree is not None and degree in DEGREE_DIACRITICS:
            dg = DEGREE_DIACRITICS[degree]
            dy = y + 5
            self.elements.append(
                f'<g transform="translate({x+10},{dy}) scale({s*0.8},{-s*0.8})">'
                f'<path d="{dg["path"]}" fill="black"/></g>'
            )

        # Affix type: dot above for Type 2, bar above for Type 3
        if affix_type == 2:
            self.elements.append(
                f'<circle cx="{x + self.CHAR_WIDTH//2}" cy="{10 - 3}" r="2" fill="black"/>'
            )
        elif affix_type == 3:
            self.elements.append(
                f'<line x1="{x + self.CHAR_WIDTH//2 - 5}" y1="{10 - 3}" '
                f'x2="{x + self.CHAR_WIDTH//2 + 5}" y2="{10 - 3}" stroke="black" stroke-width="1.5"/>'
            )

        self.x_cursor += self.CHAR_WIDTH + self.SPACING

    def add_primary(self, spec='BSC', ctx='EXS', stem=1, func='STA'):
        """Add a primary character (simplified: diagonal bar with diacritics)."""
        x = self.x_cursor
        y = 10
        h = self.CHAR_HEIGHT

        # Main diagonal bar
        self.elements.append(
            f'<line x1="{x+5}" y1="{y+h-5}" x2="{x+self.CHAR_WIDTH-5}" y2="{y+5}" '
            f'stroke="black" stroke-width="4" stroke-linecap="round"/>'
        )

        # Specification shape (lower-left zone)
        spec_shapes = {
            'BSC': '',  # default = plain diagonal
            'CTE': f'<line x1="{x+2}" y1="{y+h//2}" x2="{x+15}" y2="{y+h//2+10}" stroke="black" stroke-width="2"/>',
            'CSV': f'<line x1="{x+2}" y1="{y+h//2+5}" x2="{x+12}" y2="{y+h//2-5}" stroke="black" stroke-width="2"/>',
            'OBJ': f'<line x1="{x+2}" y1="{y+h//2}" x2="{x+12}" y2="{y+h//2}" stroke="black" stroke-width="2"/>',
        }
        self.elements.append(spec_shapes.get(spec, ''))

        # Context diacritic (superposed)
        ctx_marks = {
            'EXS': f'<circle cx="{x+self.CHAR_WIDTH//2}" cy="{y-3}" r="2" fill="black"/>',
            'FNC': f'<line x1="{x+self.CHAR_WIDTH//2-5}" y1="{y-3}" x2="{x+self.CHAR_WIDTH//2+5}" y2="{y-3}" stroke="black" stroke-width="1.5"/>',
            'RPS': f'<line x1="{x+self.CHAR_WIDTH//2-4}" y1="{y-1}" x2="{x+self.CHAR_WIDTH//2+4}" y2="{y-5}" stroke="black" stroke-width="1.5"/>',
            'AMG': f'<line x1="{x+self.CHAR_WIDTH//2+4}" y1="{y-1}" x2="{x+self.CHAR_WIDTH//2-4}" y2="{y-5}" stroke="black" stroke-width="1.5"/>',
        }
        self.elements.append(ctx_marks.get(ctx, ''))

        # Stem marks (small ticks at top-right)
        for i in range(stem):
            sx = x + self.CHAR_WIDTH - 8 + i * 4
            self.elements.append(
                f'<line x1="{sx}" y1="{y+8}" x2="{sx}" y2="{y+14}" stroke="black" stroke-width="1"/>'
            )

        # Function: STA = thin diagonal, DYN = thick
        if func == 'DYN':
            self.elements.append(
                f'<line x1="{x+8}" y1="{y+h-8}" x2="{x+self.CHAR_WIDTH-8}" y2="{y+8}" '
                f'stroke="black" stroke-width="2" stroke-linecap="round"/>'
            )

        self.x_cursor += self.CHAR_WIDTH + self.SPACING

    def add_quaternary(self, case_type=0, case_num=0):
        """Add a quaternary character (case indicator)."""
        x = self.x_cursor
        y = 10
        h = self.CHAR_HEIGHT

        # Vertical stem
        self.elements.append(
            f'<line x1="{x+self.CHAR_WIDTH//2}" y1="{y+10}" '
            f'x2="{x+self.CHAR_WIDTH//2}" y2="{y+h-10}" '
            f'stroke="black" stroke-width="2.5"/>'
        )

        # Top extension (case type)
        cx = x + self.CHAR_WIDTH // 2
        ty = y + 10
        type_marks = {
            0: '',  # TRANS: plain
            1: f'<line x1="{cx}" y1="{ty}" x2="{cx+12}" y2="{ty+10}" stroke="black" stroke-width="2"/>',
            2: f'<line x1="{cx}" y1="{ty}" x2="{cx+10}" y2="{ty+8}" stroke="black" stroke-width="2"/>'
               f'<line x1="{cx}" y1="{ty}" x2="{cx-10}" y2="{ty+8}" stroke="black" stroke-width="2"/>',
            3: f'<path d="M{cx},{ty} Q{cx+15},{ty+15} {cx+10},{ty+20}" fill="none" stroke="black" stroke-width="2"/>',
            4: f'<line x1="{cx}" y1="{ty}" x2="{cx-12}" y2="{ty+10}" stroke="black" stroke-width="2"/>',
            5: f'<line x1="{cx-12}" y1="{ty}" x2="{cx+12}" y2="{ty}" stroke="black" stroke-width="2"/>',
            6: f'<line x1="{cx-12}" y1="{ty}" x2="{cx+12}" y2="{ty}" stroke="black" stroke-width="2"/>'
               f'<line x1="{cx+12}" y1="{ty}" x2="{cx+15}" y2="{ty+10}" stroke="black" stroke-width="1.5"/>',
            7: f'<line x1="{cx-12}" y1="{ty}" x2="{cx+12}" y2="{ty}" stroke="black" stroke-width="2"/>'
               f'<line x1="{cx-12}" y1="{ty}" x2="{cx-15}" y2="{ty+10}" stroke="black" stroke-width="1.5"/>',
        }
        self.elements.append(type_marks.get(case_type, ''))

        # Bottom extension (case number)
        by = y + h - 10
        if case_num > 0:
            angle = -30 - (case_num - 1) * 15
            import math
            elen = 15
            ex = cx + elen * math.cos(math.radians(angle))
            ey = by - elen * math.sin(math.radians(angle))
            self.elements.append(
                f'<line x1="{cx}" y1="{by}" x2="{ex:.0f}" y2="{ey:.0f}" stroke="black" stroke-width="2"/>'
            )

        self.x_cursor += self.CHAR_WIDTH + self.SPACING

    def to_svg(self, width=None, height=130):
        """Generate complete SVG string."""
        if width is None:
            width = self.x_cursor + 10
        parts = [
            f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" '
            f'viewBox="0 0 {width} {height}">',
            f'<rect width="100%" height="100%" fill="white"/>',
        ]
        parts.extend(e for e in self.elements if e)
        parts.append('</svg>')
        return '\n'.join(parts)


# ============================================================================
# High-level rendering API
# ============================================================================

def render_consonant_cluster(cluster):
    """Split a consonant cluster into core + extensions for secondary char rendering."""
    # For now, render each consonant in the cluster separately
    # A proper implementation would use extensions
    consonants = []
    i = 0
    text = cluster.lower()
    while i < len(text):
        # Try 2-char consonants first
        if i + 1 < len(text) and text[i:i+2] in SECONDARY:
            consonants.append(text[i:i+2])
            i += 2
        elif text[i] in SECONDARY or text[i:i+1] in SECONDARY:
            consonants.append(text[i])
            i += 1
        else:
            i += 1  # skip unknown
    return consonants


def render_word(root_consonants, affixes=None, case_type=0, case_num=1,
                spec='BSC', ctx='EXS', stem=1, func='STA'):
    """Render a single word to SVG.

    Args:
        root_consonants: string like "m" or "rr" or "lţk"
        affixes: list of (cs, degree, affix_type, slot) tuples
        case_type: 0-7 for case group
        case_num: 1-9 for case within group
        spec, ctx, stem, func: primary character params
    """
    r = FormativeRenderer()

    # 1. Primary character
    r.add_primary(spec=spec, ctx=ctx, stem=stem, func=func)

    # 2. Root consonant(s) as secondary char(s)
    for c in render_consonant_cluster(root_consonants):
        r.add_secondary(c)

    # 3. Slot V affixes (not rotated)
    if affixes:
        for cs, degree, atype, slot in affixes:
            rotated = (slot == 7)
            for c in render_consonant_cluster(cs):
                r.add_secondary(c, rotated=rotated, degree=degree, affix_type=atype)

    # 4. Quaternary character (case)
    r.add_quaternary(case_type=case_type, case_num=case_num)

    return r.to_svg()


def render_test_words():
    """Render test words to verify the pipeline."""
    words = []

    # "Malëuţřait" - the language's name
    # Root -m-, STA/BSC/EXS, S1/PRC, affix -ţř- (SYS), case POS (Appositive #1)
    svg1 = render_word('m', affixes=[('ţř', 5, 1, 5)], case_type=1, case_num=1,
                        spec='BSC', ctx='EXS', stem=1, func='STA')
    words.append(('maleutrrait', svg1))

    # Simple word: root -l- (talk), ERG case (Transrelative #7)
    svg2 = render_word('l', case_type=0, case_num=7, func='DYN')
    words.append(('talk-ERG', svg2))

    # Root -rr- (cat), THM case
    svg3 = render_word('rr', case_type=0, case_num=1, stem=2)
    words.append(('cat-THM', svg3))

    # Root with affix: -kš- (clown) + -NEG- affix
    svg4 = render_word('kš', affixes=[('r', 4, 1, 7)], case_type=0, case_num=3)
    words.append(('clown+affix-ABS', svg4))

    # Compose into test page
    page_parts = [
        '<svg xmlns="http://www.w3.org/2000/svg" width="800" height="600" viewBox="0 0 800 600">',
        '<rect width="100%" height="100%" fill="#faf8f0"/>',
        '<text x="20" y="25" font-size="14" font-family="sans-serif" fill="#333">Ithkuil V4 Script - Test Words</text>',
    ]

    y_off = 40
    for label, svg_content in words:
        # Extract inner SVG content (skip the outer svg/rect tags)
        inner = '\n'.join(line for line in svg_content.split('\n')
                          if not line.startswith('<svg') and not line.startswith('</svg')
                          and not line.startswith('<rect width="100%"'))
        page_parts.append(f'<text x="20" y="{y_off + 10}" font-size="10" fill="#999">{label}</text>')
        page_parts.append(f'<g transform="translate(20,{y_off + 15})">{inner}</g>')
        y_off += 140

    page_parts.append('</svg>')

    output = 'script/test_words.svg'
    with open(output, 'w') as f:
        f.write('\n'.join(page_parts))
    print(f'Wrote {output}')
    return output


if __name__ == '__main__':
    render_test_words()
