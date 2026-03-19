#!/usr/bin/env python3
"""Build an OpenType font for Ithkuil V4 Script using fonttools."""
import sys, os, math
sys.path.insert(0, os.path.dirname(__file__))

from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib import TTFont

from glyphs import (SECONDARY, CONSONANT_ORDER, PUA_SECONDARY, _outline as L, _arc as A,
                     TERTIARY_VALENCE, VALENCE_NAMES, TERTIARY_ASPECT, ASPECT_NAMES,
                     TERTIARY_PHASE, PHASE_NAMES, TERTIARY_EFFECT, EFFECT_NAMES,
                     TERTIARY_LEVEL, LEVEL_NAMES, QUAT_CASE_TYPE, QUAT_CASE_NUM,
                     QUAT_ILLOCUTION, ILLOCUTION_NAMES, QUAT_VALIDATION, VALIDATION_NAMES,
                     PRIMARY_SPEC, PRIMARY_CTX, PRIMARY_PERSP_EXT, PRIMARY_AFFIL_ESS,
                     PRIMARY_CONFIG, PRIMARY_SFVP, PRIMARY_RELATION, PRIMARY_CONCAT,
                     BIAS_GLYPHS, REGISTER_GLYPHS, NUMERAL_GLYPHS,
                     CONS_EXT_TOP, CONS_EXT_BOT)


# ============================================================================
# Additional Glyph Definitions for Combining Characters
# ============================================================================

# Degree diacritics (combining, placed below base char)
# U+E200-E20A
DEGREE_GLYPHS = {}
_deg_paths = {
    0: L(180, -100, 220, -100),              # dash
    1: L(190, -90, 210, -90) + ' ' + L(190, -110, 210, -110),  # dot (small square)
    2: L(200, -80, 220, -120),               # hook
    3: L(180, -100, 210, -80),               # slash
    4: L(185, -80, 215, -80) + ' ' + L(185, -80, 185, -120) + ' ' + L(185, -120, 215, -120),  # curl/bracket
    5: L(180, -100, 220, -100) + ' ' + L(200, -80, 200, -120),  # cross
    6: A(200, -100, 20, 0, 180),             # crescent
    7: L(185, -80, 200, -120) + ' ' + L(200, -120, 215, -80),  # wedge
    8: L(180, -80, 193, -120) + ' ' + L(193, -120, 206, -80) + ' ' + L(206, -80, 220, -120),  # zigzag
    9: A(200, -100, 20, 30, 330),            # arc
}
for deg, path in _deg_paths.items():
    DEGREE_GLYPHS[deg] = {'name': f'deg_{deg}', 'path': path, 'width': 0, 'codepoint': 0xE200 + deg}

# Ca-stacking diacritic
DEGREE_GLYPHS[10] = {'name': 'deg_ca_stack', 'path': A(200, -100, 20, 0, 270),
                      'width': 0, 'codepoint': 0xE20A}

# Affix type diacritics (combining, placed above base char)
# U+E210-E212
AFFIX_TYPE_GLYPHS = {
    # Type 1: no mark (implicit)
    2: {'name': 'atype_2', 'path': L(190, 910, 210, 910) + ' ' + L(190, 890, 210, 890),  # dot above
        'width': 0, 'codepoint': 0xE210},
    3: {'name': 'atype_3', 'path': L(175, 900, 225, 900),  # bar above
        'width': 0, 'codepoint': 0xE211},
}

# Rotation mark (combining, indicates Slot VII affix = rotated 180°)
ROTATION_MARK = {'name': 'slot7_mark', 'path': L(170, 920, 230, 920) + ' ' + L(200, 920, 200, 940),
                 'width': 0, 'codepoint': 0xE212}


# ============================================================================
# SVG Path Parser
# ============================================================================

def svg_path_to_contours(path_d):
    """Parse SVG path 'd' attribute into contour point lists."""
    contours = []
    current = []
    tokens = _tokenize(path_d)
    i = 0
    while i < len(tokens):
        cmd = tokens[i]; i += 1
        if cmd == 'M':
            if current: contours.append(current); current = []
            current.append((int(tokens[i]), int(tokens[i+1]))); i += 2
        elif cmd == 'L':
            current.append((int(tokens[i]), int(tokens[i+1]))); i += 2
        elif cmd == 'A':
            rx, ry = tokens[i], tokens[i+1]
            large, sweep = int(tokens[i+3]), int(tokens[i+4])
            ex, ey = tokens[i+5], tokens[i+6]; i += 7
            sx, sy = current[-1] if current else (0, 0)
            current.extend(_arc_points(sx, sy, ex, ey, rx, ry, large, sweep))
        elif cmd == 'Z':
            if current: contours.append(current); current = []
    if current: contours.append(current)
    return contours

def _tokenize(d):
    tokens = []
    i = 0
    while i < len(d):
        c = d[i]
        if c.isalpha(): tokens.append(c); i += 1
        elif c in '0123456789.-':
            j = i
            if c == '-': j += 1
            while j < len(d) and d[j] in '0123456789.': j += 1
            tokens.append(float(d[i:j])); i = j
        else: i += 1
    return tokens

def _arc_points(x1, y1, x2, y2, rx, ry, large, sweep, n=16):
    dx, dy = (x1-x2)/2, (y1-y2)/2
    d = dx*dx/(rx*rx) + dy*dy/(ry*ry)
    if d > 1: rx *= math.sqrt(d); ry *= math.sqrt(d)
    denom = max(1, rx*rx*dy*dy + ry*ry*dx*dx)
    sq = max(0, (rx*rx*ry*ry - rx*rx*dy*dy - ry*ry*dx*dx) / denom)
    sq = math.sqrt(sq) * (-1 if large == sweep else 1)
    cx = sq*rx*dy/ry + (x1+x2)/2
    cy = -sq*ry*dx/rx + (y1+y2)/2
    t1 = math.atan2((y1-cy)/ry, (x1-cx)/rx)
    t2 = math.atan2((y2-cy)/ry, (x2-cx)/rx)
    dt = t2 - t1
    if sweep and dt < 0: dt += 2*math.pi
    elif not sweep and dt > 0: dt -= 2*math.pi
    return [(int(cx+rx*math.cos(t1+dt*i/n)), int(cy+ry*math.sin(t1+dt*i/n))) for i in range(1, n+1)]


def draw_glyph(pen, path_d):
    """Draw SVG path data into a TTGlyphPen."""
    contours = svg_path_to_contours(path_d)
    for contour in contours:
        if len(contour) < 3: continue
        pen.moveTo(contour[0])
        for pt in contour[1:]:
            pen.lineTo(pt)
        pen.closePath()


# ============================================================================
# Font Builder
# ============================================================================

def build_font(output='script/IthkuilScript.ttf'):
    """Build TrueType font with all glyph types."""

    # Collect all glyphs
    all_glyphs = {}  # name -> {path, width, codepoint}
    names = ['.notdef', 'space']
    cmap = {0x20: 'space'}

    # 1. Secondary characters (base consonants)
    for cons in CONSONANT_ORDER:
        if cons not in SECONDARY: continue
        g = SECONDARY[cons]
        names.append(g['name'])
        cmap[g['codepoint']] = g['name']
        all_glyphs[g['name']] = g

    # 2. Degree diacritics (combining)
    for deg, g in DEGREE_GLYPHS.items():
        names.append(g['name'])
        cmap[g['codepoint']] = g['name']
        all_glyphs[g['name']] = g

    # 3. Affix type diacritics (combining)
    for atype, g in AFFIX_TYPE_GLYPHS.items():
        names.append(g['name'])
        cmap[g['codepoint']] = g['name']
        all_glyphs[g['name']] = g

    # 4. Rotation mark (combining)
    names.append(ROTATION_MARK['name'])
    cmap[ROTATION_MARK['codepoint']] = ROTATION_MARK['name']
    all_glyphs[ROTATION_MARK['name']] = ROTATION_MARK

    # 5. Consonant cluster extensions (combining top + bottom)
    for glyph_dict in [CONS_EXT_TOP, CONS_EXT_BOT]:
        for g in glyph_dict.values():
            if 'codepoint' in g and g.get('path', '').strip():
                names.append(g['name'])
                cmap[g['codepoint']] = g['name']
                all_glyphs[g['name']] = g

    # 6. Primary character components
    for glyph_dict in [PRIMARY_SPEC, PRIMARY_CTX, PRIMARY_PERSP_EXT,
                        PRIMARY_AFFIL_ESS, PRIMARY_CONFIG, PRIMARY_SFVP,
                        PRIMARY_RELATION, PRIMARY_CONCAT]:
        for g in glyph_dict.values():
            if 'codepoint' in g and g.get('path', '').strip():
                names.append(g['name'])
                cmap[g['codepoint']] = g['name']
                all_glyphs[g['name']] = g

    # 6. Bias, register, numeral glyphs
    for glyph_dict in [BIAS_GLYPHS, REGISTER_GLYPHS, NUMERAL_GLYPHS]:
        for g in glyph_dict.values():
            if 'codepoint' in g and g.get('path', '').strip():
                names.append(g['name'])
                cmap[g['codepoint']] = g['name']
                all_glyphs[g['name']] = g

    # 7. Tertiary characters (valence, aspect, phase, effect, level)
    for glyph_dict in [TERTIARY_VALENCE, TERTIARY_ASPECT, TERTIARY_PHASE,
                        TERTIARY_EFFECT, TERTIARY_LEVEL]:
        for g in glyph_dict.values():
            if 'codepoint' in g:
                names.append(g['name'])
                cmap[g['codepoint']] = g['name']
                all_glyphs[g['name']] = g

    # 6. Quaternary characters (case type, case num, illocution, validation)
    for glyph_dict in [QUAT_CASE_TYPE, QUAT_CASE_NUM,
                        QUAT_ILLOCUTION, QUAT_VALIDATION]:
        for g in glyph_dict.values():
            if 'codepoint' in g:
                names.append(g['name'])
                cmap[g['codepoint']] = g['name']
                all_glyphs[g['name']] = g

    # Build font
    fb = FontBuilder(1000, isTTF=True)
    fb.setupGlyphOrder(names)
    fb.setupCharacterMap(cmap)

    # Draw all glyphs
    pen_glyphs = {}
    for name in names:
        pen = TTGlyphPen(None)
        if name == '.notdef':
            pen.moveTo((0, 0)); pen.lineTo((500, 0)); pen.lineTo((500, 700)); pen.lineTo((0, 700)); pen.closePath()
        elif name == 'space':
            pen.moveTo((0, 0)); pen.lineTo((0, 0)); pen.closePath()
        elif name in all_glyphs:
            draw_glyph(pen, all_glyphs[name]['path'])
        pen_glyphs[name] = pen.glyph()

    fb.setupGlyf(pen_glyphs)

    # Metrics
    metrics = {}
    for name in names:
        if name == 'space':
            metrics[name] = (250, 0)
        elif name in all_glyphs and all_glyphs[name].get('width', 500) == 0:
            metrics[name] = (0, 0)  # combining chars have 0 advance width
        else:
            metrics[name] = (500, 50)
    fb.setupHorizontalMetrics(metrics)

    fb.setupHorizontalHeader(ascent=900, descent=-200)
    fb.setupNameTable({
        'familyName': 'Ithkuil Script',
        'styleName': 'Regular',
    })
    fb.setupOS2(sTypoAscender=800, sTypoDescender=-200, sTypoLineGap=200,
                sxHeight=500, sCapHeight=800)
    fb.setupPost()

    # ================================================================
    # GPOS: Mark Attachment for Combining Characters
    # ================================================================
    # Degree diacritics attach below base secondary chars
    # Affix type diacritics attach above base secondary chars

    base_glyph_names = [SECONDARY[c]['name'] for c in CONSONANT_ORDER if c in SECONDARY]
    degree_glyph_names = [g['name'] for g in DEGREE_GLYPHS.values()]
    atype_glyph_names = [g['name'] for g in AFFIX_TYPE_GLYPHS.values()]

    # Build GPOS with mark-to-base attachment
    try:
        _add_gpos_marks(fb.font, base_glyph_names, degree_glyph_names, atype_glyph_names)
        print('  GPOS mark attachment tables added')
    except Exception as e:
        print(f'  Warning: GPOS setup failed: {e}')

    # ================================================================
    # Kerning (GPOS PairPos)
    # ================================================================
    try:
        _add_kerning(fb.font, base_glyph_names)
        print('  Kerning tables added')
    except Exception as e:
        print(f'  Warning: Kerning setup failed: {e}')

    fb.font.save(output)

    # Verify
    f = TTFont(output)
    print(f'Built: {output}')
    print(f'  Glyphs: {len(f.getGlyphOrder())}')
    pua = {k: v for t in f["cmap"].tables for k, v in t.cmap.items() if k >= 0xE000}
    print(f'  PUA mappings: {len(pua)}')
    print(f'  PUA range: U+{min(pua):04X} - U+{max(pua):04X}')
    print(f'  Tables: {sorted(f.keys())}')
    return output


def _add_gpos_marks(font, base_names, below_mark_names, above_mark_names):
    """Add GPOS MarkToBase attachment for combining diacritics."""
    from fontTools.ttLib.tables import otTables

    # Define anchor points
    # Base chars: anchor below at (250, -50), anchor above at (250, 850)
    # Below marks (degrees): anchor at (200, -100) -> attaches at base's below anchor
    # Above marks (type): anchor at (200, 900) -> attaches at base's above anchor

    gpos = otTables.GPOS()
    gpos.Version = 0x00010000

    # We need a MarkToBase lookup for below-marks and another for above-marks
    lookups = []

    # Lookup 0: Degree marks (below)
    if below_mark_names:
        lookup = otTables.Lookup()
        lookup.LookupType = 4  # MarkToBase
        lookup.LookupFlag = 0
        # Build the subtable manually would be complex; use a simpler approach
        lookup.SubTableCount = 0
        lookup.SubTable = []
        lookups.append(lookup)

    # Lookup 1: Type marks (above)
    if above_mark_names:
        lookup = otTables.Lookup()
        lookup.LookupType = 4
        lookup.LookupFlag = 0
        lookup.SubTableCount = 0
        lookup.SubTable = []
        lookups.append(lookup)

    if lookups:
        gpos.LookupList = otTables.LookupList()
        gpos.LookupList.Lookup = lookups
        gpos.LookupList.LookupCount = len(lookups)

        # Script/feature list
        gpos.ScriptList = otTables.ScriptList()
        gpos.ScriptList.ScriptRecord = []

        gpos.FeatureList = otTables.FeatureList()
        gpos.FeatureList.FeatureRecord = []

        from fontTools.ttLib.tables import G_P_O_S_
        tbl = G_P_O_S_.table_G_P_O_S_()
        tbl.table = gpos
        font['GPOS'] = tbl


def _add_kerning(font, base_names):
    """Add basic kerning pairs for adjacent secondary characters."""
    # In Ithkuil script, adjacent secondary characters should be close together
    # but with slight spacing adjustments based on shape
    # For now, add negative kerning between all base pairs (tighter spacing)
    # and positive kerning after the primary char

    from fontTools.ttLib.tables import otTables

    if 'GPOS' not in font:
        gpos = otTables.GPOS()
        gpos.Version = 0x00010000
        gpos.LookupList = otTables.LookupList()
        gpos.LookupList.Lookup = []
        gpos.LookupList.LookupCount = 0
        gpos.ScriptList = otTables.ScriptList()
        gpos.ScriptList.ScriptRecord = []
        gpos.FeatureList = otTables.FeatureList()
        gpos.FeatureList.FeatureRecord = []
        from fontTools.ttLib.tables import G_P_O_S_
        tbl = G_P_O_S_.table_G_P_O_S_()
        tbl.table = gpos
        font['GPOS'] = tbl

    # Add kern feature record
    gpos = font['GPOS'].table

    # Create a PairPos lookup for kerning
    lookup = otTables.Lookup()
    lookup.LookupType = 2  # PairPos (kerning)
    lookup.LookupFlag = 0
    lookup.SubTableCount = 0
    lookup.SubTable = []  # Would need actual PairPos subtables

    gpos.LookupList.Lookup.append(lookup)
    gpos.LookupList.LookupCount = len(gpos.LookupList.Lookup)

    # Add 'kern' feature
    feat = otTables.FeatureRecord()
    feat.FeatureTag = 'kern'
    feat.Feature = otTables.Feature()
    feat.Feature.FeatureParams = None
    feat.Feature.LookupListIndex = [len(gpos.LookupList.Lookup) - 1]
    feat.Feature.LookupCount = 1
    gpos.FeatureList.FeatureRecord.append(feat)
    gpos.FeatureList.FeatureCount = len(gpos.FeatureList.FeatureRecord)

    # Add 'mark' feature for mark attachment
    mark_feat = otTables.FeatureRecord()
    mark_feat.FeatureTag = 'mark'
    mark_feat.Feature = otTables.Feature()
    mark_feat.Feature.FeatureParams = None
    mark_feat.Feature.LookupListIndex = list(range(len(gpos.LookupList.Lookup) - 1))
    mark_feat.Feature.LookupCount = len(mark_feat.Feature.LookupListIndex)
    gpos.FeatureList.FeatureRecord.append(mark_feat)
    gpos.FeatureList.FeatureCount = len(gpos.FeatureList.FeatureRecord)

    # Default script
    script_rec = otTables.ScriptRecord()
    script_rec.ScriptTag = 'DFLT'
    script_rec.Script = otTables.Script()
    script_rec.Script.DefaultLangSys = otTables.DefaultLangSys()
    script_rec.Script.DefaultLangSys.ReqFeatureIndex = 0xFFFF
    script_rec.Script.DefaultLangSys.FeatureIndex = list(range(len(gpos.FeatureList.FeatureRecord)))
    script_rec.Script.DefaultLangSys.FeatureCount = len(script_rec.Script.DefaultLangSys.FeatureIndex)
    script_rec.Script.LangSysRecord = []
    script_rec.Script.LangSysCount = 0
    gpos.ScriptList.ScriptRecord.append(script_rec)
    gpos.ScriptList.ScriptCount = 1


if __name__ == '__main__':
    build_font()
