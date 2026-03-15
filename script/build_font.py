#!/usr/bin/env python3
"""Build an OpenType font for Ithkuil V4 Script using fonttools."""
import sys, os, math
sys.path.insert(0, os.path.dirname(__file__))

from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib import TTFont

from glyphs import SECONDARY, CONSONANT_ORDER, PUA_SECONDARY


def svg_path_to_contours(path_d):
    """Parse SVG path 'd' attribute into contour point lists (y-up coords)."""
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
        if c.isalpha():
            tokens.append(c); i += 1
        elif c in '0123456789.-':
            j = i
            if c == '-': j += 1
            while j < len(d) and d[j] in '0123456789.': j += 1
            tokens.append(float(d[i:j])); i = j
        else:
            i += 1
    return tokens


def _arc_points(x1, y1, x2, y2, rx, ry, large, sweep, n=16):
    dx, dy = (x1-x2)/2, (y1-y2)/2
    d = dx*dx/(rx*rx) + dy*dy/(ry*ry)
    if d > 1: rx *= math.sqrt(d); ry *= math.sqrt(d)
    sq = max(0, (rx*rx*ry*ry - rx*rx*dy*dy - ry*ry*dx*dx) / max(1, rx*rx*dy*dy + ry*ry*dx*dx))
    sq = math.sqrt(sq) * (-1 if large == sweep else 1)
    cx = sq*rx*dy/ry + (x1+x2)/2
    cy = -sq*ry*dx/rx + (y1+y2)/2
    t1 = math.atan2((y1-cy)/ry, (x1-cx)/rx)
    t2 = math.atan2((y2-cy)/ry, (x2-cx)/rx)
    dt = t2 - t1
    if sweep and dt < 0: dt += 2*math.pi
    elif not sweep and dt > 0: dt -= 2*math.pi
    return [(int(cx+rx*math.cos(t1+dt*i/n)), int(cy+ry*math.sin(t1+dt*i/n))) for i in range(1, n+1)]


def build_font(output='script/IthkuilScript.ttf'):
    """Build TrueType font with secondary character glyphs."""
    # Collect glyph data
    names = ['.notdef', 'space']
    cmap = {0x20: 'space'}
    glyphs = {}

    for cons in CONSONANT_ORDER:
        if cons not in SECONDARY: continue
        g = SECONDARY[cons]
        names.append(g['name'])
        if 'codepoint' in g:
            cmap[g['codepoint']] = g['name']
        glyphs[g['name']] = g

    # Build with FontBuilder
    fb = FontBuilder(1000, isTTF=True)
    fb.setupGlyphOrder(names)
    fb.setupCharacterMap(cmap)

    # Draw glyphs using TTGlyphPen
    pen_glyphs = {}
    for name in names:
        pen = TTGlyphPen(None)
        if name in ('.notdef', 'space'):
            pen.moveTo((0, 0))
            pen.lineTo((500, 0))
            pen.lineTo((500, 700))
            pen.lineTo((0, 700))
            pen.closePath()
            if name == 'space':
                pen = TTGlyphPen(None)  # empty glyph for space
                # Just an empty path
                pen.moveTo((0, 0))
                pen.lineTo((0, 0))
                pen.closePath()
        elif name in glyphs:
            contours = svg_path_to_contours(glyphs[name]['path'])
            for contour in contours:
                if len(contour) < 3: continue
                pen.moveTo(contour[0])
                for pt in contour[1:]:
                    pen.lineTo(pt)
                pen.closePath()
        pen_glyphs[name] = pen.glyph()

    fb.setupGlyf(pen_glyphs)

    # Metrics: all glyphs 500 units wide
    metrics = {}
    for name in names:
        if name == 'space':
            metrics[name] = (250, 0)
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

    fb.font.save(output)

    # Verify
    f = TTFont(output)
    print(f'Built: {output}')
    print(f'  Glyphs: {len(f.getGlyphOrder())}')
    print(f'  Cmap: {sum(len(t.cmap) for t in f["cmap"].tables)} entries')
    print(f'  PUA range: U+{min(cmap):04X} - U+{max(cmap):04X}')
    return output


if __name__ == '__main__':
    build_font()
