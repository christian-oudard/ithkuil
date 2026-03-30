#!/usr/bin/env python3
"""
Render a V3 consonant chart as SVG.

Usage: python3 -m v3.demo [output.svg]

Produces a grid showing all 46 V3 consonants, each with:
- Normal side ending (no vowel marker)
- Normal bottom ending
"""
import sys
import os
import math
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))

from pen import Paper
from v3.consonant import consonants
from v3.side_ending import Normal as SNormal
from v3.bottom_ending import Normal as BNormal


def render_chart(cols=8, width=0.5):
    rows = math.ceil(len(consonants) / cols)

    # Each character occupies a cell in the grid.
    # Characters are in Y-up coords: height ~ [BOTTOM, TOP] = [0, 7] * CELL_H
    CELL_W = 12.0
    CELL_H = 16.0
    MARGIN = 2.0
    LABEL_H = 2.5   # extra space below for label text

    svg_w = MARGIN * 2 + cols * CELL_W
    svg_h = MARGIN * 2 + rows * (CELL_H + LABEL_H)

    parts = [
        f'<svg xmlns="http://www.w3.org/2000/svg"'
        f' width="{svg_w * 18:.0f}" height="{svg_h * 18:.0f}"'
        f' viewBox="0 0 {svg_w} {svg_h}">',
        '<rect width="100%" height="100%" fill="#f9f9f2"/>',
    ]

    for i, cc in enumerate(consonants):
        col = i % cols
        row = i // cols
        cx = MARGIN + col * CELL_W + CELL_W / 2   # cell center x (SVG)
        top_y = MARGIN + row * (CELL_H + LABEL_H)  # cell top y (SVG down)
        label_y = top_y + CELL_H + LABEL_H - 0.5

        char = cc(SNormal, BNormal)
        paper = char.draw_character(width=width)
        paper.finish()

        # Collect path data in Y-up coords (no flip), then apply transform
        # Transform: translate to cell center, flip Y, scale to fit cell
        # Characters occupy roughly x in [-4,4], y in [-2, 7]
        # Scale to fit in CELL_W × CELL_H with a small margin
        scale = min(CELL_W * 0.8 / 8.0, CELL_H * 0.9 / 9.0)
        # pen paper has y-up coords. SVG transform: translate(cx, top_y + CELL_H*0.5) scale(scale, -scale)
        # This centers the char in the cell and flips Y.
        char_y_center = top_y + CELL_H * 0.55  # slightly above cell center
        transform = f'translate({cx:.3f},{char_y_center:.3f}) scale({scale:.4f},{-scale:.4f})'

        for path in paper._paths:
            for d in path.to_svg_paths(precision=2, flip_y=False):
                if d:
                    parts.append(f'<path d="{d}" transform="{transform}" fill="#1a1a2e"/>')
        for cf in paper._closed:
            d = cf.to_svg_path(2, flip_y=False)
            if d:
                parts.append(f'<path d="{d}" transform="{transform}" fill="#1a1a2e"/>')

        # Label
        label = cc.pronunciation if hasattr(cc, 'pronunciation') else '?'
        parts.append(
            f'<text x="{cx:.2f}" y="{label_y:.2f}" text-anchor="middle"'
            f' font-size="1.6" font-family="serif" fill="#555">{label}</text>'
        )

    parts.append('</svg>')
    return '\n'.join(parts)


if __name__ == '__main__':
    outfile = sys.argv[1] if len(sys.argv) > 1 else 'script/v3_chart.svg'
    svg = render_chart()
    with open(outfile, 'w') as f:
        f.write(svg)
    print(f'Wrote {outfile}', file=sys.stderr)
