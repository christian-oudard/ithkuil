#!/usr/bin/env python3
"""
Stroke-based drawing library for Ithkuil script.

Draws filled outlines from pen strokes, supporting:
- Variable width along a stroke
- Curved strokes (arcs)
- End caps: flat, pointed, chisel (angled slant)
- Proper miter/bevel joins at stroke junctions

Output is SVG path data (filled contours), suitable for both
SVG rendering and font glyph construction.

Inspired by canoepaddle (christian-oudard/canoepaddle).
"""
import math
from dataclasses import dataclass, field


@dataclass
class Point:
    x: float
    y: float

    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y)

    def __sub__(self, other):
        return Point(self.x - other.x, self.y - other.y)

    def __mul__(self, s):
        return Point(self.x * s, self.y * s)

    def __rmul__(self, s):
        return Point(self.x * s, self.y * s)

    def length(self):
        return math.sqrt(self.x * self.x + self.y * self.y)

    def normalized(self):
        ln = self.length()
        if ln < 1e-9:
            return Point(0, 0)
        return Point(self.x / ln, self.y / ln)

    def rotated(self, angle_deg):
        a = math.radians(angle_deg)
        c, s = math.cos(a), math.sin(a)
        return Point(self.x * c - self.y * s, self.x * s + self.y * c)

    def normal(self):
        """90-degree counterclockwise rotation (left normal)."""
        return Point(-self.y, self.x)

    def tuple(self):
        return (self.x, self.y)


# End cap styles
CAP_FLAT = 'flat'
CAP_POINTED = 'pointed'
CAP_CHISEL = 'chisel'


@dataclass
class Stroke:
    """A single stroke segment with centerline, width, and end caps."""
    points: list       # list of Point (centerline)
    widths: list       # width at each point (same length as points)
    start_cap: str = CAP_FLAT
    end_cap: str = CAP_FLAT
    start_slant: float = 0  # slant angle in degrees for chisel cap
    end_slant: float = 0


class Pen:
    """Stroke-based drawing pen.

    Usage:
        pen = Pen(width=50)
        pen.move_to(100, 800)
        pen.line_to(100, 400)
        pen.arc_to(250, 200, radius=150)
        pen.set_width(30)
        pen.line_to(400, 200)

        path_d = pen.to_path()  # SVG path data
    """

    def __init__(self, width=50):
        self.x = 0.0
        self.y = 0.0
        self.width = float(width)
        self.strokes = []
        self._current_points = []
        self._current_widths = []
        self._start_cap = CAP_FLAT
        self._end_cap = CAP_FLAT
        self._start_slant = 0.0
        self._end_slant = 0.0

    def move_to(self, x, y):
        """Move pen without drawing. Finalizes any current stroke."""
        self._finish_stroke()
        self.x = float(x)
        self.y = float(y)

    def line_to(self, x, y, end_width=None):
        """Draw a straight line to (x, y).

        If end_width is given, the stroke tapers from current width to end_width.
        """
        if not self._current_points:
            self._current_points.append(Point(self.x, self.y))
            self._current_widths.append(self.width)
        self.x = float(x)
        self.y = float(y)
        self._current_points.append(Point(self.x, self.y))
        if end_width is not None:
            self.width = float(end_width)
        self._current_widths.append(self.width)

    def arc_to(self, x, y, radius=None, clockwise=False, n_segments=12):
        """Draw a circular arc to (x, y).

        If radius is None, it's computed from the distance.
        The arc is approximated as line segments.
        """
        start = Point(self.x, self.y)
        end = Point(float(x), float(y))
        chord = end - start
        chord_len = chord.length()
        if chord_len < 1e-9:
            return

        if radius is None:
            radius = chord_len  # default: semicircle-ish

        radius = max(abs(radius), chord_len / 2)  # must be >= half chord

        # Find arc center
        mid = 0.5 * (start + end)
        chord_dir = chord.normalized()
        # Normal to chord
        if clockwise:
            perp = Point(chord_dir.y, -chord_dir.x)
        else:
            perp = Point(-chord_dir.y, chord_dir.x)

        half_chord = chord_len / 2
        h = math.sqrt(max(0, radius * radius - half_chord * half_chord))
        center = mid + h * perp

        # Angles
        a1 = math.atan2(start.y - center.y, start.x - center.x)
        a2 = math.atan2(end.y - center.y, end.x - center.x)

        # Ensure correct direction
        da = a2 - a1
        if clockwise:
            if da > 0:
                da -= 2 * math.pi
        else:
            if da < 0:
                da += 2 * math.pi

        # Generate arc points
        if not self._current_points:
            self._current_points.append(start)
            self._current_widths.append(self.width)

        for i in range(1, n_segments + 1):
            t = i / n_segments
            angle = a1 + da * t
            px = center.x + radius * math.cos(angle)
            py = center.y + radius * math.sin(angle)
            self._current_points.append(Point(px, py))
            self._current_widths.append(self.width)

        self.x = end.x
        self.y = end.y

    def set_width(self, width):
        """Change the pen width for subsequent strokes."""
        self.width = float(width)

    def set_start_cap(self, cap, slant=0):
        """Set the start cap style for the next stroke."""
        self._start_cap = cap
        self._start_slant = float(slant)

    def set_end_cap(self, cap, slant=0):
        """Set the end cap style for the next stroke."""
        self._end_cap = cap
        self._end_slant = float(slant)

    def set_caps(self, cap, slant=0):
        """Set both start and end cap styles."""
        self.set_start_cap(cap, slant)
        self.set_end_cap(cap, slant)

    def finish(self):
        """Finalize the current stroke."""
        self._finish_stroke()

    def _finish_stroke(self):
        if len(self._current_points) < 2:
            self._current_points = []
            self._current_widths = []
            return
        self.strokes.append(Stroke(
            points=self._current_points,
            widths=self._current_widths,
            start_cap=self._start_cap,
            end_cap=self._end_cap,
            start_slant=self._start_slant,
            end_slant=self._end_slant,
        ))
        self._current_points = []
        self._current_widths = []
        self._start_cap = CAP_FLAT
        self._end_cap = CAP_FLAT
        self._start_slant = 0.0
        self._end_slant = 0.0

    def to_outlines(self):
        """Convert all strokes to outline polygons.

        Returns list of (left_edge, right_edge) point lists.
        Each outline is a closed contour suitable for SVG path or font glyph.
        """
        self._finish_stroke()
        outlines = []
        for stroke in self.strokes:
            outline = _stroke_to_outline(stroke)
            if outline:
                outlines.append(outline)
        return outlines

    def to_path(self):
        """Convert all strokes to a single SVG path data string."""
        outlines = self.to_outlines()
        parts = []
        for contour in outlines:
            if len(contour) < 3:
                continue
            parts.append(f'M{contour[0].x:.0f},{contour[0].y:.0f}')
            for pt in contour[1:]:
                parts.append(f' L{pt.x:.0f},{pt.y:.0f}')
            parts.append(' Z')
        return ''.join(parts)

    def to_svg_group(self, fill='black'):
        """Render as an SVG group with filled paths."""
        path_d = self.to_path()
        if not path_d:
            return ''
        return f'<path d="{path_d}" fill="{fill}" fill-rule="nonzero"/>'


def _stroke_to_outline(stroke):
    """Convert a stroke to a closed outline contour.

    Computes left and right edges offset from the centerline by width/2,
    with proper miter joins and end caps.
    """
    pts = stroke.points
    widths = stroke.widths
    n = len(pts)
    if n < 2:
        return []

    left_edge = []
    right_edge = []

    for i in range(n):
        hw = widths[i] / 2  # half width

        # Compute direction at this point
        if i == 0:
            d = (pts[1] - pts[0]).normalized()
        elif i == n - 1:
            d = (pts[n-1] - pts[n-2]).normalized()
        else:
            # Average of incoming and outgoing directions
            d_in = (pts[i] - pts[i-1]).normalized()
            d_out = (pts[i+1] - pts[i]).normalized()
            d = (d_in + d_out).normalized()
            if d.length() < 1e-9:
                d = d_in

            # Miter factor: adjust offset to maintain width at the join
            cos_half = max(0.1, d_in.x * d.x + d_in.y * d.y)
            hw = hw / cos_half
            # Limit miter to prevent spikes
            hw = min(hw, widths[i] * 1.5)

        normal = d.normal()
        left_edge.append(pts[i] + hw * normal)
        right_edge.append(pts[i] - hw * normal)

    # Apply end caps
    _apply_start_cap(left_edge, right_edge, pts, widths, stroke)
    _apply_end_cap(left_edge, right_edge, pts, widths, stroke)

    # Build closed contour: left edge forward, right edge backward
    contour = left_edge + list(reversed(right_edge))
    return contour


def _apply_start_cap(left, right, pts, widths, stroke):
    """Modify the start of the outline for the start cap style."""
    if stroke.start_cap == CAP_POINTED:
        # Replace first left/right points with a single center point
        center = pts[0]
        left[0] = center
        right[0] = center
    elif stroke.start_cap == CAP_CHISEL:
        # Angled cut: shift left and right points along the stroke direction
        d = (pts[1] - pts[0]).normalized()
        hw = widths[0] / 2
        slant_offset = hw * math.tan(math.radians(stroke.start_slant))
        left[0] = left[0] + slant_offset * d
        right[0] = right[0] - slant_offset * d


def _apply_end_cap(left, right, pts, widths, stroke):
    """Modify the end of the outline for the end cap style."""
    n = len(pts) - 1
    if stroke.end_cap == CAP_POINTED:
        center = pts[n]
        left[n] = center
        right[n] = center
    elif stroke.end_cap == CAP_CHISEL:
        d = (pts[n] - pts[n-1]).normalized()
        hw = widths[n] / 2
        slant_offset = hw * math.tan(math.radians(stroke.end_slant))
        left[n] = left[n] + slant_offset * d
        right[n] = right[n] - slant_offset * d


# ============================================================================
# Convenience functions
# ============================================================================

def draw_glyph(draw_fn, width=50, em=1000):
    """Helper to draw a glyph using a function that takes a Pen.

    Returns SVG path data string.
    """
    pen = Pen(width=width)
    draw_fn(pen)
    return pen.to_path()


def mirror_x(path_d, cx=250):
    """Mirror SVG path data horizontally around cx."""
    # Parse and transform x coordinates
    import re
    def flip(m):
        cmd = m.group(1)
        x = float(m.group(2))
        y = m.group(3)
        new_x = 2 * cx - x
        return f'{cmd}{new_x:.0f},{y}'
    return re.sub(r'([ML])\s*(-?\d+(?:\.\d+)?),(-?\d+(?:\.\d+)?)', flip, path_d)


def mirror_y(path_d, cy=500):
    """Mirror SVG path data vertically around cy."""
    import re
    def flip(m):
        cmd = m.group(1)
        x = m.group(2)
        y = float(m.group(3))
        new_y = 2 * cy - y
        return f'{cmd}{x},{new_y:.0f}'
    return re.sub(r'([ML])\s*(-?\d+(?:\.\d+)?),(-?\d+(?:\.\d+)?)', flip, path_d)


# ============================================================================
# Test / Demo
# ============================================================================

def demo():
    """Draw sample characters to test the pen library."""
    page_w, page_h = 800, 500
    svg = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{page_w}" height="{page_h}" '
        f'viewBox="0 0 {page_w} {page_h}">',
        '<rect width="100%" height="100%" fill="white"/>',
        '<text x="20" y="25" font-size="14" font-family="sans-serif" fill="#333">'
        'Pen Library Demo</text>',
    ]

    scale = 0.09
    y_base = 40

    # Character: p (Gamma with J-foot)
    def draw_p(pen):
        pen.set_width(50)
        pen.move_to(100, 800)
        pen.line_to(350, 800)        # top horizontal
        pen.move_to(100, 800)
        pen.line_to(100, 350)        # vertical down
        pen.set_end_cap(CAP_POINTED)
        pen.arc_to(350, 350, radius=150, clockwise=True)  # bottom curve

    # Character: m (single diagonal)
    def draw_m(pen):
        pen.set_width(50)
        pen.set_start_cap(CAP_CHISEL, 30)
        pen.set_end_cap(CAP_CHISEL, -30)
        pen.move_to(100, 800)
        pen.line_to(350, 200)

    # Character: s (zigzag)
    def draw_s(pen):
        pen.set_width(50)
        pen.move_to(100, 800)
        pen.line_to(350, 550)
        pen.line_to(100, 300)

    # Character: ţ (C-curve)
    def draw_tc(pen):
        pen.set_width(50)
        pen.set_start_cap(CAP_POINTED)
        pen.set_end_cap(CAP_POINTED)
        pen.move_to(300, 800)
        pen.arc_to(300, 200, radius=250, clockwise=False)

    # Varying width demo
    def draw_taper(pen):
        pen.set_width(60)
        pen.move_to(100, 800)
        pen.line_to(250, 500, end_width=20)
        pen.set_end_cap(CAP_POINTED)
        pen.line_to(400, 200, end_width=5)

    demos = [
        ('p', draw_p),
        ('m (chisel)', draw_m),
        ('s', draw_s),
        ('ţ (curve)', draw_tc),
        ('taper', draw_taper),
    ]

    for i, (label, fn) in enumerate(demos):
        x_off = 30 + i * 150
        pen = Pen(width=50)
        fn(pen)
        path_d = pen.to_path()
        transform = f'translate({x_off},{y_base + 80}) scale({scale},{-scale})'
        svg.append(f'<g transform="{transform}">'
                   f'<path d="{path_d}" fill="black" fill-rule="nonzero"/></g>')
        svg.append(f'<text x="{x_off + 20}" y="{y_base + 95}" font-size="10" fill="#666">'
                   f'{label}</text>')

    # Show mirrored p -> b
    pen = Pen(width=50)
    draw_p(pen)
    p_path = pen.to_path()
    b_path = mirror_x(p_path, cx=250)
    x_off = 30 + len(demos) * 150
    transform = f'translate({x_off},{y_base + 80}) scale({scale},{-scale})'
    svg.append(f'<g transform="{transform}">'
               f'<path d="{b_path}" fill="black" fill-rule="nonzero"/></g>')
    svg.append(f'<text x="{x_off + 20}" y="{y_base + 95}" font-size="10" fill="#666">'
               f'b (mirror p)</text>')

    svg.append('</svg>')

    output = 'script/pen_demo.svg'
    with open(output, 'w') as f:
        f.write('\n'.join(svg))
    print(f'Wrote {output}')


if __name__ == '__main__':
    demo()
